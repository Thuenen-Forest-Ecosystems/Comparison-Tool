library(jsonlite)
library(tidyverse)
library(openxlsx)

has_data <- function(df) {
  !is.null(df) && nrow(df) > 0
}

safe_block <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

json_historie <- fromJSON(
  "",
  simplifyVector = FALSE
)

json_aktuell <- fromJSON(
  "",
  simplifyVector = FALSE
)

flatten_json <- function(x, parent = "") {
  
  if (length(x) == 0) {
    return(
      tibble(
        Field = parent,
        Wert = NA_character_
      )
    )
  }
  
  if (!is.list(x)) {
    return(
      tibble(
        Field = parent,
        Wert = as.character(x)
      )
    )
  }
  
  result <- purrr::map_dfr(seq_along(x), function(i) {
    
    nm <- names(x)[i]
    
    if (is.null(nm) || nm == "") {
      new_parent <- paste0(parent, "[", i - 1, "]")
    } else if (parent == "") {
      new_parent <- nm
    } else {
      new_parent <- paste0(parent, ".", nm)
    }
    
    flatten_json(x[[i]], new_parent)
  })
  
  result
}

aktuell_tbl <- flatten_json(json_aktuell)
historie_tbl <- flatten_json(json_historie)

Vergleich_zwei <- full_join(
  aktuell_tbl,
  historie_tbl,
  by = "Field"
) %>%
  rename(
    Aktuell = Wert.x,
    Historie = Wert.y
  )

Vergleich_zwei <- Vergleich_zwei %>%
  mutate(
    Aktuell_num = suppressWarnings(as.numeric(Aktuell)),
    Historie_num = suppressWarnings(as.numeric(Historie))
  )


# WZP4 Vergleich ----------------------------------------------------------

wzp4_export <- safe_block({
  
  tree <- Vergleich_zwei %>%
    filter(grepl("^tree\\[.*\\]\\.(dbh|tree_height|tree_number|distance)$", Field)) %>%
    mutate(
      Baum_ID = sub("^tree\\[(.*)\\]\\..*$", "\\1", Field),
      Typ     = sub(".*\\]\\.(.*)$", "\\1", Field)
    ) %>%
    mutate(
      Differenz = abs(coalesce(Aktuell_num, 0) - coalesce(Historie_num, 0))
    )
  
  if (nrow(tree) == 0) return(NULL)
  
  baumnummern <- tree %>%
    filter(Typ == "tree_number") %>%
    transmute(
      Baum_ID,
      Baumnummer = Aktuell_num
    )
  
  dbh_grenzen <- tree %>%
    filter(Typ == "dbh") %>%
    transmute(
      Baum_ID,
      Distance_Grenze = Aktuell_num / 2 / 10
    )
  
  tree <- tree %>%
    left_join(dbh_grenzen, by = "Baum_ID") %>%
    left_join(baumnummern, by = "Baum_ID") %>%
    mutate(
      OK = case_when(
        Typ == "dbh" ~ Differenz < 3,
        
        Typ == "tree_height" ~ Differenz < 20,
        
        Typ == "tree_number" ~
          coalesce(Aktuell_num, 0) == coalesce(Historie_num, 0),
        
        Typ == "distance" ~
          Differenz <= coalesce(Distance_Grenze, 0),
        
        TRUE ~ NA
      )
    )
  
  ergebnis <- tree %>%
    filter(OK == FALSE | is.na(OK)) %>%
    select(
      Baum_ID,
      Baumnummer,
      Field,
      Typ,
      Aktuell_num,
      Historie_num,
      Differenz,
      Distance_Grenze,
      OK
    )
  
  if (nrow(ergebnis) == 0) return(NULL)
  
  wzp4_export <- ergebnis %>%
    mutate(
      Maske = "WZP4",
      Wert_KT = paste0("Baum ", Baumnummer, " | ", Typ, " = ", Aktuell_num),
      Wert_AT = paste0("Baum ", Baumnummer, " | ", Typ, " = ", Historie_num),
      Unterschiede = Differenz,
      Bemerkungen = ifelse(
        Typ == "distance",
        paste0("Grenzwert: ", round(Distance_Grenze, 2)),
        ""
      )
    ) %>%
    select(
      Maske,
      Wert_KT,
      Wert_AT,
      Unterschiede,
      Bemerkungen
    )
  
wzp4_export$Maske[-1] <- ""
  wzp4_export
  
})


# Verjüngung Vergleich ----------------------------------------------------

verjuengung_export <- safe_block({
  
  regen <- Vergleich_zwei %>%
    filter(str_detect(Field, "^regeneration\\["))
  
  if (nrow(regen) == 0) return(NULL)
  
  regen <- regen %>%
    mutate(
      regeneration_id = str_extract(Field, "(?<=\\[)\\d+(?=\\])"),
      variable = sub("^.*\\]\\.", "", Field)
    )
  
  aktuell_tbl <- regen %>%
    select(regeneration_id, variable, value = Aktuell) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  historie_tbl <- regen %>%
    select(regeneration_id, variable, value = Historie) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  # Sicherheitscheck: wenn Struktur fehlt → abbrechen
  required_cols <- c("tree_size_class", "tree_species", "tree_count")
  
  if (!all(required_cols %in% names(aktuell_tbl)) &&
      !all(required_cols %in% names(historie_tbl))) {
    return(NULL)
  }
  
  vergleich <- full_join(
    aktuell_tbl %>%
      select(
        Größenklasse = tree_size_class,
        Baumart = tree_species,
        Anzahl_KT = tree_count
      ),
    historie_tbl %>%
      select(
        Größenklasse = tree_size_class,
        Baumart = tree_species,
        Anzahl_AT = tree_count
      ),
    by = c("Größenklasse", "Baumart")
  ) %>%
    mutate(
      Differenz = abs(
        coalesce(as.numeric(Anzahl_KT), 0) -
          coalesce(as.numeric(Anzahl_AT), 0)
      )
    )
  
  if (nrow(vergleich) == 0) return(NULL)
  
  verjuengung_export <- vergleich %>%
    mutate(
      Maske = "Verjüngung",
      Wert_KT = paste0(
        "GK:", Größenklasse,
        " | ", Baumart,
        " | Anzahl:", Anzahl_KT
      ),
      Wert_AT = paste0(
        "GK:", Größenklasse,
        " | ", Baumart,
        " | Anzahl:", Anzahl_AT
      ),
      Unterschiede = Differenz
    ) %>%
    select(Maske, Wert_KT, Wert_AT, Unterschiede)
  
  verjuengung_export$Maske[-1] <- ""
  verjuengung_export
})


# Totholz vergleich -------------------------------------------------------

safe_row <- function(x) {
  
  cleaned <- purrr::map(x, function(v) {
    
    if (is.null(v)) {
      return(NA)
    }
    
    if (length(v) == 0) {
      return(NA)
    }
    
    if (is.list(v)) {
      return(as.character(v[[1]]))
    }
    
    v
  })
  
  tibble::as_tibble_row(cleaned)
}

deadwood_export <- safe_block({

  if (is.null(json_historie$deadwood) &&
      is.null(json_aktuell$deadwood)) {
    return(NULL)
  }  
  # ---------------------------
  # Historie
  # ---------------------------
  
  if (is.null(json_historie$deadwood) ||
      length(json_historie$deadwood) == 0) {
    
    dw1 <- tibble()
    
  } else {
    
    dw1 <- map_dfr(json_historie$deadwood, safe_row) %>%
      mutate(piece_id = row_number())
  }
  
  # ---------------------------
  # Aktuell
  # ---------------------------
  
  if (is.null(json_aktuell$deadwood) ||
      length(json_aktuell$deadwood) == 0) {
    
    dw2 <- tibble()
    
  } else {
    
    dw2 <- map_dfr(json_aktuell$deadwood, safe_row) %>%
      mutate(piece_id = row_number())
  }
  
  # Wenn gar kein Totholz vorhanden
  
  if (nrow(dw1) == 0 && nrow(dw2) == 0) {
    return(NULL)
  }
  
  # ---------------------------
  # Vergleich
  # ---------------------------
  
  dw_compare <- full_join(
    dw1,
    dw2,
    by = "piece_id",
    suffix = c("_old", "_new")
  )
  
  # Fehlende Spalten absichern
  
  required <- c(
    "dead_wood_type_old",
    "dead_wood_type_new",
    "diameter_butt_old",
    "diameter_butt_new",
    "diameter_top_old",
    "diameter_top_new",
    "length_height_old",
    "length_height_new"
  )
  
  if (!all(required %in% names(dw_compare))) {
    return(NULL)
  }
  
  dw_compare <- dw_compare %>%
    mutate(
      diff_butt =
        coalesce(as.numeric(diameter_butt_new), 0) -
        coalesce(as.numeric(diameter_butt_old), 0),
      
      diff_top =
        coalesce(as.numeric(diameter_top_new), 0) -
        coalesce(as.numeric(diameter_top_old), 0),
      
      diff_length =
        coalesce(as.numeric(length_height_new), 0) -
        coalesce(as.numeric(length_height_old), 0),
      
      type_change =
        coalesce(as.character(dead_wood_type_old), "") !=
        coalesce(as.character(dead_wood_type_new), "")
    )
  
  deadwood_changes <- dw_compare %>%
    filter(
      type_change |
        diff_butt != 0 |
        diff_top != 0 |
        diff_length != 0
    )
  
  if (nrow(deadwood_changes) == 0) {
    return(NULL)
  }
  
  # ---------------------------
  # Export
  # ---------------------------
  
  deadwood_export <- deadwood_changes %>%
    mutate(
      Maske = "Totholz",
      
      Wert_KT = paste0(
        "Typ=", dead_wood_type_new,
        " | Butt=", diameter_butt_new,
        " | Top=", diameter_top_new,
        " | Länge=", length_height_new
      ),
      
      Wert_AT = paste0(
        "Typ=", dead_wood_type_old,
        " | Butt=", diameter_butt_old,
        " | Top=", diameter_top_old,
        " | Länge=", length_height_old
      ),
      
      Unterschiede = paste(
        "Butt:", diff_butt,
        "| Top:", diff_top,
        "| Länge:", diff_length
      ),
      
      Bemerkungen = ifelse(
        type_change,
        "Totholztyp geändert",
        ""
      )
    ) %>%
    select(
      Maske,
      Wert_KT,
      Wert_AT,
      Unterschiede,
      Bemerkungen
    )
  
  deadwood_export$Maske[-1] <- ""
  
  deadwood_export
})

# Bestockung kleiner 4m Vergleich ----------------------------------------------------


bestockung_export <- safe_block({
  
  bst <- Vergleich_zwei %>%
    filter(str_detect(Field, "^structure_lt4m\\["))
  
  if (nrow(bst) == 0) return(NULL)
  
  bst <- bst %>%
    mutate(
      id = str_extract(Field, "(?<=\\[)\\d+(?=\\])"),
      variable = sub("^.*\\]\\.", "", Field)
    )
  
  aktuell_tbl <- bst %>%
    select(id, variable, value = Aktuell) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  historie_tbl <- bst %>%
    select(id, variable, value = Historie) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  if (!("tree_species" %in% names(aktuell_tbl)) &&
      !("tree_species" %in% names(historie_tbl))) {
    return(NULL)
  }
  
  vergleich <- full_join(
    aktuell_tbl,
    historie_tbl,
    by = "tree_species"
  ) %>%
    mutate(
      Differenz = abs(
        coalesce(as.numeric(coverage.x), 0) -
          coalesce(as.numeric(coverage.y), 0)
      )
    )
  
  if (nrow(vergleich) == 0) return(NULL)
  
  vergleich %>%
    mutate(
      Maske = "Bestockung <4m",
      Wert_KT = paste0(tree_species, " | Anteil:", coverage.x),
      Wert_AT = paste0(tree_species, " | Anteil:", coverage.y),
      Unterschiede = Differenz
    ) %>%
    select(Maske, Wert_KT, Wert_AT, Unterschiede)

  bestockung_export$Maske[-1] <- ""
  
  bestockung_export
  })

# Bestockung größer 4m Vergleich ------------------------------------------

bestockung_gt4m_export <- safe_block({
  
  bst <- Vergleich_zwei %>%
    filter(str_detect(Field, "^structure_gt4m\\["))
  
  if (nrow(bst) == 0) return(NULL)
  
  bst <- bst %>%
    mutate(
      id = str_extract(Field, "(?<=\\[)\\d+(?=\\])"),
      variable = sub("^.*\\]\\.", "", Field)
    )
  
  aktuell_tbl <- bst %>%
    select(id, variable, value = Aktuell) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  historie_tbl <- bst %>%
    select(id, variable, value = Historie) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  if (!("tree_species" %in% names(aktuell_tbl)) &&
      !("tree_species" %in% names(historie_tbl))) {
    return(NULL)
  }
  
  vergleich <- full_join(
    aktuell_tbl,
    historie_tbl,
    by = "tree_species"
  ) %>%
    mutate(
      Differenz = abs(
        coalesce(as.numeric(count.x), 0) -
          coalesce(as.numeric(count.y), 0)
      )
    )
  
  if (nrow(vergleich) == 0) return(NULL)
  
  vergleich %>%
    mutate(
      Maske = "Bestockung >4m",
      Wert_KT = paste0(tree_species, " | Anzahl:", count.x),
      Wert_AT = paste0(tree_species, " | Anzahl:", count.y),
      Unterschiede = Differenz
    ) %>%
    select(Maske, Wert_KT, Wert_AT, Unterschiede)
 
  
  bestockung_gt4m_export$Maske[-1] <- ""
  
  bestockung_gt4m_export
})


traktnummer <- Vergleich_zwei %>%
  filter(Field == "cluster_name") %>%
  pull(Aktuell)

traktecke <- Vergleich_zwei %>%
  filter(Field == "plot_name") %>%
  pull(Aktuell)

datum_kt <- Vergleich_zwei %>%
  filter(Field == "position.start_measurement") %>%
  pull(Aktuell)

datum_at <- Vergleich_zwei %>%
  filter(Field == "position.start_measurement") %>%
  pull(Historie)

export_df <- bind_rows(
  wzp4_export,
  verjuengung_export,
  deadwood_export,
  bestockung_export,
  bestockung_gt4m_export
)

if (nrow(export_df) == 0) {
  export_df <- tibble(
    Maske = "Keine Daten vorhanden",
    Wert_KT = NA,
    Wert_AT = NA,
    Unterschiede = NA,
    Bemerkungen = NA
  )
}

export_df <- export_df %>%
  mutate(
    Traktnummer = "",
    Traktecke = "",
    Datum_KT = "",
    Datum_AT = "",
    AT_Personen = "",
    KT_Personen = ""
  ) %>%
  relocate(
    Traktnummer,
    Traktecke,
    Datum_KT,
    Datum_AT,
    AT_Personen,
    KT_Personen
  )

export_df$Traktnummer[1] <- traktnummer
export_df$Traktecke[1] <- traktecke

export_df$Datum_KT[1] <- datum_kt
export_df$Datum_AT[1] <- datum_at

wb <- createWorkbook()
addWorksheet(wb, "Vergleich")

writeData(
  wb,
  "Vergleich",
  export_df
)

header_style <- createStyle(
  textDecoration = "bold"
)

addStyle(
  wb,
  "Vergleich",
  header_style,
  rows = 1,
  cols = 1:ncol(export_df),
  gridExpand = TRUE
)

setColWidths(
  wb,
  "Vergleich",
  cols = 1:ncol(export_df),
  widths = "auto"
)

italic_style <- createStyle(textDecoration = "italic")
masken_zeilen <- which(export_df$Maske != "") + 1
addStyle(
  wb,
  "Vergleich",
  style = italic_style,
  rows = masken_zeilen,
  cols = 7,   # Spalte Maske
  gridExpand = TRUE
)

saveWorkbook(
  wb,
  "C:/Users/weber_h/Documents/R/Daten/Vergleichsergebnis_json.xlsx",
  overwrite = TRUE
)











