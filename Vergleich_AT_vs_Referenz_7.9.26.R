# ============================================================================
# 1. Pakete und Grundeinstellungen
# ============================================================================


library(jsonlite)
library(tidyverse)
library(openxlsx)
library(httr)

setwd("C:/Users/lutz/lutz/R/Comparison-Tool")


readRenviron(".env")

getwd()
list.files()


# Verzeichnisse -------------------------------------------------------------

input_dir <- "input"
output_dir <- "output"

dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ============================================================================
# 2. Hilfsfunktionen
# ============================================================================


has_data <- function(df) {
  !is.null(df) && nrow(df) > 0
}


safe_block <- function(fn) {
  tryCatch(
    fn(),
    error = function(e)
      NULL
  )
}


`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}


prompt_text <- function(label) {
  repeat {
    input <- trimws(readline(paste0(label, ": ")))
    
    if (nzchar(input)) {
      return(input)
    }
    
    message("Bitte einen Text eingeben.")
  }
}


# ============================================================================
# 3. tfm-api Login
# ============================================================================


tfm_api_login <- function() {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  email <- Sys.getenv("TFM_API_EMAIL")
  password <- Sys.getenv("TFM_API_PASSWORD")
  
  
  if (base_url == "" ||
      api_key == "" ||
      email == "" ||
      password == "") {
    stop(
      "Missing tfm-api credentials.
      Set TFM_API_URL, TFM_API_KEY, TFM_API_EMAIL and TFM_API_PASSWORD in .env."
    )
  }
  
  
  res <- POST(
    url = paste0(base_url, "auth/v1/token?grant_type=password"),
    
    add_headers("apikey" = api_key, "Content-Type" = "application/json"),
    
    body = list(email = email, password = password),
    
    encode = "json"
  )
  
  
  stop_for_status(res, "log in to tfm-api")
  
  
  content(res, as = "parsed", type = "application/json")$access_token
}


# ============================================================================
# 4. Trupp auswaehlen
# ============================================================================


get_troop_id <- function(troop_name, token = tfm_api_login()) {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  
  
  res <- GET(
    url = paste0(base_url, "rest/v1/troop"),
    
    query = list(
      name = paste0("ilike.*", troop_name, "*"),
      
      select = "id,name"
    ),
    
    add_headers(
      "apikey" = api_key,
      
      "Authorization" =
        paste("Bearer", token),
      
      "Accept" =
        "application/json",
      
      "Accept-Profile" =
        "public"
    )
  )
  
  
  stop_for_status(res)
  
  
  troops <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  
  if (nrow(troops) == 0) {
    stop("Kein passender Trupp gefunden.")
  }
  
  
  troops <- troops %>%
    arrange(name)
  
  
  if (nrow(troops) == 1) {
    cat("\nAusgewaehlter Trupp:", troops$name[1], "\n\n")
    
    return(troops$id[1])
  }
  
  
  cat("\nMehrere Trupps gefunden:\n\n")
  
  
  for (i in seq_len(nrow(troops))) {
    cat(sprintf("%2d: %s\n", i, troops$name[i]))
  }
  
  
  repeat {
    auswahl <- readline(sprintf("\nBitte Nummer auswaehlen (1-%d): ", nrow(troops)))
    
    
    if (!grepl("^[0-9]+$", auswahl)) {
      cat("Bitte eine Zahl eingeben.\n")
      next
    }
    
    
    auswahl <- as.integer(auswahl)
    
    
    if (auswahl >= 1 &&
        auswahl <= nrow(troops)) {
      cat("\nAusgewaehlter Trupp:", troops$name[auswahl], "\n\n")
      
      return(troops$id[auswahl])
    }
    
    
    cat("Ungueltige Auswahl.\n")
  }
}


# ============================================================================
# 5. Records laden
# ============================================================================


get_tfm_records <- function(troop_id, token = tfm_api_login()) {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  
  
  res <- GET(
    url = paste0(base_url, "rest/v1/records"),
    
    query = list(
      responsible_troop =
        paste0("eq.", troop_id),
      
      completed_at_troop =
        "not.is.null",
      
      select =
        paste(
          "cluster_name,",
          "plot_name,",
          "properties,",
          "previous_properties,",
          "completed_at_troop,",
          "responsible_troop"
        ),
      
      order =
        "cluster_name,plot_name"
    ),
    
    add_headers(
      "apikey" = api_key,
      
      "Authorization" =
        paste("Bearer", token),
      
      "Accept" =
        "application/json",
      
      "Accept-Profile" =
        "public"
    )
  )
  
  
  stop_for_status(res)
  
  
  records <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  if (length(records) == 0)
    stop("Keine Records fuer diesen Trupp gefunden.")
  
  records
}

flatten_json <- function(x, parent = "") {
  if (length(x) == 0) {
    return(tibble(Field = parent, Wert = NA_character_))
  }
  
  if (!is.list(x)) {
    return(tibble(Field = parent, Wert = as.character(x)))
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


# ============================================================================
# 6. Start: Trupp auswaehlen und Records laden
# ============================================================================


troop_name <- prompt_text("Name des Trupps (oder Teil des Namens)")

token <- tfm_api_login()

troop_id <- get_troop_id(troop_name = troop_name, token = token)

records <- get_tfm_records(troop_id = troop_id, token = token)

cat("\nGeladene Records:", length(records), "\n")


glimpse(records)


# ============================================================================
# 7. Vergleich Aufnahme 2027 vs. BWI 2022
# ============================================================================


compare_record <- function(record) {
  # Aktuelle Aufnahme 2027
  trees_2027 <- record$properties$tree %||% list()
  
  # Vorherige Aufnahme BWI 2022
  trees_2022 <- record$previous_properties$tree %||% list()
  
  
  # Anzahl der Bäume je Aufnahme
  anzahl_baeume_2027 <- length(trees_2027)
  anzahl_baeume_2022 <- length(trees_2022)
  
  
  # Wenn in beiden Aufnahmen keine Bäume vorhanden sind
  if (length(trees_2027) == 0 && length(trees_2022) == 0) {
    return(NULL)
  }
  
  
  # ==========================================================================
  # Bäume 2027
  # ==========================================================================
  
  trees_2027_tbl <- purrr::map_dfr(trees_2027, function(tree) {
    tibble(
      tree_id = tree$id %||% NA_character_,
      tree_number = tree$tree_number %||% NA,
      tree_species = tree$tree_species %||% NA,
      tree_status = tree$tree_status %||% NA,
      tree_height_2027 = tree$tree_height %||% NA,
      dbh_2027 = tree$dbh %||% NA
    )
  })
  
  
  # ==========================================================================
  # Bäume 2022
  # ==========================================================================
  
  trees_2022_tbl <- purrr::map_dfr(trees_2022, function(tree) {
    tibble(
      tree_id = tree$id %||% NA_character_,
      tree_species_2022 = tree$tree_species %||% NA,
      tree_status_2022 = tree$tree_status %||% NA,
      tree_height_2022 = tree$tree_height %||% NA,
      dbh_2022 = tree$dbh %||% NA
    )
  })
  
  
  # ==========================================================================
  # Leere Tabellen mit definierter Struktur
  # ==========================================================================
  
  if (nrow(trees_2027_tbl) == 0) {
    trees_2027_tbl <- tibble(
      tree_id = character(),
      tree_number = numeric(),
      tree_species = numeric(),
      tree_status = numeric(),
      tree_height_2027 = numeric(),
      dbh_2027 = numeric()
    )
  }
  
  
  if (nrow(trees_2022_tbl) == 0) {
    trees_2022_tbl <- tibble(
      tree_id = character(),
      tree_species_2022 = numeric(),
      tree_status_2022 = numeric(),
      tree_height_2022 = numeric(),
      dbh_2022 = numeric()
    )
  }
  
  
  # ==========================================================================
  # Aufnahme 2027 mit BWI 2022 vergleichen
  # ==========================================================================
  
  full_join(trees_2027_tbl, trees_2022_tbl, by = "tree_id") %>%
    mutate(
      cluster_name = record$cluster_name,
      plot_name = record$plot_name,
      anzahl_baeume_2027 = anzahl_baeume_2027,
      anzahl_baeume_2022 = anzahl_baeume_2022
    )
}


# ============================================================================
# Alle Records zusammenführen
# ============================================================================

vergleich_liste <- purrr::map(records, function(record) {
  vergleich <- compare_record(record)
  
  if (is.null(vergleich)) {
    return(NULL)
  }
  
  vergleich
})


vergleich_alle <- bind_rows(vergleich_liste)


# ============================================================================
# 8. Bundesland auswählen
# ============================================================================


get_states <- function(token = tfm_api_login()) {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  
  res <- GET(
    url = paste0(base_url, "rest/v1/lookup_state"),
    query = list(select = "code,name_de", order = "name_de"),
    add_headers(
      apikey = api_key,
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      "Accept-Profile" = "lookup"
    )
  )
  
  stop_for_status(res)
  
  fromJSON(content(res, "text", encoding = "UTF-8"))
}


# ============================================================================
# 9. Referenzdaten ci2017 und bwi2022 für das ausgewählte Bundesland laden
# ============================================================================


get_reference_data_2017_2022 <- function(state_code, token = tfm_api_login()) {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  
  # Alle Cluster des Bundeslands laden
  cluster_data <- GET(
    url = paste0(base_url, "rest/v1/cluster"),
    query = list(
      state_responsible = paste0("eq.", state_code),
      select = "cluster_name"
    ),
    add_headers(
      apikey = api_key,
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      "Accept-Profile" = "inventory_archive"
    )
  ) |>
    content("text", encoding = "UTF-8") |>
    fromJSON()
  
  # Cluster in Blöcke teilen
  cluster_chunks <- split(cluster_data$cluster_name, ceiling(seq_along(cluster_data$cluster_name) / 100))
  
  # Plots blockweise laden
  plot_data <- purrr::map_dfr(cluster_chunks, function(cluster_chunk) {
    cluster_filter <- paste0("in.(", paste(cluster_chunk, collapse = ","), ")")
    
    res_plot <- GET(
      url = paste0(base_url, "rest/v1/plot"),
      query = list(
        cluster_name = cluster_filter,
        select = paste(
          "id,",
          "cluster_name,",
          "plot_name,",
          "interval_name,",
          "trees_greater_4meter_basal_area_factor"
        )
      ),
      add_headers(
        apikey = api_key,
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        "Accept-Profile" = "inventory_archive"
      )
    )
    
    stop_for_status(res_plot)
    
    fromJSON(content(res_plot, "text", encoding = "UTF-8"))
  })
  
  # Plots in Blöcke teilen
  plot_chunks <- split(plot_data$id, ceiling(seq_along(plot_data$id) / 100))
  
  # Bäume blockweise laden
  tree_data <- purrr::map_dfr(plot_chunks, function(plot_chunk) {
    plot_filter <- paste0("in.(", paste(plot_chunk, collapse = ","), ")")
    
    res_tree <- GET(
      url = paste0(base_url, "rest/v1/tree"),
      query = list(
        plot_id = plot_filter,
        select = paste(
          "id,",
          "tree_number,",
          "tree_species,",
          "dbh,",
          "tree_height,",
          "plot_id"
        )
      ),
      add_headers(
        apikey = api_key,
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        "Accept-Profile" = "inventory_archive"
      )
    )
    
    stop_for_status(res_tree)
    
    fromJSON(content(res_tree, "text", encoding = "UTF-8"),
             simplifyDataFrame = TRUE)
  })
  
  tree_data <- tree_data %>%
    left_join(plot_data, by = c("plot_id" = "id"))
  
  list(tree_data = tree_data, plot_data = plot_data)
}


# ============================================================================
# Bundesland auswählen und Referenzdaten laden
# ============================================================================

states <- get_states(token)

auswahl <- menu(states$name_de, title = "Bitte Bundesland auswählen:")

state_code <- states$code[auswahl]

referenz_daten <- get_reference_data_2017_2022(state_code = state_code, token = token)


# ============================================================================
# 10. Baumartengruppen definieren
# ============================================================================


baumarten_gruppen <- tibble(
  tree_species = c(
    # ALH
    140,
    141,
    142,
    181,
    120,
    130,
    190,
    150,
    193,
    160,
    191,
    170,
    
    # ALN
    200,
    201,
    295,
    211,
    212,
    290,
    224,
    221,
    222,
    223,
    220,
    250,
    252,
    230,
    251,
    240,
    292,
    293,
    
    # BU
    100,
    
    # DGL
    40,
    
    # LAE
    50,
    51,
    
    # TA
    33,
    30,
    39,
    
    # FI
    94,
    90,
    10,
    12,
    19,
    
    # EI
    112,
    111,
    110,
    
    # KI
    21,
    20,
    22,
    25,
    24,
    29
  ),
  
  baumartengruppe = c(
    rep("ALH", 12),
    rep("ALN", 18),
    "BU",
    "DGL",
    rep("LAE", 2),
    rep("TA", 3),
    rep("FI", 5),
    rep("EI", 3),
    rep("KI", 6)
  )
)

glimpse(baumarten_gruppen)


# ============================================================================
# 11. Referenzdaten ci2017 und bwi2022 vorbereiten
# ============================================================================


referenz_baeume <- referenz_daten$tree_data
referenz_plots <- referenz_daten$plot_data

trees_2017 <- referenz_baeume %>%
  filter(interval_name == "ci2017") %>%
  select(
    plot_id,
    cluster_name,
    plot_name,
    interval_name,
    id,
    tree_number,
    tree_species,
    dbh,
    tree_height
  )

trees_2022 <- referenz_baeume %>%
  filter(interval_name == "bwi2022") %>%
  select(
    plot_id,
    cluster_name,
    plot_name,
    interval_name,
    id,
    tree_number,
    tree_species,
    dbh,
    tree_height
  )


# ============================================================================
# 12. BHD-Zuwachs 2017 -> 2022 berechnen
# ============================================================================


dbh_wachstum_2017_2022 <- trees_2017 %>%
  select(cluster_name, plot_name, tree_number, tree_species, dbh_2017 = dbh) %>%
  inner_join(
    trees_2022 %>%
      select(cluster_name, plot_name, tree_number, tree_species, dbh_2022 = dbh),
    by = c("cluster_name", "plot_name", "tree_number", "tree_species")
  ) %>%
  filter(!is.na(dbh_2017), !is.na(dbh_2022)) %>%
  mutate(dbh_zuwachs_2017_2022 = dbh_2022 - dbh_2017)


# ============================================================================
# 13. BHD-Referenzwerte nach Baumartengruppe
# ============================================================================


dbh_referenz_baumartengruppe <- dbh_wachstum_2017_2022 %>%
  left_join(baumarten_gruppen, by = "tree_species") %>%
  group_by(baumartengruppe) %>%
  summarise(
    anzahl_baeume_referenz = n(),
    
    mean_dbh_2017 = mean(dbh_2017, na.rm = TRUE),
    
    mean_dbh_2022 = mean(dbh_2022, na.rm = TRUE),
    
    mean_dbh_zuwachs_2017_2022 = mean(dbh_zuwachs_2017_2022, na.rm = TRUE),
    
    sd_dbh_zuwachs_2017_2022 = sd(dbh_zuwachs_2017_2022, na.rm = TRUE),
    
    median_dbh_zuwachs_2017_2022 = median(dbh_zuwachs_2017_2022, na.rm = TRUE),
    
    iqr_dbh_zuwachs_2017_2022 = IQR(dbh_zuwachs_2017_2022, na.rm = TRUE),
    
    .groups = "drop"
  )

glimpse(dbh_referenz_baumartengruppe)


# ============================================================================
# 14. Höhenwachstum 2017 -> 2022 berechnen
# ============================================================================


hoehe_wachstum_2017_2022 <- trees_2017 %>%
  select(cluster_name,
         plot_name,
         tree_number,
         tree_species,
         hoehe_2017 = tree_height) %>%
  inner_join(
    trees_2022 %>%
      select(
        cluster_name,
        plot_name,
        tree_number,
        tree_species,
        hoehe_2022 = tree_height
      ),
    by = c("cluster_name", "plot_name", "tree_number", "tree_species")
  ) %>%
  filter(!is.na(hoehe_2017), !is.na(hoehe_2022)) %>%
  mutate(hoehe_zuwachs_2017_2022 = hoehe_2022 - hoehe_2017)


# ============================================================================
# 15. Höhen-Referenzwerte nach Baumartengruppe
# ============================================================================


hoehe_referenz_baumartengruppe <- hoehe_wachstum_2017_2022 %>%
  left_join(baumarten_gruppen, by = "tree_species") %>%
  group_by(baumartengruppe) %>%
  summarise(
    anzahl_baeume_referenz = n(),
    
    mean_hoehe_2017 = mean(hoehe_2017, na.rm = TRUE),
    
    mean_hoehe_2022 = mean(hoehe_2022, na.rm = TRUE),
    
    mean_hoehe_zuwachs_2017_2022 = mean(hoehe_zuwachs_2017_2022, na.rm = TRUE),
    
    sd_hoehe_zuwachs_2017_2022 = sd(hoehe_zuwachs_2017_2022, na.rm = TRUE),
    
    median_hoehe_zuwachs_2017_2022 = median(hoehe_zuwachs_2017_2022, na.rm = TRUE),
    
    iqr_hoehe_zuwachs_2017_2022 = IQR(hoehe_zuwachs_2017_2022, na.rm = TRUE),
    
    .groups = "drop"
  )

glimpse(hoehe_referenz_baumartengruppe)


# ============================================================================
# 16. Vorbereitung Export BHD
# ============================================================================


bhd_export <- vergleich_alle %>%
  mutate(dbh_zuwachs_2022_2027 = dbh_2027 - dbh_2022) %>%
  left_join(baumarten_gruppen, by = "tree_species") %>%
  left_join(
    dbh_referenz_baumartengruppe %>%
      select(
        baumartengruppe,
        mean_dbh_zuwachs_2017_2022,
        sd_dbh_zuwachs_2017_2022,
        median_dbh_zuwachs_2017_2022,
        iqr_dbh_zuwachs_2017_2022
      ),
    by = "baumartengruppe"
  ) %>%
  mutate(dbh_plausibel = if_else(
    between(
      dbh_zuwachs_2022_2027,
      median_dbh_zuwachs_2017_2022 -
        1.5 * iqr_dbh_zuwachs_2017_2022,
      median_dbh_zuwachs_2017_2022 +
        1.5 * iqr_dbh_zuwachs_2017_2022
    ),
    "Ja",
    "Nein"
  )) %>%
  select(
    cluster_name,
    plot_name,
    tree_number,
    tree_species,
    baumartengruppe,
    anzahl_baeume_2022,
    anzahl_baeume_2027,
    dbh_2022,
    dbh_2027,
    dbh_zuwachs_2022_2027,
    mean_dbh_zuwachs_2017_2022,
    sd_dbh_zuwachs_2017_2022,
    median_dbh_zuwachs_2017_2022,
    iqr_dbh_zuwachs_2017_2022,
    dbh_plausibel
  )


# ============================================================================
# Spalten für Exportdarstellung leeren
# ============================================================================

cluster <- bhd_export$cluster_name
plot <- bhd_export$plot_name
anzahl_2022 <- bhd_export$anzahl_baeume_2022
anzahl_2027 <- bhd_export$anzahl_baeume_2027

bhd_export$cluster_name <- ""
bhd_export$plot_name <- ""
bhd_export$anzahl_baeume_2022 <- ""
bhd_export$anzahl_baeume_2027 <- ""

bhd_export$cluster_name[1] <- cluster[1]
bhd_export$plot_name[1] <- plot[1]
bhd_export$anzahl_baeume_2022[1] <- anzahl_2022[1]
bhd_export$anzahl_baeume_2027[1] <- anzahl_2027[1]

for (i in 2:nrow(bhd_export)) {
  # Neuer Cluster
  if (cluster[i] != cluster[i - 1]) {
    bhd_export$cluster_name[i] <- cluster[i]
  }
  
  # Neuer Plot innerhalb eines Clusters
  if (cluster[i] != cluster[i - 1] ||
      plot[i] != plot[i - 1]) {
    bhd_export$plot_name[i] <- plot[i]
    bhd_export$anzahl_baeume_2022[i] <- anzahl_2022[i]
    bhd_export$anzahl_baeume_2027[i] <- anzahl_2027[i]
  }
}

# Traktnummer und Traktecke für jede Datenzeile beibehalten.
bhd_export$cluster_name <- cluster
bhd_export$plot_name <- plot


# ============================================================================
# Spaltenüberschriften BHD anpassen
# ============================================================================

bhd_export <- bhd_export %>%
  rename(
    Traktnummer = cluster_name,
    Traktecke = plot_name,
    Baumnummer = tree_number,
    Baumart = tree_species,
    Baumartengruppe = baumartengruppe,
    Anzahl_Baeume_22 = anzahl_baeume_2022,
    Anzahl_Baeume_27 = anzahl_baeume_2027,
    BHD_22 = dbh_2022,
    BHD_27 = dbh_2027,
    `Zuwachs_BHD_22-27` = dbh_zuwachs_2022_2027,
    `Mean_Zuwachs_BHD_17-22` = mean_dbh_zuwachs_2017_2022,
    `SD_Zuwachs_BHD_17-22` = sd_dbh_zuwachs_2017_2022,
    `Median_Zuwachs_BHD_17-22` = median_dbh_zuwachs_2017_2022,
    `IQR_Zuwachs_BHD_17-22` = iqr_dbh_zuwachs_2017_2022,
    BHD_plausibel = dbh_plausibel
  )


# ============================================================================
# 17. Höhen-Traktbetrachtung
# ============================================================================


hoehe_export <- vergleich_alle %>%
  mutate(hoehe_zuwachs_2022_2027 = tree_height_2027 - tree_height_2022) %>%
  left_join(baumarten_gruppen, by = "tree_species") %>%
  left_join(
    hoehe_referenz_baumartengruppe %>%
      select(
        baumartengruppe,
        mean_hoehe_zuwachs_2017_2022,
        sd_hoehe_zuwachs_2017_2022,
        median_hoehe_zuwachs_2017_2022,
        iqr_hoehe_zuwachs_2017_2022
      ),
    by = "baumartengruppe"
  ) %>%
  mutate(hoehe_plausibel = if_else(
    between(
      hoehe_zuwachs_2022_2027,
      median_hoehe_zuwachs_2017_2022 -
        1.5 * iqr_hoehe_zuwachs_2017_2022,
      median_hoehe_zuwachs_2017_2022 +
        1.5 * iqr_hoehe_zuwachs_2017_2022
    ),
    "Ja",
    "Nein"
  )) %>%
  select(
    cluster_name,
    plot_name,
    tree_number,
    tree_species,
    baumartengruppe,
    anzahl_baeume_2022,
    anzahl_baeume_2027,
    hoehe_2022 = tree_height_2022,
    hoehe_2027 = tree_height_2027,
    hoehe_zuwachs_2022_2027,
    mean_hoehe_zuwachs_2017_2022,
    sd_hoehe_zuwachs_2017_2022,
    median_hoehe_zuwachs_2017_2022,
    iqr_hoehe_zuwachs_2017_2022,
    hoehe_plausibel
  )


# ============================================================================
# Spalten für Exportdarstellung leeren
# ============================================================================

cluster_hoehe <- hoehe_export$cluster_name
plot_hoehe <- hoehe_export$plot_name
anzahl_2022_hoehe <- hoehe_export$anzahl_baeume_2022
anzahl_2027_hoehe <- hoehe_export$anzahl_baeume_2027

hoehe_export$cluster_name <- ""
hoehe_export$plot_name <- ""
hoehe_export$anzahl_baeume_2022 <- ""
hoehe_export$anzahl_baeume_2027 <- ""

hoehe_export$cluster_name[1] <- cluster_hoehe[1]
hoehe_export$plot_name[1] <- plot_hoehe[1]
hoehe_export$anzahl_baeume_2022[1] <- anzahl_2022_hoehe[1]
hoehe_export$anzahl_baeume_2027[1] <- anzahl_2027_hoehe[1]

for (i in 2:nrow(hoehe_export)) {
  # Neuer Cluster
  if (cluster_hoehe[i] != cluster_hoehe[i - 1]) {
    hoehe_export$cluster_name[i] <- cluster_hoehe[i]
  }
  
  # Neuer Plot innerhalb eines Clusters
  if (cluster_hoehe[i] != cluster_hoehe[i - 1] ||
      plot_hoehe[i] != plot_hoehe[i - 1]) {
    hoehe_export$plot_name[i] <- plot_hoehe[i]
    hoehe_export$anzahl_baeume_2022[i] <- anzahl_2022_hoehe[i]
    hoehe_export$anzahl_baeume_2027[i] <- anzahl_2027_hoehe[i]
  }
}

# Traktnummer und Traktecke für jede Datenzeile beibehalten.
hoehe_export$cluster_name <- cluster_hoehe
hoehe_export$plot_name <- plot_hoehe


# ============================================================================
# Spaltenüberschriften Hoehe anpassen
# ============================================================================

hoehe_export <- hoehe_export %>%
  rename(
    Traktnummer = cluster_name,
    Traktecke = plot_name,
    Baumnummer = tree_number,
    Baumart = tree_species,
    Baumartengruppe = baumartengruppe,
    Anzahl_Baeume_22 = anzahl_baeume_2022,
    Anzahl_Baeume_27 = anzahl_baeume_2027,
    Hoehe_22 = hoehe_2022,
    Hoehe_27 = hoehe_2027,
    `Zuwachs_Hoehe_22-27` = hoehe_zuwachs_2022_2027,
    `Mean_Zuwachs_Hoehe_17-22` = mean_hoehe_zuwachs_2017_2022,
    `SD_Zuwachs_Hoehe_17-22` = sd_hoehe_zuwachs_2017_2022,
    `Median_Zuwachs_Hoehe_17-22` = median_hoehe_zuwachs_2017_2022,
    `IQR_Zuwachs_Hoehe_17-22` = iqr_hoehe_zuwachs_2017_2022,
    Hoehe_plausibel = hoehe_plausibel
  )


# ============================================================================
# 18. Bestockung 2022 vs. 2027
# ============================================================================


# ============================================================================
# Bestockungsdaten > 4 m auslesen
# ============================================================================

get_structure_gt4m_counts <- function(record, property_name) {
  structure <- record[[property_name]]$structure_gt4m %||% list()
  
  if (length(structure) == 0) {
    return(tibble(
      tree_species = numeric(),
      is_mirrored = logical(),
      count = numeric()
    ))
  }
  
  purrr::map_dfr(structure, function(x) {
    tibble(
      tree_species = as.numeric(x$tree_species %||% NA),
      is_mirrored = as.logical(x$is_mirrored %||% NA),
      count = as.numeric(x$count %||% NA)
    )
  })
}


# ============================================================================
# Bestockungsanzahl 2022 und 2027 vergleichen
# ============================================================================

bestockung_export <- purrr::map_dfr(records, function(record) {
  # Aktuelle Aufnahme 2027
  bestockung_2027 <- get_structure_gt4m_counts(record, "properties") %>%
    group_by(tree_species, is_mirrored) %>%
    summarise(anzahl_2027 = sum(count, na.rm = TRUE),
              .groups = "drop")
  
  # Vorherige Aufnahme 2022
  bestockung_2022 <- get_structure_gt4m_counts(record, "previous_properties") %>%
    group_by(tree_species, is_mirrored) %>%
    summarise(anzahl_2022 = sum(count, na.rm = TRUE),
              .groups = "drop")
  
  # 2022 und 2027 zusammenführen
  full_join(bestockung_2022,
            bestockung_2027,
            by = c("tree_species", "is_mirrored")) %>%
    mutate(cluster_name = record$cluster_name,
           plot_name = record$plot_name)
}) %>%
  mutate(
    anzahl_2022 = replace_na(anzahl_2022, 0),
    anzahl_2027 = replace_na(anzahl_2027, 0),
    
    anzahl_differenz_2022_2027 =
      anzahl_2027 - anzahl_2022,
    
    gespiegelt = case_when(is_mirrored ~ "ja", !is_mirrored ~ "nein", TRUE ~ NA_character_)
  ) %>%
  left_join(baumarten_gruppen, by = "tree_species")


# ============================================================================
# Zählfaktor 2027 aus properties laden
# ============================================================================

get_baf <- function(record, property_name) {
  baf <- record[[property_name]]$trees_greater_4meter_basal_area_factor %||% NA_real_
  
  as.numeric(baf)
}


zaehlfaktor_2027 <- purrr::map_dfr(records, function(record) {
  tibble(
    cluster_name = as.character(record$cluster_name),
    plot_name = as.character(record$plot_name),
    zaehlfaktor_2027 = get_baf(record, "properties")
  )
})


# ============================================================================
# Zählfaktor 2022 aus inventory_archive laden
# ============================================================================

zaehlfaktor_2022 <- referenz_plots %>%
  filter(interval_name == "bwi2022") %>%
  transmute(
    cluster_name = as.character(cluster_name),
    plot_name = as.character(plot_name),
    zaehlfaktor_2022 =
      trees_greater_4meter_basal_area_factor
  )


# ============================================================================
# Zählfaktoren 2022 und 2027 zusammenführen
# ============================================================================

zaehlfaktor_vergleich <- zaehlfaktor_2022 %>%
  full_join(zaehlfaktor_2027, by = c("cluster_name", "plot_name"))


# ============================================================================
# Bestockung und Zählfaktor zusammenführen
# ============================================================================

bestockung_export <- bestockung_export %>%
  mutate(cluster_name = as.character(cluster_name),
         plot_name = as.character(plot_name)) %>%
  left_join(zaehlfaktor_vergleich, by = c("cluster_name", "plot_name")) %>%
  select(
    cluster_name,
    plot_name,
    tree_species,
    baumartengruppe,
    gespiegelt,
    anzahl_2022,
    anzahl_2027,
    anzahl_differenz_2022_2027,
    zaehlfaktor_2022,
    zaehlfaktor_2027
  ) %>%
  arrange(cluster_name, plot_name, tree_species, gespiegelt)


# ============================================================================
# Spalten für Exportdarstellung leeren
# ============================================================================

cluster_bestockung <- bestockung_export$cluster_name
plot_bestockung <- bestockung_export$plot_name

zaehlfaktor_2022_export <-
  bestockung_export$zaehlfaktor_2022

zaehlfaktor_2027_export <-
  bestockung_export$zaehlfaktor_2027


bestockung_export$cluster_name <- ""
bestockung_export$plot_name <- ""
bestockung_export$zaehlfaktor_2022 <- ""
bestockung_export$zaehlfaktor_2027 <- ""


bestockung_export$cluster_name[1] <-
  cluster_bestockung[1]

bestockung_export$plot_name[1] <-
  plot_bestockung[1]

bestockung_export$zaehlfaktor_2022[1] <-
  zaehlfaktor_2022_export[1]

bestockung_export$zaehlfaktor_2027[1] <-
  zaehlfaktor_2027_export[1]


for (i in 2:nrow(bestockung_export)) {
  # Neuer Cluster
  if (cluster_bestockung[i] !=
      cluster_bestockung[i - 1]) {
    bestockung_export$cluster_name[i] <-
      cluster_bestockung[i]
  }
  
  # Neuer Plot innerhalb eines Clusters
  if (cluster_bestockung[i] !=
      cluster_bestockung[i - 1] ||
      plot_bestockung[i] !=
      plot_bestockung[i - 1]) {
    bestockung_export$plot_name[i] <-
      plot_bestockung[i]
    
    bestockung_export$zaehlfaktor_2022[i] <-
      zaehlfaktor_2022_export[i]
    
    bestockung_export$zaehlfaktor_2027[i] <-
      zaehlfaktor_2027_export[i]
  }
}

# Traktnummer und Traktecke für jede Datenzeile beibehalten.
bestockung_export$cluster_name <- cluster_bestockung
bestockung_export$plot_name <- plot_bestockung


# ============================================================================
# Spaltenüberschriften Bestockung anpassen
# ============================================================================

bestockung_export <- bestockung_export %>%
  rename(
    Traktnummer = cluster_name,
    Traktecke = plot_name,
    Baumart = tree_species,
    Baumartengruppe = baumartengruppe,
    Gespiegelt = gespiegelt,
    Anzahl_Baeume_22 = anzahl_2022,
    Anzahl_Baeume_27 = anzahl_2027,
    `Differenz_Anzahl_Baeume_22-27` = anzahl_differenz_2022_2027,
    ZF_22 = zaehlfaktor_2022,
    ZF_27 = zaehlfaktor_2027
  )


# ============================================================================
# Leere Trennzeile bei Wechsel der Traktnummer einfügen
# ============================================================================

add_cluster_separators <- function(
  df,
  cluster_col = "Traktnummer",
  plot_col = "Traktecke"
) {
  if (!(cluster_col %in% names(df))) {
    stop("Spalte '", cluster_col, "' wurde nicht gefunden.")
  }

  if (!(plot_col %in% names(df))) {
    stop("Spalte '", plot_col, "' wurde nicht gefunden.")
  }

  if (nrow(df) <= 1) {
    return(list(
      data = df,
      excel_rows = integer(0),
      plot_border_excel_rows = integer(0)
    ))
  }

  rows <- list()
  separator_data_rows <- integer(0)
  plot_border_data_rows <- integer(0)
  output_row <- 0L

  for (i in seq_len(nrow(df))) {
    if (i > 1) {
      cluster_changed <- !identical(
        as.character(df[[cluster_col]][i]),
        as.character(df[[cluster_col]][i - 1])
      )

      plot_changed <- !identical(
        as.character(df[[plot_col]][i]),
        as.character(df[[plot_col]][i - 1])
      )

      if (cluster_changed) {
        output_row <- output_row + 1L
        separator_data_rows <- c(separator_data_rows, output_row)
        rows[[length(rows) + 1L]] <- df[NA_integer_, ]
      } else if (plot_changed) {
        # Die Rahmenlinie wird unter der letzten Zeile der vorherigen Ecke
        # gesetzt. output_row bezeichnet an dieser Stelle genau diese Zeile.
        plot_border_data_rows <- c(plot_border_data_rows, output_row)
      }
    }

    output_row <- output_row + 1L
    rows[[length(rows) + 1L]] <- df[i, ]
  }

  list(
    data = bind_rows(rows),
    # Plus 1, weil Zeile 1 in Excel die Überschriften enthält.
    excel_rows = separator_data_rows + 1L,
    plot_border_excel_rows = plot_border_data_rows + 1L
  )
}

bhd_with_separators <- add_cluster_separators(bhd_export)
bhd_export <- bhd_with_separators$data
bhd_separator_rows <- bhd_with_separators$excel_rows
bhd_plot_border_rows <- bhd_with_separators$plot_border_excel_rows

hoehe_with_separators <- add_cluster_separators(hoehe_export)
hoehe_export <- hoehe_with_separators$data
hoehe_separator_rows <- hoehe_with_separators$excel_rows
hoehe_plot_border_rows <- hoehe_with_separators$plot_border_excel_rows

bestockung_with_separators <- add_cluster_separators(bestockung_export)
bestockung_export <- bestockung_with_separators$data
bestockung_separator_rows <- bestockung_with_separators$excel_rows
bestockung_plot_border_rows <-
  bestockung_with_separators$plot_border_excel_rows


# ============================================================================
# 19. Export BHD / Hoehe / Bestockung
# ============================================================================

# Truppname für Dateinamen bereinigen

troop_name_datei <- troop_name %>%
  str_replace_all("[^[:alnum:]_-]", "_")


# ============================================================================
# Dateiname
# ============================================================================

export_datei <- file.path(output_dir,
                          paste0("Vergleich_CI27_vs_Referenz_", troop_name_datei, ".xlsx"))


# ============================================================================
# Workbook erstellen
# ============================================================================

wb <- createWorkbook()


# ============================================================================
# Formatierungen
# ============================================================================

# Überschriften fett
header_style <- createStyle(textDecoration = "bold")

# Zwei Nachkommastellen
decimal_style <- createStyle(numFmt = "0.00")

# Dunkelgraue Trennzeile zwischen zwei Trakten
separator_style <- createStyle(fgFill = "#595959")

# Untere Rahmenlinie zwischen zwei Traktecken desselben Traktes
plot_border_style <- createStyle(
  border = "bottom",
  borderColour = "#000000",
  borderStyle = "thin"
)


# ============================================================================
# BHD
# ============================================================================

addWorksheet(wb, "BHD")
freezePane(wb, sheet = "BHD", firstRow = TRUE)

writeData(wb, sheet = "BHD", x = bhd_export)

addStyle(
  wb,
  sheet = "BHD",
  style = header_style,
  rows = 1,
  cols = 1:ncol(bhd_export),
  gridExpand = TRUE
)

if (length(bhd_separator_rows) > 0) {
  addStyle(
    wb,
    sheet = "BHD",
    style = separator_style,
    rows = bhd_separator_rows,
    cols = 1:ncol(bhd_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}

dbh_decimal_cols <- which(
  names(bhd_export) %in% c(
    "Mean_Zuwachs_BHD_17-22",
    "SD_Zuwachs_BHD_17-22"
  )
)

addStyle(
  wb,
  sheet = "BHD",
  style = decimal_style,
  rows = 2:(nrow(bhd_export) + 1),
  cols = dbh_decimal_cols,
  gridExpand = TRUE,
  stack = TRUE
)

if (length(bhd_plot_border_rows) > 0) {
  addStyle(
    wb,
    sheet = "BHD",
    style = plot_border_style,
    rows = bhd_plot_border_rows,
    cols = 1:ncol(bhd_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}


# ============================================================================
# Hoehe
# ============================================================================

addWorksheet(wb, "Hoehe")
freezePane(wb, sheet = "Hoehe", firstRow = TRUE)

writeData(wb, sheet = "Hoehe", x = hoehe_export)

addStyle(
  wb,
  sheet = "Hoehe",
  style = header_style,
  rows = 1,
  cols = 1:ncol(hoehe_export),
  gridExpand = TRUE
)

if (length(hoehe_separator_rows) > 0) {
  addStyle(
    wb,
    sheet = "Hoehe",
    style = separator_style,
    rows = hoehe_separator_rows,
    cols = 1:ncol(hoehe_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}

hoehe_decimal_cols <- which(
  names(hoehe_export) %in% c(
    "Mean_Zuwachs_Hoehe_17-22",
    "SD_Zuwachs_Hoehe_17-22"
  )
)

addStyle(
  wb,
  sheet = "Hoehe",
  style = decimal_style,
  rows = 2:(nrow(hoehe_export) + 1),
  cols = hoehe_decimal_cols,
  gridExpand = TRUE,
  stack = TRUE
)

if (length(hoehe_plot_border_rows) > 0) {
  addStyle(
    wb,
    sheet = "Hoehe",
    style = plot_border_style,
    rows = hoehe_plot_border_rows,
    cols = 1:ncol(hoehe_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}


# ============================================================================
# Bestockung
# ============================================================================

addWorksheet(wb, "Bestockung")
freezePane(wb, sheet = "Bestockung", firstRow = TRUE)

writeData(wb, sheet = "Bestockung", x = bestockung_export)

addStyle(
  wb,
  sheet = "Bestockung",
  style = header_style,
  rows = 1,
  cols = 1:ncol(bestockung_export),
  gridExpand = TRUE
)

if (length(bestockung_separator_rows) > 0) {
  addStyle(
    wb,
    sheet = "Bestockung",
    style = separator_style,
    rows = bestockung_separator_rows,
    cols = 1:ncol(bestockung_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}

if (length(bestockung_plot_border_rows) > 0) {
  addStyle(
    wb,
    sheet = "Bestockung",
    style = plot_border_style,
    rows = bestockung_plot_border_rows,
    cols = 1:ncol(bestockung_export),
    gridExpand = TRUE,
    stack = TRUE
  )
}


# ============================================================================
# Excel-Datei speichern
# ============================================================================

saveWorkbook(wb, file = export_datei, overwrite = TRUE)


# ============================================================================
# Erfolgsmeldung
# ============================================================================

if (file.exists(export_datei)) {
  cat("Excel-Datei wurde erstellt:\n", export_datei)
}
