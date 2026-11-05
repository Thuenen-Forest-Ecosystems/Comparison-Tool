# ============================================================================
# ERSTER abgegebener AT-Stand gegen LETZTEN abgegebenen AT-Stand
# Beide Datenstaende haben dieselbe responsible_troop-ID.
# Der dazwischenliegende KT-Stand wird NICHT in den Vergleich einbezogen.
# Auswahl: kleinster / groesster completed_at_troop aus records + record_changes.
# Differenz: letzter AT-Stand minus erster AT-Stand (Neu - Alt).
# ============================================================================

library(jsonlite)
library(tidyverse)
library(openxlsx)
library(httr)

setwd("C:/Users/lutz/lutz/R/Comparison-Tool")
readRenviron(".env")

# Die AT-Trupp-ID wird nach Eingabe der Traktnummer automatisch ermittelt.
export_timezone <- "Europe/Berlin"

# Downloaded records go to input/, generated files go to output/.
input_dir <- "input"
output_dir <- "output"
dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

has_data <- function(df) {
  !is.null(df) && nrow(df) > 0
}

# Der Block ist eine Funktion
# (not a bare { } expression) so that early `return(NULL)` calls inside it
# return from the block instead of aborting the enclosing script.
safe_block <- function(mask_name, fn) {
  tryCatch(fn(), error = function(e) {
    stop("Fehler in Maske '", mask_name, "': ", conditionMessage(e), call. = FALSE)
  })
}

# tfm-api connection -------------------------------------------------------

tfm_api_login <- function() {
  base_url <- Sys.getenv("TFM_API_URL")
  api_key <- Sys.getenv("TFM_API_KEY")
  email <- Sys.getenv("TFM_API_EMAIL")
  password <- Sys.getenv("TFM_API_PASSWORD")
  
  if (base_url == "" || api_key == "" || email == "" || password == "") {
    stop("Missing tfm-api credentials. Set TFM_API_URL, TFM_API_KEY, TFM_API_EMAIL and TFM_API_PASSWORD in .env (see .env.example).")
  }
  
  res <- POST(
    url = paste0(base_url, "auth/v1/token?grant_type=password"),
    add_headers(
      "apikey" = api_key,
      "Content-Type" = "application/json"
    ),
    body = list(email = email, password = password),
    encode = "json"
  )
  
  stop_for_status(res, "log in to tfm-api")
  
  content(res, as = "parsed", type = "application/json")$access_token
}

# Trupp-Zuordnung aus aktuellen UND archivierten Datensaetzen ermitteln.
# Pagination verhindert, dass eine lange Historie relevante Trupps abschneidet.
get_cluster_troops <- function(cluster_name, token) {
  fetch_metadata <- function(source) {
    rows <- list()
    offset <- 0L
    repeat {
      query <- list(
        cluster_name = paste0("eq.", cluster_name),
        select = "plot_name,responsible_troop,completed_at_troop",
        order = if (source == "record_changes") {
          "plot_name.asc,created_at.asc"
        } else "plot_name.asc",
        limit = 500L, offset = offset
      )
      res <- GET(
        url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/", source),
        query = query,
        add_headers(
          apikey = Sys.getenv("TFM_API_KEY"),
          Authorization = paste("Bearer", token),
          Accept = "application/json", `Accept-Profile` = "public"
        )
      )
      stop_for_status(res, paste0("Truppzuordnung aus ", source, " laden"))
      batch <- fromJSON(content(res, as = "text", encoding = "UTF-8"),
                        simplifyVector = FALSE)
      if (!length(batch)) break
      rows <- c(rows, batch)
      # Auch bei serverseitig kleineren Seiten bis zur ersten leeren Seite laden.
      offset <- offset + length(batch)
    }
    rows
  }
  rows <- c(fetch_metadata("records"), fetch_metadata("record_changes"))
  scalar_text <- function(x) {
    if (length(x) != 1L || is.list(x) || is.na(x)) return(NA_character_)
    x <- trimws(as.character(x))
    if (nzchar(x)) x else NA_character_
  }
  metadata <- purrr::map_dfr(rows, function(x) {
    tibble(
      Traktecke = suppressWarnings(as.integer(scalar_text(x$plot_name))),
      Trupp_ID = tolower(scalar_text(x$responsible_troop)),
      Abschluss = scalar_text(x$completed_at_troop)
    )
  })
  if (!nrow(metadata)) stop("Keine Datensaetze fuer Trakt ", cluster_name, " gefunden.")
  metadata <- metadata %>% filter(Traktecke %in% 1:4, !is.na(Trupp_ID))
  if (!nrow(metadata)) return(tibble(
    Traktecke = integer(), Trupp_ID = character(),
    Abschluss = character(), Truppname = character(), Kontrolltrupp = logical()
  ))

  # Jeder am Trakt gefundene Trupp wird genau einmal ueber public.troop geprueft.
  roles <- purrr::map_dfr(sort(unique(metadata$Trupp_ID)), function(troop_id) {
    res <- GET(
      url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/troop"),
      query = list(id = paste0("eq.", troop_id),
                   select = "id,name,is_control_troop", limit = 2L),
      add_headers(
        apikey = Sys.getenv("TFM_API_KEY"),
        Authorization = paste("Bearer", token),
        Accept = "application/json", `Accept-Profile` = "public"
      )
    )
    stop_for_status(res, "Aufnahme-/Kontrolltrupp pruefen")
    troops <- fromJSON(content(res, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE)
    if (length(troops) != 1L ||
        !identical(tolower(scalar_text(troops[[1L]]$id)), troop_id)) {
      stop("Trupp-ID nicht eindeutig pruefbar: ", troop_id)
    }
    flag <- tolower(scalar_text(troops[[1L]]$is_control_troop))
    # Nur explizit FALSE gilt als Aufnahmetrupp; NULL/NA ist keine Freigabe.
    if (is.na(flag) || !flag %in% c("true", "false", "t", "f", "1", "0")) {
      stop("is_control_troop fehlt oder ist ungueltig fuer Trupp ", troop_id, ".")
    }
    is_control <- flag %in% c("true", "t", "1")
    name <- scalar_text(troops[[1L]]$name)
    message("Trupp ", coalesce(name, troop_id), ": ",
            if (is_control) "Kontrolltrupp - ausgeschlossen" else "Aufnahmetrupp - geprueft")
    tibble(Trupp_ID = troop_id, Truppname = name, Kontrolltrupp = is_control)
  })
  left_join(metadata, roles, by = "Trupp_ID") %>% distinct()
}

# Pro Ecke nur zwei Staende desselben, nachweislich geprueften AT vergleichen.
# Mehrere ATs pro Ecke werden nicht stillschweigend miteinander vermischt.
select_plot_at_troop <- function(troop_metadata, plot_name) {
  candidates <- troop_metadata %>%
    filter(Traktecke == plot_name, Kontrolltrupp == FALSE, !is.na(Abschluss)) %>%
    distinct(Trupp_ID, Truppname)
  if (!nrow(candidates)) return(NULL)
  if (nrow(candidates) > 1L) {
    stop("Mehrere Aufnahmetrupps mit abgeschlossenen Staenden an Ecke ", plot_name,
         ": ", paste(paste0(coalesce(candidates$Truppname, "ohne Namen"),
                             " [", candidates$Trupp_ID, "]"), collapse = "; "),
         ". Eine eindeutige Auswahl ist erforderlich; kein Vergleich zwischen verschiedenen ATs.")
  }
  candidates$Trupp_ID[[1L]]
}

# Ersten und letzten abgegebenen Stand DESSELBEN Trupps ermitteln.
# Erster Stand = kleinster completed_at_troop; bei gleicher Abgabe aelteste Historie.
# Letzter Stand = groesster completed_at_troop; bei Gleichstand records bevorzugen,
# sonst juengste Historie. KT-Aufnahmen werden durch die Trupp-ID ausgeschlossen.
select_first_last_at <- function(candidates) {
  if (length(candidates) < 2L) return(NULL)
  completed <- vapply(candidates, function(x) x$.completed_numeric, numeric(1))
  archived <- vapply(candidates, function(x) x$.archived_numeric, numeric(1))
  current <- vapply(candidates, function(x) identical(x$.source, "records"), logical(1))
  if (length(unique(completed)) < 2L) return(NULL)
  # Am ersten Abschluss die zuerst archivierte Fassung erhalten.
  first <- order(completed, as.integer(current), archived)[[1L]]
  # Am letzten Abschluss die aktuelle bzw. zuletzt archivierte Fassung verwenden.
  last <- order(-completed, -as.integer(current), -archived)[[1L]]
  list(alt = candidates[[first]], neu = candidates[[last]])
}

get_first_last_completed_at <- function(cluster_name, plot_name, troop_id, token) {
  fetch <- function(source, which_end) {
    query <- list(
      cluster_name = paste0("eq.", cluster_name),
      plot_name = paste0("eq.", plot_name),
      responsible_troop = paste0("eq.", troop_id),
      completed_at_troop = "not.is.null",
      select = "properties,completed_at_troop,responsible_troop"
    )
    if (source == "record_changes") {
      query$select <- paste0(query$select, ",created_at")
      query$order <- if (which_end == "erster") {
        "completed_at_troop.asc,created_at.asc"
      } else {
        "completed_at_troop.desc,created_at.desc"
      }
      query$limit <- 1L
    } else {
      query$limit <- 2L
    }
    res <- GET(
      url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/", source),
      query = query,
      add_headers(
        apikey = Sys.getenv("TFM_API_KEY"),
        Authorization = paste("Bearer", token),
        Accept = "application/json", `Accept-Profile` = "public"
      )
    )
    stop_for_status(res, paste0("AT-Stand aus ", source, " laden"))
    raw <- content(res, as = "text", encoding = "UTF-8")
    writeLines(raw, file.path(input_dir, sprintf(
      "%s_AT_%s_%s_%s.json", source, which_end, cluster_name, plot_name
    )))
    rows <- fromJSON(raw, simplifyVector = FALSE)
    if (!length(rows)) return(NULL)
    if (length(rows) != 1L) stop("Aktueller AT-Stand ist nicht eindeutig.")
    x <- rows[[1L]]
    if (length(x$responsible_troop) != 1L ||
        !identical(tolower(x$responsible_troop), tolower(troop_id))) {
      stop("Unerwartete Trupp-ID im AT-Datensatz.")
    }
    stamp <- x$completed_at_troop
    if (length(stamp) != 1L || is.na(stamp) || !nzchar(trimws(stamp))) {
      stop("Abschlusszeitstempel des AT fehlt.")
    }
    # ISO-Zeitpunkte inklusive Offset und Sekundenbruchteilen vergleichen.
    parse_iso <- function(z) {
      z <- sub("Z$", "+0000", z)
      z <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", z)
      ans <- as.POSIXct(z, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      if (is.na(ans)) stop("Zeitstempel nicht lesbar: ", z)
      as.numeric(ans)
    }
    x$.source <- source
    x$.completed_numeric <- parse_iso(stamp)
    x$.archived_numeric <- if (source == "record_changes") {
      if (length(x$created_at) != 1L || is.na(x$created_at)) stop("Archivzeitstempel fehlt.")
      parse_iso(x$created_at)
    } else Inf
    x
  }

  # Drei gezielte Abrufe reichen: aktueller AT, fruehester AT, spaetester AT.
  candidates <- Filter(Negate(is.null), list(
    fetch("records", "aktuell"),
    fetch("record_changes", "erster"),
    fetch("record_changes", "letzter")
  ))
  pair <- select_first_last_at(candidates)
  if (is.null(pair)) {
    if (!length(candidates)) return(NULL)
    # Ein einzelner Abschluss bleibt fuer die Waldentscheid-Anzeige erhalten.
    completed <- vapply(candidates, function(x) x$.completed_numeric, numeric(1))
    archived <- vapply(candidates, function(x) x$.archived_numeric, numeric(1))
    current <- vapply(candidates, function(x) identical(x$.source, "records"), logical(1))
    last <- order(-completed, -as.integer(current), -archived)[[1L]]
    pair <- list(alt = NULL, neu = candidates[[last]], vergleichbar = FALSE)
  } else {
    pair$vergleichbar <- TRUE
  }
  for (x in Filter(Negate(is.null), pair[c("alt", "neu")])) {
    if (is.null(x$properties) || !length(x$properties)) {
      stop("Messdaten im ausgewaehlten AT-Stand fehlen.")
    }
  }
  pair
}

# Waldentscheid: Code und deutsche Bezeichnung ------------------------------
# Zuordnung gemaess TFM: plot.forest_status -> lookup.lookup_forest_status.code.
# Die Lookup-Tabelle wird einmal pro Skriptlauf geladen.
get_forest_status_lookup <- function(token) {
  empty_lookup <- tibble(code = character(), name_de = character())
  
  tryCatch({
    res <- GET(
      url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/lookup_forest_status"),
      query = list(select = "code,name_de", order = "code.asc"),
      add_headers(
        "apikey" = Sys.getenv("TFM_API_KEY"),
        "Authorization" = paste("Bearer", token),
        "Accept" = "application/json",
        "Accept-Profile" = "lookup"
      )
    )
    
    stop_for_status(res, "Waldentscheid-Lookup laden")
    lookup <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    
    if (!is.data.frame(lookup) ||
        !all(c("code", "name_de") %in% names(lookup))) {
      stop("Lookup enthaelt keine Tabelle mit code und name_de.")
    }
    
    lookup <- as_tibble(lookup) %>%
      transmute(code = as.character(code), name_de = as.character(name_de)) %>%
      distinct()
    
    if (anyDuplicated(lookup$code)) {
      stop("Waldentscheid-Lookup enthaelt mehrdeutige Codes.")
    }
    
    lookup
  }, error = function(e) {
    warning(
      "Waldentscheid-Bezeichnungen konnten nicht geladen werden: ",
      conditionMessage(e),
      ". Waldentscheide bleiben als nicht aufgeloest markiert.",
      call. = FALSE
    )
    empty_lookup
  })
}

# Je nach JSON-Struktur steht das Feld unter plot.forest_status oder
# direkt unter forest_status. Fehlende Werte werden nicht als Code 0 behandelt.
get_forest_status <- function(flat_tbl, lookup) {
  field_index <- match("plot.forest_status", flat_tbl$Field)
  if (is.na(field_index)) {
    field_index <- match("forest_status", flat_tbl$Field)
  }
  
  code <- if (is.na(field_index)) {
    NA_character_
  } else {
    as.character(flat_tbl$Wert[[field_index]])
  }
  
  if (is.na(code) || !nzchar(trimws(code))) {
    return(list(code = "", name = "nicht angegeben"))
  }
  
  code <- trimws(code)
  name <- lookup$name_de[match(code, lookup$code)]
  
  if (is.na(name) || !nzchar(trimws(name))) {
    name <- "Bezeichnung nicht aufgeloest"
  }
  
  list(code = code, name = name)
}

# Waldentscheid direkt aus einer ausgewaehlten Aufnahme mit Code darstellen.
forest_status_text <- function(properties, lookup) {
  code <- if (is.list(properties$plot)) properties$plot$forest_status else NULL
  if (is.null(code) || !length(code)) code <- properties$forest_status
  if (length(code) != 1L || is.list(code)) code <- NA_character_
  status <- get_forest_status(
    tibble(Field = "forest_status", Wert = as.character(code)), lookup
  )
  paste0("Waldentscheid: ",
         if (nzchar(status$code)) paste0(status$code, "; ") else "", status$name)
}

# Alle vier Traktecken unabhaengig von Trupp und Abschluss anzeigen.
# Die Uebersicht zeigt den aktuellen Waldstatus aus records, nicht den AT-Vergleich.
# Auch Nichtwald/Nichtholzboden ohne Aufnahmetrupp wird dadurch beruecksichtigt.
# Interne Hilfstabelle fuer Ecke-Zeilen; kein separates Excel-Blatt.
get_traktecken_uebersicht <- function(cluster_name, token, lookup) {
  res <- GET(
    url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/records"),
    query = list(
      cluster_name = paste0("eq.", cluster_name),
      select = "plot_name,properties",
      order = "plot_name.asc"
    ),
    add_headers(
      apikey = Sys.getenv("TFM_API_KEY"),
      Authorization = paste("Bearer", token),
      Accept = "application/json", `Accept-Profile` = "public"
    )
  )
  stop_for_status(res, "Waldstatus aller vier Traktecken laden")
  records <- fromJSON(
    content(res, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  record_plot_names <- vapply(records, function(x) {
    if (length(x$plot_name) != 1L) return(NA_integer_)
    suppressWarnings(as.integer(x$plot_name))
  }, integer(1))

  purrr::map_dfr(1:4, function(ecke) {
    rows <- records[which(record_plot_names == ecke)]
    if (!length(rows)) {
      return(tibble(
        Traktnummer = cluster_name, Traktecke = ecke,
        Waldstatus = "nicht angegeben (kein aktueller Datensatz vorhanden)"
      ))
    }
    codes <- vapply(rows, function(x) {
      properties <- x$properties
      if (!is.list(properties)) return(NA_character_)
      code <- if (is.list(properties$plot)) properties$plot$forest_status else NULL
      if (is.null(code) || !length(code)) code <- properties$forest_status
      if (length(code) != 1L || is.list(code) || is.na(code)) return(NA_character_)
      code <- trimws(as.character(code))
      if (!nzchar(code)) NA_character_ else code
    }, character(1))
    # Mehrere aktuelle Datensaetze nur bei uebereinstimmendem Status zulassen.
    codes <- unique(codes[!is.na(codes)])
    if (length(codes) > 1L) {
      stop("Widerspruechliche aktuelle Waldstatus fuer Trakt ", cluster_name,
           ", Ecke ", ecke, ": ", paste(codes, collapse = ", "), ".")
    }
    code <- if (length(codes)) codes[[1L]] else NA_character_
    status <- get_forest_status(
      tibble(Field = "forest_status", Wert = code), lookup
    )
    tibble(
      Traktnummer = cluster_name, Traktecke = ecke,
      Waldstatus = paste0(
        if (nzchar(status$code)) paste0(status$code, "; ") else "",
        status$name
      )
    )
  }) %>% arrange(Traktnummer, Traktecke)
}

# Abschlusszeitstempel korrekt aus der Quellzeitzone nach Berlin umrechnen.
format_export_datum <- function(x) {
  if (length(x) == 0L || is.na(x[[1]])) return(NA_character_)
  zeit <- lubridate::ymd_hms(as.character(x[[1]]), quiet = TRUE, tz = "UTC")
  if (is.na(zeit)) stop("Zeitstempel nicht lesbar: ", x[[1]])
  format(zeit, format = "%d.%m.%Y; %H:%M Uhr", tz = export_timezone)
}

# Truppnamen fuer die Exportanzeige ueber public.troop aufloesen.
# Jede benoetigte UUID wird pro Skriptlauf nur einmal abgefragt.
make_troop_name_resolver <- function(token) {
  cache <- new.env(parent = emptyenv())
  
  function(troop_id) {
    if (length(troop_id) == 0 || is.na(troop_id[[1]])) {
      return(NA_character_)
    }
    
    troop_id <- tolower(trimws(as.character(troop_id[[1]])))
    if (!nzchar(troop_id)) {
      return(NA_character_)
    }
    
    if (exists(troop_id, envir = cache, inherits = FALSE)) {
      return(get(troop_id, envir = cache, inherits = FALSE))
    }
    
    troop_name <- tryCatch({
      res <- GET(
        url = paste0(Sys.getenv("TFM_API_URL"), "rest/v1/troop"),
        query = list(
          id = paste0("eq.", troop_id),
          select = "id,name",
          limit = 1
        ),
        add_headers(
          "apikey" = Sys.getenv("TFM_API_KEY"),
          "Authorization" = paste("Bearer", token),
          "Accept" = "application/json",
          "Accept-Profile" = "public"
        )
      )
      
      stop_for_status(res, "Truppname aus public.troop laden")
      troops <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
      
      if (!is.data.frame(troops) || nrow(troops) != 1 ||
          !all(c("id", "name") %in% names(troops))) {
        stop("Kein lesbarer Trupp mit dieser ID in public.troop gefunden.")
      }
      
      if (!identical(tolower(as.character(troops$id[[1]])), troop_id)) {
        stop("Zurueckgegebene Trupp-ID stimmt nicht mit der angefragten ID ueberein.")
      }
      
      name <- as.character(troops$name[[1]])
      if (is.na(name) || !nzchar(trimws(name))) {
        stop("Der zugeordnete Trupp hat keinen Namen.")
      }
      
      name
    }, error = function(e) {
      warning(
        "Truppname fuer ID ", troop_id, " konnte nicht zugeordnet werden: ",
        conditionMessage(e), ". Namensfeld im Export bleibt leer.",
        call. = FALSE
      )
      NA_character_
    })
    
    assign(troop_id, troop_name, envir = cache)
    troop_name
  }
}

# Numerische Exportdifferenz passend zur Spaltenreihenfolge: AT minus KT.
# Fehlende Werte werden wie in den bisherigen Differenzrechnungen als 0 behandelt.
# Interne absolute Differenzen fuer Toleranzpruefungen bleiben unveraendert.
export_differenz <- function(wert_at, wert_kt) {
  coalesce(suppressWarnings(as.numeric(wert_at)), 0) -
    coalesce(suppressWarnings(as.numeric(wert_kt)), 0)
}

# Record to compare ---------------------------------------------------------

prompt_integer <- function(label) {
  repeat {
    input <- trimws(readline(paste0(label, ": ")))
    value <- suppressWarnings(as.integer(input))
    if (!is.na(value)) {
      return(value)
    }
    message("Bitte eine gueltige Zahl eingeben.")
  }
}

cluster_name <- prompt_integer("Traktnummer (cluster_name)")
token <- tfm_api_login()
troop_metadata <- get_cluster_troops(cluster_name, token)
get_troop_name <- make_troop_name_resolver(token)
forest_status_lookup <- get_forest_status_lookup(token)
plot_names <- 1:4
traktecken_uebersicht <- get_traktecken_uebersicht(
  cluster_name, token, forest_status_lookup
)

message(
  "Gefundene Traktecken fuer Trakt ",
  cluster_name,
  ": ",
  paste(plot_names, collapse = ", ")
)

# Processes one plot and returns its rows for the combined Excel export.
compare_plot <- function(cluster_name, plot_name, token, forest_status_lookup, get_troop_name) {
  message(
    "Verarbeite Trakt ", cluster_name,
    ", Ecke ", plot_name, " ..."
  )
  
  at_troop_id <- select_plot_at_troop(troop_metadata, plot_name)
  record_pair <- if (is.null(at_troop_id)) NULL else get_first_last_completed_at(
    cluster_name, plot_name, at_troop_id, token
  )
  if (is.null(record_pair) || !isTRUE(record_pair$vergleichbar)) {
    warning(
      "Trakt ", cluster_name, ", Ecke ", plot_name,
      ": weniger als zwei unterschiedliche abgegebene AT-Staende. Nur Waldentscheid wird ausgegeben.",
      call. = FALSE
    )
    # Fehlende historische AT-Staende nicht durch KT- oder aktuelle Daten ersetzen.
    status <- traktecken_uebersicht %>%
      filter(Traktnummer == cluster_name, Traktecke == plot_name)
    if (nrow(status) != 1L) stop("Waldstatus-Uebersicht ist nicht eindeutig.")
    single <- if (is.null(record_pair)) NULL else record_pair$neu
    status_alt <- "Waldentscheid: kein aelterer abgeschlossener AT-Stand vorhanden"
    status_neu <- if (!is.null(single)) {
      forest_status_text(single$properties, forest_status_lookup)
    } else paste0("Waldentscheid: ", status$Waldstatus, " (aktueller Stand)")
    return(tibble(
      Traktnummer = as.character(cluster_name),
      Traktecke = as.character(plot_name),
      Datum_KT = "",
      Datum_AT = if (is.null(single)) "" else format_export_datum(single$completed_at_troop),
      AT_Personen = if (is.null(single)) "" else get_troop_name(single$responsible_troop),
      KT_Personen = "",
      Maske = "Ecke", Wert_KT = status_alt, Wert_AT = status_neu,
      Unterschiede = "",
      Bemerkungen = paste0(
        "Kein AT-Vergleich: weniger als zwei unterschiedliche abgegebene AT-Staende. ",
        if (is.null(single)) {
          "Kein abgeschlossener Stand eines geprueften Aufnahmetrupps vorhanden; aktueller Waldentscheid ohne Zuordnung zu AT alt/neu."
        } else "Waldentscheid neu aus dem einzigen abgeschlossenen AT-Stand; alter AT-Stand fehlt."
      )
    ))
  }

  # Bestehende Maskenanalysen beibehalten:
  # interne KT-/Aktuell-Seite = ERSTER AT-Stand (alt)
  # interne AT-/Historie-Seite = LETZTER AT-Stand (neu)
  # Damit bleibt die vorhandene Differenz AT minus KT = NEU minus ALT.
  record <- record_pair$alt
  latest_change <- record_pair$neu
  json_aktuell <- record$properties
  json_historie <- latest_change$properties
  message(
    "  Erster AT: ", format_export_datum(record$completed_at_troop),
    " (", record$.source, ") | Letzter AT: ",
    format_export_datum(latest_change$completed_at_troop),
    " (", latest_change$.source, ") | Trupp: ", get_troop_name(at_troop_id)
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
  
  # Waldentscheid unabhaengig von vorhandenen Baum-/Maskendaten ermitteln.
  waldstatus_kt <- get_forest_status(aktuell_tbl, forest_status_lookup)
  waldstatus_at <- get_forest_status(historie_tbl, forest_status_lookup)
  
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
  
  # ============================================================================
  # WZP4: Beschriftungen und Hilfsfunktionen
  # ============================================================================
  
  wzp4_beschriftungen <- c(
    dbh              = "BHD",
    dbh_height       = "BHD-Messhoehe",
    tree_height      = "Baumhoehe",
    tree_number      = "BaumNr",
    distance         = "Distanz",
    tree_status      = "PK",
    pruning          = "Astung",
    cave_tree        = "Höhlenbaum",
    deprecated       = "deprecated",
    bark_pocket      = "Rindentasche",
    damage_dead      = "Frisch abgestorben",
    tree_marked      = "Permanent markiert",
    damage_other     = "Sonstige Stammschäden",
    damage_resin     = "Harzlachten",
    within_stand     = "Bestandeszugehörigkeit",
    stand_layer = "Bestandesschicht",
    damage_beetle    = "Käferbohrlöcher",
    damage_fungus    = "Pilz",
    biotope_marked   = "Markierter Biotopbaum",
    damage_logging   = "Rücke- und Fällschäden",
    crown_dead_wood  = "Lichtkrone abgestorben / starke Totäste",
    damage_peel_new  = "Schälschäden frisch",
    damage_peel_old  = "Schälschäden alt",
    tree_top_drought = "Zopftrocknis",
    stem_form        = "Höhen-KZ"
  )
  
  # Deutsche Beschriftung ermitteln.
  # Nicht zugeordnete Felder behalten ihren internen Namen.
  wzp4_beschriftung <- function(typ) {
    beschriftung <- unname(wzp4_beschriftungen[typ])
    
    dplyr::coalesce(beschriftung, typ)
  }
  
  # Dokumentierte Boolean-Felder.
  # Astung und Stammform bleiben numerische Codes.
  wzp4_boolean_felder <- c(
    "cave_tree",
    "bark_pocket",
    "damage_dead",
    "tree_marked",
    "damage_other",
    "damage_resin",
    "within_stand",
    "damage_beetle",
    "damage_fungus",
    "biotope_marked",
    "damage_logging",
    "crown_dead_wood",
    "damage_peel_new",
    "damage_peel_old",
    "tree_top_drought"
  )
  
  # Boolean-Werte ausschließlich für die Anzeige in Ja/Nein umwandeln.
  wzp4_anzeigewert <- function(wert, typ) {
    text <- as.character(wert)
    normalisiert <- tolower(trimws(text))
    ist_boolean <- typ %in% wzp4_boolean_felder
    
    dplyr::case_when(
      is.na(wert) ~ NA_character_,
      
      ist_boolean &
        normalisiert %in% c("true", "t", "1", "ja") ~ "Ja",
      
      ist_boolean &
        normalisiert %in% c("false", "f", "0", "nein") ~ "Nein",
      
      TRUE ~ text
    )
  }
  
  # ============================================================================
  # WZP4: Baeume ueber tree_number statt ueber JSON-Listenposition zuordnen
  # ============================================================================

  prepare_tree_side <- function(flat_tbl, seite) {
    werte <- flat_tbl %>%
      filter(str_detect(Field, "^tree\\[[0-9]+\\]\\.[^.]+$")) %>%
      transmute(
        Index = str_extract(Field, "(?<=\\[)[0-9]+(?=\\])"),
        Typ = sub("^.*\\]\\.", "", Field),
        Wert = as.character(Wert)
      )
    if (anyDuplicated(werte[c("Index", "Typ")])) {
      stop("Doppelte Baumfelder im ", seite, "-Stand.")
    }
    nummern <- werte %>%
      filter(Typ == "tree_number") %>%
      transmute(Index, Baumnummer = trimws(Wert)) %>%
      mutate(Baumnummer = na_if(Baumnummer, ""))
    stamm <- werte %>% distinct(Index) %>%
      left_join(nummern, by = "Index") %>%
      mutate(
        # Ohne Baumnummer nie anhand derselben Listenposition zusammenfuehren.
        Baum_ID = if_else(
          is.na(Baumnummer),
          paste0("ohne_Nummer_", seite, "_", Index),
          paste0("Nummer_", Baumnummer)
        ),
        Baum_Label = coalesce(
          Baumnummer, paste0("ohne Nummer (", seite, "-Index ", Index, ")")
        ),
        Vorhanden = TRUE
      )
    if (anyDuplicated(stamm$Baum_ID)) {
      stop("Mehrdeutige Baumnummer im ", seite, "-Stand; Zuordnung abgebrochen.")
    }
    pk <- werte %>% filter(Typ == "tree_status") %>%
      transmute(Index, PK = Wert)
    stamm <- stamm %>% left_join(pk, by = "Index")
    list(
      stamm = stamm %>% select(-Index, -Baumnummer),
      werte = werte %>% left_join(stamm %>% select(Index, Baum_ID), by = "Index") %>%
        select(Baum_ID, Typ, Wert)
    )
  }

  tree_kt <- prepare_tree_side(aktuell_tbl, "Alt")
  tree_at <- prepare_tree_side(historie_tbl, "Neu")
  baum_kontext <- full_join(
    tree_kt$stamm, tree_at$stamm, by = "Baum_ID", suffix = c("_KT", "_AT")
  ) %>% mutate(
    Baumnummer = coalesce(Baum_Label_KT, Baum_Label_AT),
    Vorhanden_KT = coalesce(Vorhanden_KT, FALSE),
    Vorhanden_AT = coalesce(Vorhanden_AT, FALSE)
  )
  tree_vergleich <- full_join(
    tree_kt$werte %>% rename(Aktuell = Wert),
    tree_at$werte %>% rename(Historie = Wert),
    by = c("Baum_ID", "Typ")
  ) %>%
    left_join(baum_kontext, by = "Baum_ID") %>%
    mutate(
      Aktuell_num = suppressWarnings(as.numeric(Aktuell)),
      Historie_num = suppressWarnings(as.numeric(Historie))
    )

  # Einseitig vorhandene Baeume einmal mit ihren Kerndaten ausgeben.
  # Die Gegenseite als fehlenden Baum kennzeichnen, nicht als Reihe von NA-Feldern.
  baum_kerndaten <- function(baum_id, seite) {
    daten <- if (seite == "KT") tree_kt$werte else tree_at$werte
    daten <- daten %>% filter(Baum_ID == baum_id, Typ %in% c("dbh", "distance", "tree_height"))
    daten <- daten %>% filter(!is.na(Wert), nzchar(trimws(Wert)))
    if (nrow(daten) == 0L) return("")
    daten <- daten %>% arrange(match(Typ, c("dbh", "distance", "tree_height")))
    paste0("; ", paste0(wzp4_beschriftung(daten$Typ), ": ", daten$Wert, collapse = "; "))
  }

  wzp4_export <- (function() {
    einseitig <- baum_kontext %>%
      filter(Vorhanden_KT != Vorhanden_AT)
    fehlende_baeume <- einseitig %>%
      mutate(
        Maske = "WZP4",
        Wert_KT = if_else(
          Vorhanden_KT,
          paste0("BaumNr ", Baumnummer, "; PK: ", coalesce(PK_KT, "nicht angegeben"),
                 purrr::map_chr(Baum_ID, ~ baum_kerndaten(.x, "KT"))),
          paste0("BaumNr ", Baumnummer, "; im ersten AT-Stand nicht vorhanden")
        ),
        Wert_AT = if_else(
          Vorhanden_AT,
          paste0("BaumNr ", Baumnummer, "; PK: ", coalesce(PK_AT, "nicht angegeben"),
                 purrr::map_chr(Baum_ID, ~ baum_kerndaten(.x, "AT"))),
          paste0("BaumNr ", Baumnummer, "; im letzten AT-Stand nicht vorhanden")
        ),
        Unterschiede = if_else(Vorhanden_AT, "Nur im letzten AT-Stand vorhanden", "Nur im ersten AT-Stand vorhanden"),
        Bemerkungen = "Baum fehlt in der anderen Aufnahme"
      ) %>% select(Maske, Wert_KT, Wert_AT, Unterschiede, Bemerkungen)

    tree <- tree_vergleich %>%
      filter(Vorhanden_KT, Vorhanden_AT,
             Typ %in% c("dbh", "tree_height", "distance", "tree_status"))
    dbh_grenzen <- tree_vergleich %>% filter(Typ == "dbh") %>%
      transmute(Baum_ID, Distance_Grenze = Aktuell_num / 2 / 10)
    ergebnis <- tree %>%
      left_join(dbh_grenzen, by = "Baum_ID") %>%
      mutate(
        Differenz = abs(coalesce(Aktuell_num, 0) - coalesce(Historie_num, 0)),
        OK = case_when(
          is.na(Aktuell) & is.na(Historie) ~ TRUE,
          is.na(Aktuell) | is.na(Historie) ~ FALSE,
          Typ == "dbh" ~ Differenz < 3,
          Typ == "tree_height" ~ Differenz < 20,
          Typ == "tree_status" ~ Aktuell == Historie,
          Typ == "distance" ~ Differenz <= coalesce(Distance_Grenze, 0),
          TRUE ~ NA
        )
      ) %>% filter(!OK | is.na(OK))
    abweichungen <- ergebnis %>%
      mutate(
        Maske = "WZP4",
        Wert_KT = paste0(
          "BaumNr ", Baumnummer, "; PK: ", coalesce(PK_KT, "nicht angegeben"),
          if_else(Typ == "tree_status", "", paste0(
            "; ", wzp4_beschriftung(Typ), ": ", coalesce(Aktuell, "nicht angegeben")
          ))
        ),
        Wert_AT = paste0(
          "BaumNr ", Baumnummer, "; PK: ", coalesce(PK_AT, "nicht angegeben"),
          if_else(Typ == "tree_status", "", paste0(
            "; ", wzp4_beschriftung(Typ), ": ", coalesce(Historie, "nicht angegeben")
          ))
        ),
        Unterschiede = case_when(
          Typ == "tree_status" ~ "PK abweichend",
          is.na(Aktuell_num) | is.na(Historie_num) ~ "Wert fehlt oder ist nicht numerisch",
          TRUE ~ as.character(Historie_num - Aktuell_num)
        ),
        Bemerkungen = if_else(
          Typ == "distance" & !is.na(Distance_Grenze),
          paste0("Grenzwert: ", round(Distance_Grenze, 2)), ""
        )
      ) %>% select(Maske, Wert_KT, Wert_AT, Unterschiede, Bemerkungen)
    ergebnis_export <- bind_rows(abweichungen, fehlende_baeume)
    if (nrow(ergebnis_export) == 0L) return(NULL)
    ergebnis_export$Maske[-1] <- ""
    ergebnis_export
  })()

  # ============================================================================
  # WZP4 Eigenschaften: dieselbe Baumnummer-Zuordnung wie fuer Messwerte und PK
  # ============================================================================
  wzp4_flags_export <- (function() {
    flags <- c(
      "pruning", "cave_tree", "deprecated", "bark_pocket", "damage_dead",
      "tree_marked", "damage_other", "damage_resin", "within_stand",
      "stand_layer", "damage_beetle", "damage_fungus", "biotope_marked",
      "damage_logging", "crown_dead_wood", "damage_peel_new", "damage_peel_old",
      "tree_top_drought", "stem_form", "dbh_height"
    )
    flag_tbl <- tree_vergleich %>%
      # Einseitige Baeume sind bereits in WZP4 vollstaendig gekennzeichnet.
      filter(Vorhanden_KT, Vorhanden_AT, Typ %in% flags) %>%
      mutate(OK = case_when(
        is.na(Aktuell) & is.na(Historie) ~ TRUE,
        is.na(Aktuell) | is.na(Historie) ~ FALSE,
        TRUE ~ Aktuell == Historie
      )) %>% filter(!OK)
    if (nrow(flag_tbl) == 0L) return(NULL)
    flag_export <- flag_tbl %>% mutate(
      Maske = "WZP4 Eigenschaften",
      Wert_KT = paste0(
        "BaumNr ", Baumnummer, "; PK: ", coalesce(PK_KT, "nicht angegeben"),
        "; ", wzp4_beschriftung(Typ), ": ",
        coalesce(wzp4_anzeigewert(Aktuell, Typ), "nicht angegeben")
      ),
      Wert_AT = paste0(
        "BaumNr ", Baumnummer, "; PK: ", coalesce(PK_AT, "nicht angegeben"),
        "; ", wzp4_beschriftung(Typ), ": ",
        coalesce(wzp4_anzeigewert(Historie, Typ), "nicht angegeben")
      ),
      Unterschiede = "abweichend"
    ) %>% select(Maske, Wert_KT, Wert_AT, Unterschiede)
    flag_export$Maske[-1] <- ""
    flag_export
  })()

  # Verjüngung Vergleich ----------------------------------------------------
  
  verjuengung_export <- safe_block("Verjuengung", function() {
    
    # Verjüngungsdaten aus regeneration
    
    regen <- Vergleich_zwei %>%
      filter(str_detect(Field, "^regeneration\\["))
    
    # stand_layer_regeneration separat aus properties
    
    stand_layer <- Vergleich_zwei %>%
      filter(Field == "stand_layer_regeneration")
    
    # subplots_relative_positions separat aus properties
    
    relative_position <- Vergleich_zwei %>%
      filter(str_detect(
        Field,
        "subplots_relative_position.*\\.(radius|azimuth|distance)$"
      ))
    
    # Wenn überhaupt keine Daten vorhanden sind
    if (nrow(regen) == 0 &&
        nrow(stand_layer) == 0 &&
        nrow(relative_position) == 0) {
      return(NULL)
    }
    
    verjuengung_regen <- NULL
    
    if (nrow(regen) > 0) {
      
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
      
      aktuell_tbl2 <- aktuell_tbl %>%
        select(
          Größenklasse = tree_size_class,
          Baumart = tree_species,
          Verbiss = browsing,
          Schälschaden = damage_peel,
          Anzahl_KT = tree_count
        )
      
      historie_tbl2 <- historie_tbl %>%
        select(
          Größenklasse = tree_size_class,
          Baumart = tree_species,
          Verbiss = browsing,
          Schälschaden = damage_peel,
          Anzahl_AT = tree_count
        )
      
      join_cols <- c("Größenklasse", "Baumart")
      
      if ("Verbiss" %in% names(aktuell_tbl2) &&
          ("Verbiss" %in% names(historie_tbl2)) &&
          (any(!is.na(aktuell_tbl2$Verbiss)) ||
           any(!is.na(historie_tbl2$Verbiss)))) {
        join_cols <- c(join_cols, "Verbiss")
      }
      
      if ("Schälschaden" %in% names(aktuell_tbl2) &&
          ("Schälschaden" %in% names(historie_tbl2)) &&
          (any(!is.na(aktuell_tbl2$Schälschaden)) ||
           any(!is.na(historie_tbl2$Schälschaden)))) {
        join_cols <- c(join_cols, "Schälschaden")
      }
      
      vergleich <- full_join(
        aktuell_tbl2,
        historie_tbl2,
        by = join_cols
      ) %>%
        mutate(
          Differenz = abs(
            coalesce(as.numeric(Anzahl_KT), 0) -
              coalesce(as.numeric(Anzahl_AT), 0)
          )
        )%>%
        arrange(as.numeric(Größenklasse))
      
      vergleich <- vergleich %>%
        filter(!(is.na(Größenklasse) & is.na(Baumart)))
      
      if (nrow(vergleich) == 0) {
        return(NULL)
      }
      
      verjuengung_regen <- vergleich %>%
        mutate(
          Maske = "Verjuengung",
          Wert_KT = paste0(
            "GK: ", Größenklasse,
            "; Baumart: ", Baumart,
            if ("Verbiss" %in% names(.))
              paste0("; Verbiss: ", Verbiss) else "",
            if ("Schälschaden" %in% names(.))
              paste0("; Schälschaden: ", Schälschaden) else "",
            "; Anzahl: ", Anzahl_KT
          ),
          Wert_AT = paste0(
            "GK: ", Größenklasse,
            "; Baumart: ", Baumart,
            if ("Verbiss" %in% names(.))
              paste0("; Verbiss: ", Verbiss) else "",
            if ("Schälschaden" %in% names(.))
              paste0("; Schälschaden: ", Schälschaden) else "",
            "; Anzahl: ", Anzahl_AT
          ),
          Unterschiede = as.character(export_differenz(Anzahl_AT, Anzahl_KT))
        ) %>%
        select(Maske, Wert_KT, Wert_AT, Unterschiede)
    }
    
    # stand_layer_regeneration vergleichen
    
    verjuengung_layer <- NULL
    
    if (nrow(stand_layer) > 0) {
      
      layer_kt <- stand_layer$Aktuell[[1]]
      layer_at <- stand_layer$Historie[[1]]
      
      # Codes entschlüsseln
      layer_text <- function(x) {
        case_when(
          as.character(x) == "0" ~ "keine Zuordnung",
          as.character(x) == "1" ~ "Hauptbestand",
          as.character(x) == "2" ~ "Unterstand",
          as.character(x) == "4" ~ "Verjüngung unter Schirm",
          is.na(x) ~ "nicht angegeben",
          TRUE ~ as.character(x)
        )
      }
      
      layer_kt_text <- layer_text(layer_kt)
      layer_at_text <- layer_text(layer_at)
      
      # Nur bei Änderung ausgeben
      if (!identical(
        as.character(layer_kt),
        as.character(layer_at)
      )) {
        
        verjuengung_layer <- tibble(
          Maske = "Verjuengung",
          
          Wert_KT = paste0(
            "Schicht: ",
            layer_kt_text
          ),
          
          Wert_AT = paste0(
            "Schicht: ",
            layer_at_text
          ),
          
          Unterschiede = "Schicht geändert"
        )
      }
    }
    
    #  subplots_relative_position vergleichen
    
    verjuengung_position <- NULL
    
    if (nrow(relative_position) > 0) {
      
      relative_position <- relative_position %>%
        mutate(
          position_variable = str_extract(
            Field,
            "(?<=\\.)[^.]+$"
          )
        ) %>%
        filter(
          position_variable %in% c(
            "radius",
            "azimuth",
            "distance"
          )
        )
      
      if (nrow(relative_position) > 0) {
        
        position_changes <- relative_position %>%
          mutate(
            Aktuell_num = suppressWarnings(as.numeric(Aktuell)),
            Historie_num = suppressWarnings(as.numeric(Historie))
          ) %>%
          filter(
            is.na(Aktuell_num) != is.na(Historie_num) |
              (!is.na(Aktuell_num) &
                 !is.na(Historie_num) &
                 Aktuell_num != Historie_num)
          )
        
        if (nrow(position_changes) > 0) {
          
          verjuengung_position <- position_changes %>%
            mutate(
              Maske = "Verjuengung",
              
              Feldname = case_when(
                position_variable == "radius" ~ "Radius",
                position_variable == "azimuth" ~ "Azimut",
                position_variable == "distance" ~ "Distanz",
                TRUE ~ position_variable
              ),
              
              Wert_KT = paste0(
                Feldname,
                ": ",
                Aktuell
              ),
              
              Wert_AT = paste0(
                Feldname,
                ": ",
                Historie
              ),
              
              Unterschiede = "abweichend"
            ) %>%
            select(
              Maske,
              Wert_KT,
              Wert_AT,
              Unterschiede
            )
        }
      }
    }
    
    # Alles zusammenführen
    
    verjuengung_export <- bind_rows(
      verjuengung_position,
      verjuengung_regen,
      verjuengung_layer
    )
    
    if (nrow(verjuengung_export) == 0) {
      return(NULL)
    }
    
    verjuengung_export$Maske[-1] <- ""
    
    verjuengung_export
  })
  
  # Totholz Vergleich -------------------------------------------------------
  
  safe_row <- function(x) {
    cleaned <- purrr::map(x, function(v) {
      if (is.null(v) || length(v) == 0) {
        return(NA)
      }
      
      if (is.list(v)) {
        return(as.character(v[[1]]))
      }
      
      v
    })
    
    tibble::as_tibble_row(cleaned)
  }
  
  
  # Werte für den Vergleich normalisieren.
  # Dadurch werden z.B. 30, "30" und "30.0" als gleicher Wert behandelt.
  
  normalisiere_totholz_wert <- function(x) {
    
    x <- as.character(x)
    x <- trimws(x)
    
    nummer <- suppressWarnings(as.numeric(x))
    
    ifelse(
      !is.na(nummer),
      format(nummer, scientific = FALSE, trim = TRUE),
      tolower(x)
    )
  }
  
  
  # Leere Seite mit allen benoetigten Spalten, damit einseitig vorhandenes
  # Totholz ebenfalls ueber die Signatur verglichen werden kann.
  leere_totholz_tabelle <- function() {
    tibble(
      dead_wood_type = character(),
      diameter_butt = double(),
      diameter_top = double(),
      length_height = double(),
      totholz_signatur = character(),
      signatur_nummer = integer()
    )
  }
  
  deadwood_export <- safe_block("Totholz", function() {
    
    # ========================================================================
    # HISTORIE
    # ========================================================================
    
    if (is.null(json_historie$deadwood) ||
        length(json_historie$deadwood) == 0) {
      
      dw_at <- leere_totholz_tabelle()
      
    } else {
      
      dw_at <- purrr::map_dfr(
        json_historie$deadwood,
        safe_row
      )
    }
    
    
    # ========================================================================
    # AKTUELL
    # ========================================================================
    
    if (is.null(json_aktuell$deadwood) ||
        length(json_aktuell$deadwood) == 0) {
      
      dw_kt <- leere_totholz_tabelle()
      
    } else {
      
      dw_kt <- purrr::map_dfr(
        json_aktuell$deadwood,
        safe_row
      )
    }
    
    
    # ========================================================================
    # ANZAHL DER TOTHOLZSTÜCKE
    # ========================================================================
    
    anzahl_at <- nrow(dw_at)
    anzahl_kt <- nrow(dw_kt)
    
    # Ohne Totholzstuecke in AT und KT keine Totholz-Maske ausgeben.
    # Einseitig vorhandene Stuecke bleiben fuer den Vergleich sichtbar.
    if (anzahl_at == 0 && anzahl_kt == 0) {
      return(NULL)
    }
    
    # Anzahl anzeigen, sobald mindestens eine Seite Totholzstuecke enthaelt.
    # Dadurch sieht man auch, wenn die Anzahl unveraendert ist.
    
    anzahl_export <- tibble(
      Maske = "Totholz",
      
      Wert_KT = paste0(
        "Anzahl Totholzstücke: ",
        anzahl_kt
      ),
      
      Wert_AT = paste0(
        "Anzahl Totholzstücke: ",
        anzahl_at
      ),
      
      Unterschiede = as.character(
        export_differenz(anzahl_at, anzahl_kt)
      ),
      
      Bemerkungen = ifelse(
        anzahl_kt == anzahl_at,
        "",
        "Anzahl Totholzstücke geändert"
      )
    )
    
    
    
    
    # ========================================================================
    # TOTHOLZ-SIGNATUR ERSTELLEN
    # ========================================================================
    #
    # Ein Totholzstück wird eindeutig beschrieben durch:
    #
    #   1. Totholztyp
    #   2. Durchmesser max
    #   3. Durchmesser min
    #   4. Länge
    #
    # Beispiel:
    #
    # 4 | 30 | NA | 1
    #
    # Zwei Stücke mit exakt dieser Signatur gelten als dasselbe Stück.
    #
    # Die occurrence-Nummer sorgt dafür, dass auch mehrere identische
    # Totholzstücke korrekt 1:1 zugeordnet werden.
    # ========================================================================
    
    if (nrow(dw_at) > 0) {
      
      dw_at <- dw_at %>%
        mutate(
          vergleich_typ =
            normalisiere_totholz_wert(dead_wood_type),
          
          vergleich_butt =
            normalisiere_totholz_wert(diameter_butt),
          
          vergleich_top =
            normalisiere_totholz_wert(diameter_top),
          
          vergleich_laenge =
            normalisiere_totholz_wert(length_height)
        ) %>%
        mutate(
          totholz_signatur = paste(
            vergleich_typ,
            vergleich_butt,
            vergleich_top,
            vergleich_laenge,
            sep = "|"
          )
        ) %>%
        group_by(totholz_signatur) %>%
        mutate(
          signatur_nummer = row_number()
        ) %>%
        ungroup()
    }
    
    
    if (nrow(dw_kt) > 0) {
      
      dw_kt <- dw_kt %>%
        mutate(
          vergleich_typ =
            normalisiere_totholz_wert(dead_wood_type),
          
          vergleich_butt =
            normalisiere_totholz_wert(diameter_butt),
          
          vergleich_top =
            normalisiere_totholz_wert(diameter_top),
          
          vergleich_laenge =
            normalisiere_totholz_wert(length_height)
        ) %>%
        mutate(
          totholz_signatur = paste(
            vergleich_typ,
            vergleich_butt,
            vergleich_top,
            vergleich_laenge,
            sep = "|"
          )
        ) %>%
        group_by(totholz_signatur) %>%
        mutate(
          signatur_nummer = row_number()
        ) %>%
        ungroup()
    }
    
    
    # ========================================================================
    # IDENTISCHE STÜCKE AUSSONDERN
    # ========================================================================
    #
    # Ein Stück ist identisch, wenn:
    #
    #   totholz_signatur + signatur_nummer
    #
    # in beiden Datensätzen vorhanden ist.
    #
    # Diese Stücke werden NICHT ausgegeben.
    # ========================================================================
    
    if (nrow(dw_at) > 0 && nrow(dw_kt) > 0) {
      
      gemeinsame_stuecke <- inner_join(
        dw_at %>%
          select(
            totholz_signatur,
            signatur_nummer
          ),
        
        dw_kt %>%
          select(
            totholz_signatur,
            signatur_nummer
          ),
        
        by = c(
          "totholz_signatur",
          "signatur_nummer"
        )
      ) %>%
        distinct(
          totholz_signatur,
          signatur_nummer
        )
      
    } else {
      
      gemeinsame_stuecke <- tibble(
        totholz_signatur = character(),
        signatur_nummer = integer()
      )
    }
    
    # ========================================================================
    # IDENTISCHE TOTHOLZSTÜCKE FÜR DEN EXPORT
    # ========================================================================
    
    identisch_export <- tibble()
    
    if (nrow(gemeinsame_stuecke) > 0) {
      
      identisch_export <- dw_kt %>%
        inner_join(
          gemeinsame_stuecke,
          by = c(
            "totholz_signatur",
            "signatur_nummer"
          )
        ) %>%
        transmute(
          
          Maske = "Totholz",
          
          Wert_KT = paste0(
            "Typ: ", dead_wood_type,
            "; Max: ", diameter_butt,
            "; Min: ", diameter_top,
            "; Laenge: ", length_height
          ),
          
          Wert_AT = paste0(
            "Typ: ", dead_wood_type,
            "; Max: ", diameter_butt,
            "; Min: ", diameter_top,
            "; Laenge: ", length_height
          ),
          
          Unterschiede = "identisch",
          
          Bemerkungen = "Totholzstück unverändert vorhanden"
        )
    }
    
    
    # ========================================================================
    # NUR AT-STÜCKE, DIE KEIN GEGENSTÜCK IN KT HABEN
    # ========================================================================
    
    nicht_in_kt <- dw_at %>%
      anti_join(
        gemeinsame_stuecke,
        by = c(
          "totholz_signatur",
          "signatur_nummer"
        )
      )
    
    
    # ========================================================================
    # NUR KT-STÜCKE, DIE KEIN GEGENSTÜCK IN AT HABEN
    # ========================================================================
    
    nicht_in_at <- dw_kt %>%
      anti_join(
        gemeinsame_stuecke,
        by = c(
          "totholz_signatur",
          "signatur_nummer"
        )
      )
    
    
    # ========================================================================
    # AT-STÜCKE AUSGEBEN, DIE NICHT MEHR IN KT VORHANDEN SIND
    # ========================================================================
    
    at_export <- tibble()
    
    if (nrow(nicht_in_kt) > 0) {
      
      at_export <- nicht_in_kt %>%
        transmute(
          
          Maske = "Totholz",
          
          Wert_KT = "Merkmalskombination im ersten AT-Stand nicht vorhanden",
          
          Wert_AT = paste0(
            "Typ: ", dead_wood_type,
            "; Max: ", diameter_butt,
            "; Min: ", diameter_top,
            "; Laenge: ", length_height
          ),
          
          Unterschiede = "Nur im letzten AT-Stand vorhanden",
          
          Bemerkungen =
            "Merkmalskombination fehlt im ersten AT-Stand"
        )
    }
    
    
    # ========================================================================
    # KT-STÜCKE AUSGEBEN, DIE NEU HINZUGEKOMMEN SIND
    # ========================================================================
    
    kt_export <- tibble()
    
    if (nrow(nicht_in_at) > 0) {
      
      kt_export <- nicht_in_at %>%
        transmute(
          
          Maske = "Totholz",
          
          Wert_KT = paste0(
            "Typ: ", dead_wood_type,
            "; Max: ", diameter_butt,
            "; Min: ", diameter_top,
            "; Laenge: ", length_height
          ),
          
          Wert_AT = "Merkmalskombination im letzten AT-Stand nicht vorhanden",
          
          Unterschiede = "Nur im ersten AT-Stand vorhanden",
          
          Bemerkungen =
            "Merkmalskombination fehlt im letzten AT-Stand"
        )
    }
    
    
    # ========================================================================
    # ALLES ZUSAMMENFÜHREN
    # ========================================================================
    
    deadwood_export <- bind_rows(
      anzahl_export,
      identisch_export,
      at_export,
      kt_export
    )
    
    
    if (nrow(deadwood_export) == 0) {
      return(NULL)
    }
    
    
    # "Totholz" nur in der ersten Zeile anzeigen
    
    deadwood_export$Maske[-1] <- ""
    
    
    deadwood_export
  })
  
  # Ränder Vergleich ---------------------------------------------------------
  
  raender_export <- safe_block("Raender", function() {
    
    rand_felder <- Vergleich_zwei %>%
      filter(
        str_detect(
          Field,
          "^edges\\[.*\\]\\.(edge_number|edge_type|edge_stand_difference)$"
        )
      ) %>%
      mutate(
        rand_index = str_extract(
          Field,
          "(?<=^edges\\[)\\d+(?=\\]\\.)"
        ),
        variable = sub(
          "^edges\\[.*\\]\\.",
          "",
          Field
        )
      )
    
    rand_positionen <- Vergleich_zwei %>%
      filter(
        str_detect(
          Field,
          "^edges\\[.*\\]\\.edges\\[.*\\]\\.(azimuth|distance)$"
        )
      ) %>%
      mutate(
        rand_index = str_extract(
          Field,
          "(?<=^edges\\[)\\d+(?=\\]\\.edges)"
        ),
        position_index = str_extract(
          Field,
          "(?<=\\.edges\\[)\\d+(?=\\]\\.)"
        ),
        variable = sub(
          "^edges\\[.*\\]\\.edges\\[.*\\]\\.",
          "",
          Field
        )
      )
    
    if (nrow(rand_felder) == 0 &&
        nrow(rand_positionen) == 0) {
      return(NULL)
    }
    
    rand_kt <- rand_felder %>%
      select(
        rand_index,
        variable,
        Wert = Aktuell
      ) %>%
      pivot_wider(
        names_from = variable,
        values_from = Wert
      )
    
    
    rand_at <- rand_felder %>%
      select(
        rand_index,
        variable,
        Wert = Historie
      ) %>%
      pivot_wider(
        names_from = variable,
        values_from = Wert
      )
    
    position_kt <- rand_positionen %>%
      select(
        rand_index,
        position_index,
        variable,
        Wert = Aktuell
      ) %>%
      pivot_wider(
        names_from = variable,
        values_from = Wert
      )
    
    position_at <- rand_positionen %>%
      select(
        rand_index,
        position_index,
        variable,
        Wert = Historie
      ) %>%
      pivot_wider(
        names_from = variable,
        values_from = Wert
      )
    
    benoetigte_rand_spalten <- c(
      "edge_number",
      "edge_type",
      "edge_stand_difference"
    )
    
    for (spalte in benoetigte_rand_spalten) {
      
      if (!spalte %in% names(rand_kt)) {
        rand_kt[[spalte]] <- NA_character_
      }
      
      if (!spalte %in% names(rand_at)) {
        rand_at[[spalte]] <- NA_character_
      }
    }
    
    rand_vergleich <- full_join(
      rand_kt,
      rand_at,
      by = "edge_number",
      suffix = c("_KT", "_AT")
    )
    
    if (nrow(rand_vergleich) == 0) {
      return(NULL)
    }
    
    # ------------------------------------------------------------------------
    # Randnummer als Schlüssel bereinigen
    # ------------------------------------------------------------------------
    
    rand_vergleich <- rand_vergleich %>%
      mutate(
        edge_number = as.character(edge_number)
      )
    
    # ------------------------------------------------------------------------
    # Vergleich von edge_type und edge_stand_difference
    # ------------------------------------------------------------------------
    
    text_unterschied <- function(kt, at) {
      
      kt <- ifelse(
        is.na(kt),
        NA_character_,
        trimws(as.character(kt))
      )
      
      at <- ifelse(
        is.na(at),
        NA_character_,
        trimws(as.character(at))
      )
      
      is.na(kt) != is.na(at) |
        (
          !is.na(kt) &
            !is.na(at) &
            kt != at
        )
    }
    
    rand_vergleich <- rand_vergleich %>%
      mutate(
        edge_type_geaendert = text_unterschied(
          edge_type_KT,
          edge_type_AT
        ),
        
        edge_stand_difference_geaendert = text_unterschied(
          edge_stand_difference_KT,
          edge_stand_difference_AT
        )
      )
    
    position_vergleich <- full_join(
      position_kt,
      position_at,
      by = c(
        "rand_index" = "rand_index",
        "position_index" = "position_index"
      ),
      suffix = c("_KT", "_AT")
    )
    
    position_kt2 <- position_kt %>%
      left_join(
        rand_kt %>%
          select(
            rand_index,
            edge_number
          ),
        by = "rand_index"
      )
    
    position_at2 <- position_at %>%
      left_join(
        rand_at %>%
          select(
            rand_index,
            edge_number
          ),
        by = "rand_index"
      )
    
    position_vergleich <- full_join(
      position_kt2,
      position_at2,
      by = c(
        "edge_number",
        "position_index"
      ),
      suffix = c("_KT", "_AT")
    )
    
    # ------------------------------------------------------------------------
    # Nur tatsächlich geänderte Randpositionen behalten
    # ------------------------------------------------------------------------
    
    position_vergleich <- position_vergleich %>%
      mutate(
        azimuth_KT_num = suppressWarnings(
          as.numeric(azimuth_KT)
        ),
        azimuth_AT_num = suppressWarnings(
          as.numeric(azimuth_AT)
        ),
        distance_KT_num = suppressWarnings(
          as.numeric(distance_KT)
        ),
        distance_AT_num = suppressWarnings(
          as.numeric(distance_AT)
        ),
        
        azimuth_geaendert =
          is.na(azimuth_KT_num) != is.na(azimuth_AT_num) |
          (
            !is.na(azimuth_KT_num) &
              !is.na(azimuth_AT_num) &
              azimuth_KT_num != azimuth_AT_num
          ),
        
        distance_geaendert =
          is.na(distance_KT_num) != is.na(distance_AT_num) |
          (
            !is.na(distance_KT_num) &
              !is.na(distance_AT_num) &
              distance_KT_num != distance_AT_num
          )
      ) %>%
      filter(
        azimuth_geaendert |
          distance_geaendert
      )
    
    
    # ------------------------------------------------------------------------
    # Export für edge_type / edge_stand_difference
    # ------------------------------------------------------------------------
    
    rand_stammdaten_export <- rand_vergleich %>%
      filter(
        edge_type_geaendert |
          edge_stand_difference_geaendert
      ) %>%
      transmute(
        Maske = "Ränder",
        
        Wert_KT = paste0(
          "RandNr: ", edge_number,
          "; Typ: ",
          coalesce(
            as.character(edge_type_KT),
            "nicht angegeben"
          ),
          "; vorgelagerter Bestand: ",
          coalesce(
            as.character(edge_stand_difference_KT),
            "nicht angegeben"
          )
        ),
        
        Wert_AT = paste0(
          "RandNr: ", edge_number,
          "; Typ: ",
          coalesce(
            as.character(edge_type_AT),
            "NA"
          ),
          "; vorgelagerter Bestand: ",
          coalesce(
            as.character(edge_stand_difference_AT),
            "NA"
          )
        ),
        
        Unterschiede = paste(
          c(
            ifelse(
              edge_type_geaendert,
              "Typ geändert",
              ""
            ),
            ifelse(
              edge_stand_difference_geaendert,
              "vorgelagerter Bestand geändert",
              ""
            )
          ),
          collapse = "; "
        )
      )
    
    # ------------------------------------------------------------------------
    # Export für Azimut / Distanz
    # ------------------------------------------------------------------------
    
    rand_position_export <- position_vergleich %>%
      transmute(
        Maske = "Ränder",
        
        Wert_KT = paste0(
          "RandNr: ", edge_number,
          "; Position: ", position_index,
          "; Azimut: ",
          coalesce(
            as.character(azimuth_KT),
            "NA"
          ),
          "; Distanz: ",
          coalesce(
            as.character(distance_KT),
            "NA"
          )
        ),
        
        Wert_AT = paste0(
          "RandNr: ", edge_number,
          "; Position: ", position_index,
          "; Azimut: ",
          coalesce(
            as.character(azimuth_AT),
            "NA"
          ),
          "; Distanz: ",
          coalesce(
            as.character(distance_AT),
            "NA"
          )
        ),
        
        Unterschiede = paste(
          c(
            ifelse(
              azimuth_geaendert,
              "azimut geändert",
              ""
            ),
            ifelse(
              distance_geaendert,
              "distanz geändert",
              ""
            )
          ),
          collapse = "; "
        )
      )
    
    # ------------------------------------------------------------------------
    # Zusammenführen
    # ------------------------------------------------------------------------
    
    raender_export <- bind_rows(
      rand_stammdaten_export,
      rand_position_export
    )
    
    if (nrow(raender_export) == 0) {
      return(NULL)
    }
    
    # "Ränder" nur in der ersten Zeile anzeigen
    raender_export$Maske[-1] <- ""
    
    raender_export
  })
  
  # Bestockung kleiner 4m Vergleich ----------------------------------------------------
  
  bestockung_export <- safe_block("Bestockung <4m", function() {
    
    #  Baumarten aus structure_lt4m
    
    bst <- Vergleich_zwei %>%
      filter(str_detect(Field, "^structure_lt4m\\["))
    
    #  trees_less_4meter_coverage separat aus properties holen
    
    coverage_lt4m <- Vergleich_zwei %>%
      filter(Field == "trees_less_4meter_coverage")
    
    layer_lt4m <- Vergleich_zwei %>%
      filter(Field == "trees_less_4meter_layer")
    
    if (nrow(bst) == 0 && nrow(coverage_lt4m) == 0&&
        nrow(layer_lt4m) == 0) {
      return(NULL)
    }
    
    #  Vergleich der Baumarten
    
    bestockung_baumarten <- NULL
    
    if (nrow(bst) > 0) {
      
      bst <- bst %>%
        mutate(
          id = str_extract(Field, "(?<=\\[)\\d+(?=\\])"),
          variable = sub("^.*\\]\\.", "", Field)
        )
      
      aktuell_tbl <- bst %>%
        select(id, variable, value = Aktuell) %>%
        pivot_wider(
          names_from = variable,
          values_from = value
        )
      
      historie_tbl <- bst %>%
        select(id, variable, value = Historie) %>%
        pivot_wider(
          names_from = variable,
          values_from = value
        )
      
      if ("tree_species" %in% names(aktuell_tbl) ||
          "tree_species" %in% names(historie_tbl)) {
        
        bestockung_baumarten <- full_join(
          aktuell_tbl,
          historie_tbl,
          by = "tree_species"
        ) %>%
          filter(
            !(
              is.na(tree_species) &
                is.na(coverage.x) &
                is.na(coverage.y) 
            )
          ) %>%
          mutate(
            Differenz = abs(
              coalesce(as.numeric(coverage.x), 0) -
                coalesce(as.numeric(coverage.y), 0)
            )
          )
        
        if (nrow(bestockung_baumarten) > 0) {
          
          bestockung_baumarten <- bestockung_baumarten %>%
            mutate(
              Maske = "Bestockung <4m",
              Wert_KT = paste0(
                "Baumart: ", tree_species,
                "; Anteil: ", coverage.x
              ),
              Wert_AT = paste0(
                "Baumart: ", tree_species,
                "; Anteil: ", coverage.y
              ),
              Unterschiede = as.character(export_differenz(coverage.y, coverage.x))
            ) %>%
            select(
              Maske,
              Wert_KT,
              Wert_AT,
              Unterschiede
            )
        }
      }
    }
    
    #  trees_less_4meter_coverage separat vergleichen
    
    bestockung_coverage <- NULL
    
    if (nrow(coverage_lt4m) > 0) {
      
      differenz_coverage <- abs(
        coalesce(
          suppressWarnings(as.numeric(coverage_lt4m$Aktuell)),
          0
        ) -
          coalesce(
            suppressWarnings(as.numeric(coverage_lt4m$Historie)),
            0
          )
      )
      
      # Allgemeinen Deckungsgrad immer vor den Baumartenanteilen anzeigen,
      # auch wenn KT und AT denselben Wert haben.
      bestockung_coverage <- tibble(
        Maske = "Bestockung <4m",
        Wert_KT = paste0(
          "Deckungsgrad: ",
          coalesce(as.character(coverage_lt4m$Aktuell), "nicht angegeben")
        ),
        Wert_AT = paste0(
          "Deckungsgrad: ",
          coalesce(as.character(coverage_lt4m$Historie), "nicht angegeben")
        ),
        Unterschiede = as.character(export_differenz(
          coverage_lt4m$Historie,
          coverage_lt4m$Aktuell
        ))
      )
    }
    
    #  trees_less_4meter_layer separat vergleichen
    
    bestockung_layer <- NULL
    
    if (nrow(layer_lt4m) > 0) {
      
      # Code des Layers entschlüsseln
      layer_text <- function(x) {
        case_when(
          as.character(x) == "1" ~ "Hauptbestockung",
          as.character(x) == "2" ~ "Verjüngung",
          is.na(x) ~ "nicht angegeben",
          TRUE ~ as.character(x)
        )
      }
      
      layer_kt <- layer_text(layer_lt4m$Aktuell)
      layer_at <- layer_text(layer_lt4m$Historie)
      
      # Prüfen, ob sich der Layer geändert hat
      # NA gegen NA ist gleich; NA gegen einen vorhandenen Wert ist anders.
      # Anders als != liefert identical() immer TRUE oder FALSE.
      differenz_layer <- !identical(
        as.character(layer_lt4m$Aktuell),
        as.character(layer_lt4m$Historie)
      )
      
      if (differenz_layer) {
        
        bestockung_layer <- tibble(
          Maske = "Bestockung <4m",
          Wert_KT = paste0(
            "Schicht: ",
            layer_kt
          ),
          Wert_AT = paste0(
            "Schicht: ",
            layer_at
          ),
          Unterschiede = "Schicht geändert"
        )
      }
    }
    
    #  Alles zusammenführen
    
    bestockung_export <- bind_rows(
      bestockung_coverage,
      bestockung_baumarten,
      bestockung_layer
    )
    
    if (nrow(bestockung_export) == 0) {
      return(NULL)
    }
    
    bestockung_export$Maske[-1] <- ""
    
    bestockung_export
  })
  
  # Bestockung größer 4m Vergleich ------------------------------------------
  
  bestockung_gt4m_export <- safe_block("Bestockung >4m", function() {
    bst <- Vergleich_zwei %>%
      filter(str_detect(Field, "^structure_gt4m\\["))
    
    if (nrow(bst) == 0) {
      return(NULL)
    }
    
    bst <- bst %>%
      mutate(
        id = str_extract(Field, "(?<=\\[)\\d+(?=\\])"),
        variable = sub("^.*\\]\\.", "", Field)
      )
    
    aktuell_tbl <- bst %>%
      select(id, variable, value = Aktuell) %>%
      pivot_wider(
        names_from = variable,
        values_from = value
      )
    
    historie_tbl <- bst %>%
      select(id, variable, value = Historie) %>%
      pivot_wider(
        names_from = variable,
        values_from = value
      )
    
    # Sicherheitscheck
    required_cols <- c("tree_species", "is_mirrored", "stock_layer", "count")
    
    if (!all(required_cols %in% names(aktuell_tbl)) &&
        !all(required_cols %in% names(historie_tbl))) {
      return(NULL)
    }
    
    # Vergleich nach Baumart UND is_mirrored
    vergleich <- full_join(
      aktuell_tbl,
      historie_tbl,
      by = c("tree_species", "is_mirrored", "stock_layer"),
      suffix = c("_KT", "_AT")
    ) %>%
      # Eintraege ohne Anzahl auf beiden Seiten entfernen.
      # Einseitig vorhandene Anzahlen und der Wert 0 bleiben erhalten.
      filter(!is.na(count_KT) | !is.na(count_AT)) %>%
      mutate(
        Differenz = abs(
          coalesce(as.numeric(count_KT), 0) -
            coalesce(as.numeric(count_AT), 0)
        )
      )
    
    if (nrow(vergleich) == 0) {
      return(NULL)
    }
    
    bestockung_gt4m_export <- vergleich %>%
      mutate(
        Maske = "Bestockung >4m",
        spiegelung = case_when(
          is.na(is_mirrored) ~ "nicht angegeben",
          tolower(as.character(is_mirrored)) %in% c("true", "1", "ja") ~ "Ja",
          tolower(as.character(is_mirrored)) %in% c("false", "0", "nein") ~ "Nein",
          TRUE ~ as.character(is_mirrored)
        ),
        bestockungsschicht = case_when(
          stock_layer == 1 ~ "Hauptbestockung",
          stock_layer == 3 ~ "Restbestockung",
          stock_layer == 9 ~ "10m-Kreis",
          is.na(stock_layer) ~ "nicht angegeben",
          TRUE ~ paste0("stock_layer ", stock_layer)
        ),
        Wert_KT = paste0(
          "Baumart: ", tree_species,
          "; gespiegelt: ", spiegelung,
          "; Bestockungsschicht: ", bestockungsschicht,
          "; Anzahl: ", count_KT
        ),
        Wert_AT = paste0(
          "Baumart: ", tree_species,
          "; gespiegelt: ", spiegelung,
          "; Bestockungsschicht: ", bestockungsschicht,
          "; Anzahl: ", count_AT
        ),
        Unterschiede = as.character(export_differenz(count_AT, count_KT))
      ) %>%
      select(
        Maske,
        Wert_KT,
        Wert_AT,
        Unterschiede
      )
    
    bestockung_gt4m_export$Maske[-1] <- ""
    
    bestockung_gt4m_export
  })
  
  # cluster_name / plot_name are columns on the record, not keys inside the
  # properties JSON, so take them from the values the user entered.
  traktnummer <- cluster_name
  traktecke <- plot_name
  
  # Abschlussdaten und Namen stammen jeweils aus derselben Version wie die
  # Messdaten: erster und letzter abgegebener Stand desselben AT-Trupps.
  datum_kt <- format_export_datum(record$completed_at_troop)
  datum_at <- format_export_datum(latest_change$completed_at_troop)
  
  kt_personen <- get_troop_name(record$responsible_troop)
  at_personen <- get_troop_name(latest_change$responsible_troop)
  
  # Waldentscheid fuer jede Traktecke immer als eigene Maskenzeile anzeigen,
  # auch wenn keine Baum-, Verjuengungs- oder Bestockungsvergleiche vorliegen.
  ecke_export <- tibble(
    Maske = "Ecke",
    Wert_KT = paste0(
      "Waldentscheid: ",
      if (nzchar(waldstatus_kt$code)) paste0(waldstatus_kt$code, "; ") else "",
      waldstatus_kt$name
    ),
    Wert_AT = paste0(
      "Waldentscheid: ",
      if (nzchar(waldstatus_at$code)) paste0(waldstatus_at$code, "; ") else "",
      waldstatus_at$name
    ),
    Unterschiede = if (identical(waldstatus_kt$code, waldstatus_at$code)) {
      ""
    } else "Waldentscheid geaendert",
    Bemerkungen = ""
  )
  
  export_df <- bind_rows(
    ecke_export,
    verjuengung_export,
    deadwood_export,
    wzp4_export,
    wzp4_flags_export,
    raender_export,
    bestockung_export,
    bestockung_gt4m_export
  )
  
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
  
  # Trakt und Ecke nur am Anfang jedes Eckblocks anzeigen.
  export_df$Traktnummer[1] <- as.character(traktnummer)
  export_df$Traktecke[1] <- as.character(traktecke)
  
  export_df$Datum_KT[1] <- datum_kt
  export_df$Datum_AT[1] <- datum_at
  
  export_df$AT_Personen[1] <- at_personen
  export_df$KT_Personen[1] <- kt_personen

  export_df
}

# Process the available plots in numeric order and combine them underneath
# one another. Jede Ecke beginnt mit ihrer Waldentscheid-Maskenzeile.
plot_results <- purrr::map(
  plot_names,
  ~ compare_plot(cluster_name, .x, token, forest_status_lookup, get_troop_name)
)

# Alle vier Ecken bleiben erhalten, auch ohne zwei vergleichbare AT-Staende.
# Jede Ecke beginnt wie im Vorbild mit einer eigenen Waldentscheid-Maskenzeile.

# Insert one blank row between plot blocks. The Excel row numbers of these
# separators are retained so that they can be filled dark grey below.
plot_row_counts <- vapply(plot_results, nrow, integer(1))

plot_results_with_separators <- purrr::map2(
  plot_results,
  seq_along(plot_results),
  function(plot_result, i) {
    if (i == length(plot_results)) {
      return(plot_result)
    }
    
    separator <- plot_result[1, , drop = FALSE]
    separator[] <- ""
    bind_rows(plot_result, separator)
  }
)

# Spaltenschema auch ohne vergleichbare Ecke erhalten.
export_df <- bind_rows(
  tibble(
    Traktnummer = character(), Traktecke = character(),
    Datum_KT = character(), Datum_AT = character(),
    AT_Personen = character(), KT_Personen = character(),
    Maske = character(), Wert_KT = character(), Wert_AT = character(),
    Unterschiede = character(), Bemerkungen = character()
  ),
  bind_rows(plot_results_with_separators)
)

# Explizite Exportzuordnung: bisherige KT-Seite = alt; bisherige AT-Seite = neu.
export_df <- export_df %>%
  rename(
    Datum_AT_alt = Datum_KT,
    Datum_AT_neu = Datum_AT,
    AT_Name_alt = KT_Personen,
    AT_Name_neu = AT_Personen,
    Wert_AT_alt = Wert_KT,
    Wert_AT_neu = Wert_AT,
    `Unterschiede_Neu-Alt` = Unterschiede
  ) %>%
  select(
    Traktnummer, Traktecke, Datum_AT_alt, Datum_AT_neu,
    AT_Name_alt, AT_Name_neu, Maske, Wert_AT_alt, Wert_AT_neu,
    `Unterschiede_Neu-Alt`, Bemerkungen
  )

# Verbleibende senkrechte Striche in Textwerten nur fuer den Export ersetzen.
export_df <- export_df %>%
  mutate(across(where(is.character), ~ gsub("|", ";", .x, fixed = TRUE)))

separator_excel_rows <- if (length(plot_results) > 1) {
  cumsum(plot_row_counts[-length(plot_row_counts)]) +
    seq_len(length(plot_results) - 1) +
    1 # Excel header row
} else {
  integer(0)
}

wb <- createWorkbook()
addWorksheet(wb, "Vergleich")
freezePane(wb, sheet = "Vergleich", firstRow = TRUE)

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

# Differenzspalte einschliesslich Ueberschrift mittig ausrichten.
addStyle(
  wb,
  "Vergleich",
  style = createStyle(halign = "center"),
  rows = seq_len(nrow(export_df) + 1L),
  cols = match("Unterschiede_Neu-Alt", names(export_df)),
  gridExpand = TRUE,
  stack = TRUE
)

setColWidths(
  wb,
  "Vergleich",
  cols = 1:ncol(export_df),
  widths = "auto"
)

italic_style <- createStyle(textDecoration = "italic")
masken_zeilen <- which(export_df$Maske != "") + 1
if (length(masken_zeilen) > 0L) {
addStyle(
  wb,
  "Vergleich",
  style = italic_style,
  rows = masken_zeilen,
  cols = match("Maske", names(export_df)), # Spalte Maske dynamisch ermitteln
  gridExpand = TRUE
)
}

if (length(separator_excel_rows) > 0) {
  separator_style <- createStyle(fgFill = "#D9D9D9")
  
  addStyle(
    wb,
    "Vergleich",
    style = separator_style,
    rows = separator_excel_rows,
    cols = 1:ncol(export_df),
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Abweichende Werte nur in den drei Vergleichsspalten rot hinterlegen.
# Fehlend auf beiden Seiten gilt als gleich; fehlend gegen vorhanden als anders.
vergleichsspalten <- match(
  c("Wert_AT_alt", "Wert_AT_neu", "Unterschiede_Neu-Alt"),
  names(export_df)
)

abweichende_excel_zeilen <- which(
  !startsWith(coalesce(export_df$Bemerkungen, ""), "Kein AT-Vergleich:") &
    (coalesce(as.character(export_df$Wert_AT_alt), "") !=
       coalesce(as.character(export_df$Wert_AT_neu), ""))
) + 1L # Kopfzeile beruecksichtigen; export_df enthaelt bereits die Trennzeilen.

# Graue Trennzeilen behalten ihre Formatierung.
abweichende_excel_zeilen <- setdiff(
  abweichende_excel_zeilen,
  separator_excel_rows
)

if (length(abweichende_excel_zeilen) > 0) {
  unterschiede_style <- createStyle(fgFill = "#F4CCCC")
  
  addStyle(
    wb,
    "Vergleich",
    style = unterschiede_style,
    rows = abweichende_excel_zeilen,
    cols = vergleichsspalten,
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Unterer Rahmen auf der letzten Datenzeile jedes Maskenblocks.
# Maske ist nur in der ersten Zeile eines Blocks beschriftet. Trennzeilen
# werden aus der Zuordnung ausgeschlossen, damit der Rahmen auf Daten liegt.
datenzeilen <- setdiff(
  seq_len(nrow(export_df)),
  separator_excel_rows - 1L
)
masken_start_positionen <- which(
  nzchar(coalesce(as.character(export_df$Maske[datenzeilen]), ""))
)

if (length(masken_start_positionen) > 0) {
  masken_ende_positionen <- c(
    masken_start_positionen[-1L] - 1L,
    length(datenzeilen)
  )
  masken_ende_excel_zeilen <- datenzeilen[masken_ende_positionen] + 1L
  
  masken_abschluss_style <- createStyle(
    border = "bottom",
    borderColour = "#808080",
    borderStyle = "medium"
  )
  
  addStyle(
    wb,
    "Vergleich",
    style = masken_abschluss_style,
    rows = masken_ende_excel_zeilen,
    cols = seq_len(ncol(export_df)),
    gridExpand = TRUE,
    stack = TRUE
  )
}

output_path <- file.path(
  output_dir,
  paste0("Protokoll_Wiederaufnahme_AT_AT_", cluster_name, ".xlsx")
)

saveWorkbook(
  wb,
  output_path,
  overwrite = TRUE
)

message("Comparison saved to ", output_path)
