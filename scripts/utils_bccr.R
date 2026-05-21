# ============================================================
# utils_bccr.R — BCCR SDDE API (new JSON endpoint, 2025+)
# ============================================================
# Inherited from the working pipeline_bccr project.
#
# API docs: https://gee.bccr.fi.cr/indicadoreseconomicos/
#           Documentos/DocumentosMetodologiasNotasTecnicas/
#           Estandar_API_SDDE.pdf
#
# Endpoint:
#   GET {BASE}/indicadoresEconomicos/{code}/series
#       ?fechaInicio=yyyy/mm/dd&fechaFin=yyyy/mm/dd&idioma=es
#   Header: Authorization: Bearer {JWT_TOKEN}
#   Response: JSON
#     { "estado": true, "mensaje": "...",
#       "datos": [{ "series": [{ "fecha": "...",
#                    "valorDatoPorPeriodo": 500.12 }, ...] }] }
#
# Required .Renviron:
#   BCCR_EMAIL=your_email@example.com
#   BCCR_TOKEN=your_jwt_token_from_sdd.bccr.fi.cr
# ============================================================

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(lubridate)

# ------ API CONSTANTS -----------------------------------------

BCCR_API_BASE <- paste0(
  "https://apim.bccr.fi.cr/SDDE/api/",
  "Bccr.Ge.SDDE.Publico.Indicadores.API"
)
BCCR_MAX_RETRIES <- 3
BCCR_RETRY_WAIT  <- 5
BCCR_PAUSE       <- 2

# ------ CREDENTIALS -------------------------------------------

get_bccr_credentials <- function() {
  token <- Sys.getenv("BCCR_TOKEN", unset = "")
  email <- Sys.getenv("BCCR_EMAIL", unset = "")
  if (token == "" || token == "your_bearer_token_here")
    stop("[BCCR] Set BCCR_TOKEN in .Renviron (JWT from sdd.bccr.fi.cr)")
  if (email == "" || email == "your_email@example.com")
    stop("[BCCR] Set BCCR_EMAIL in .Renviron")
  list(token = token, email = email)
}

validate_bccr_credentials <- function(creds = NULL) {
  if (is.null(creds)) creds <- get_bccr_credentials()
  df <- tryCatch(
    download_bccr_series("318",
                          format(Sys.Date() - 7, "%Y/%m/%d"),
                          format(Sys.Date(), "%Y/%m/%d"),
                          creds, verbose = FALSE, save_raw = FALSE),
    error = function(e) NULL
  )
  !is.null(df) && nrow(df) > 0
}

# ------ URL BUILDER -------------------------------------------

build_bccr_url <- function(code, start_date, end_date) {
  # Dates must be yyyy/mm/dd
  paste0(
    BCCR_API_BASE,
    "/indicadoresEconomicos/", code, "/series",
    "?fechaInicio=", URLencode(start_date),
    "&fechaFin=",    URLencode(end_date),
    "&idioma=es"
  )
}

# ------ SINGLE SERIES DOWNLOAD --------------------------------

download_bccr_series <- function(series_code,
                                  start_date = "2010/01/01",
                                  end_date   = format(Sys.Date(), "%Y/%m/%d"),
                                  credentials = NULL,
                                  verbose = TRUE,
                                  save_raw = TRUE) {
  if (is.null(credentials)) credentials <- get_bccr_credentials()
  code <- as.character(series_code)
  url  <- build_bccr_url(code, start_date, end_date)

  if (verbose) message("[BCCR] GET serie ", code, " (", start_date, " — ", end_date, ")")

  response <- NULL
  for (i in seq_len(BCCR_MAX_RETRIES)) {
    response <- tryCatch(
      httr::GET(
        url,
        httr::add_headers(
          Authorization  = paste("Bearer", credentials$token),
          `Content-Type` = "application/json"
        ),
        httr::timeout(60),
        httr::user_agent("R-pipeline_fx_forecast_cr/1.0")
      ),
      error = function(e) {
        if (verbose) warning("[BCCR] Attempt ", i, "/", BCCR_MAX_RETRIES, ": ", e$message)
        NULL
      }
    )
    if (!is.null(response) && httr::status_code(response) == 200) break
    if (i < BCCR_MAX_RETRIES) Sys.sleep(BCCR_RETRY_WAIT)
  }

  if (is.null(response) || httr::status_code(response) != 200) {
    st <- if (!is.null(response)) httr::status_code(response) else "NULL"
    warning("[BCCR] Serie ", code, " failed (HTTP ", st, ")")
    return(NULL)
  }

  # Save raw JSON for debugging
  if (save_raw) {
    raw_dir <- "data_raw/bccr"
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    rf <- file.path(raw_dir, sprintf("raw_%s_%s.json", code,
                                      format(Sys.Date(), "%Y%m%d")))
    writeBin(httr::content(response, as = "raw"), rf)
  }

  parse_bccr_json(response, code)
}

# ------ JSON PARSING ------------------------------------------

parse_bccr_json <- function(response, code) {
  txt <- httr::content(response, as = "text", encoding = "UTF-8")

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = TRUE, flatten = TRUE),
    error = function(e) {
      warning("[BCCR] JSON parse error for ", code, ": ", e$message)
      NULL
    }
  )

  if (is.null(parsed)) return(NULL)
  extract_sdde_series(parsed, code)
}

extract_sdde_series <- function(parsed, code) {
  # Check estado flag
  if (!is.null(parsed$estado) && !isTRUE(parsed$estado)) {
    warning("[BCCR] API error for ", code, ": ",
            parsed$mensaje %||% "estado=FALSE")
    return(NULL)
  }

  # Navigate: datos -> first element -> series
  datos <- parsed$datos
  if (is.null(datos) || length(datos) == 0) {
    warning("[BCCR] Empty 'datos' for ", code)
    return(NULL)
  }

  # datos may be data.frame or list
  if (is.data.frame(datos)) {
    series_data <- datos$series[[1]]
  } else if (is.list(datos)) {
    series_data <- datos[[1]]$series
  } else {
    warning("[BCCR] Unexpected 'datos' structure for ", code)
    return(NULL)
  }

  if (is.null(series_data) || length(series_data) == 0) {
    warning("[BCCR] Empty 'series' for ", code)
    return(NULL)
  }

  # Flatten if list of records
  if (is.list(series_data) && !is.data.frame(series_data)) {
    series_data <- tryCatch(dplyr::bind_rows(series_data),
                             error = function(e) NULL)
    if (is.null(series_data)) return(NULL)
  }

  # Detect column names
  date_col  <- intersect(names(series_data),
                          c("fecha", "Fecha", "date"))[1]
  value_col <- intersect(names(series_data),
                          c("valorDatoPorPeriodo", "valor",
                            "Valor", "value"))[1]

  if (is.na(date_col) || is.na(value_col)) {
    warning("[BCCR] Unrecognized columns for ", code, ": ",
            paste(names(series_data), collapse = ", "))
    return(NULL)
  }

  df <- tibble(
    series_code = code,
    date  = parse_sdde_date(series_data[[date_col]]),
    value = as.numeric(series_data[[value_col]])
  ) %>%
    dplyr::filter(!is.na(date), !is.na(value)) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::arrange(date)

  if (nrow(df) == 0) return(NULL)
  df
}

parse_sdde_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, "%Y-%m-%dT%H:%M:%S"))
  m <- is.na(d); if (any(m)) d[m] <- suppressWarnings(as.Date(x[m], "%Y/%m/%d"))
  m <- is.na(d); if (any(m)) d[m] <- suppressWarnings(as.Date(x[m], "%Y-%m-%d"))
  m <- is.na(d); if (any(m)) d[m] <- suppressWarnings(as.Date(x[m], "%d/%m/%Y"))
  d
}

# ------ CACHE CHECK -------------------------------------------

is_bccr_cached <- function(code, cache_dir = "data_raw/bccr",
                            max_age_hours = 12) {
  files <- list.files(cache_dir, pattern = sprintf("^raw_%s_", code),
                       full.names = TRUE)
  if (length(files) == 0) return(FALSE)
  newest <- files[which.max(file.mtime(files))]
  age_h <- as.numeric(difftime(Sys.time(), file.mtime(newest), units = "hours"))
  age_h <= max_age_hours
}

# ------ BATCH DOWNLOAD ----------------------------------------

download_bccr_batch <- function(catalog,
                                 cache_dir = "data_raw/bccr",
                                 force_refresh = FALSE,
                                 credentials = NULL) {
  if (is.null(credentials)) credentials <- get_bccr_credentials()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  results <- list()

  for (i in seq_len(nrow(catalog))) {
    row <- catalog[i, ]
    if (!isTRUE(row$include)) next

    code <- as.character(row$series_code)

    # Check cache
    if (!force_refresh && is_bccr_cached(code, cache_dir)) {
      # Load from parsed CSV if available
      parsed_file <- file.path("data_intermediate/clean",
                                paste0("bccr_", row$var_name, ".csv"))
      if (!file.exists(parsed_file)) {
        parsed_file <- file.path(cache_dir,
                                  paste0("bccr_", code, ".csv"))
      }
      if (file.exists(parsed_file)) {
        message("[BCCR] Cache hit: ", row$var_name)
        results[[row$var_name]] <- readr::read_csv(parsed_file,
                                                    show_col_types = FALSE)
        next
      }
    }

    # Format dates as yyyy/mm/dd for SDDE
    sd <- format(as.Date(row$start_date), "%Y/%m/%d")
    ed <- if (!is.na(row$end_date) && row$end_date != "")
      format(as.Date(row$end_date), "%Y/%m/%d")
    else format(Sys.Date(), "%Y/%m/%d")

    df <- download_bccr_series(code, sd, ed, credentials)

    if (!is.null(df) && nrow(df) > 0) {
      # Save parsed CSV for cache
      csv_file <- file.path(cache_dir, paste0("bccr_", code, ".csv"))
      readr::write_csv(df, csv_file)
      results[[row$var_name]] <- df
      message("[BCCR] OK: ", row$var_name, " — ", nrow(df), " obs")
    } else {
      warning("[BCCR] FAIL: ", row$var_name, " (code ", code, ")")
    }

    Sys.sleep(BCCR_PAUSE)
  }

  results
}

# ------ COVERAGE REPORT ---------------------------------------

bccr_coverage_report <- function(data_list, catalog) {
  active <- dplyr::filter(catalog, include == TRUE)
  tibble(
    var_name    = active$var_name,
    series_code = active$series_code,
    downloaded  = var_name %in% names(data_list),
    n_obs = sapply(var_name, function(v)
      if (v %in% names(data_list)) nrow(data_list[[v]]) else 0L),
    min_date = sapply(var_name, function(v)
      if (v %in% names(data_list)) as.character(min(data_list[[v]]$date))
      else NA_character_),
    max_date = sapply(var_name, function(v)
      if (v %in% names(data_list)) as.character(max(data_list[[v]]$date))
      else NA_character_)
  )
}
