# ============================================================
# utils_external.R — External data sources
# ============================================================
# SOURCE HIERARCHY:
#   1. FRED API  — primary external source (structural)
#   2. Yahoo     — auxiliary fallback for market data
#   3. CSV       — manual override when needed
#
# FRED is treated as the second structural API of the pipeline,
# alongside BCCR for local series.
# ============================================================

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(lubridate)

# ==============================================================
# FRED API — PRIMARY EXTERNAL SOURCE
# ==============================================================

get_fred_key <- function() {
  key <- Sys.getenv("FRED_API_KEY")
  if (key == "") {
    warning("[FRED] FRED_API_KEY not set in .Renviron")
    return(NULL)
  }
  key
}

validate_fred_credentials <- function() {
  key <- get_fred_key()
  if (is.null(key)) return(FALSE)

  # Quick probe: fetch a few obs of DGS10
  resp <- tryCatch(
    httr::GET(
      "https://api.stlouisfed.org/fred/series/observations",
      query = list(
        series_id        = "DGS10",
        api_key          = key,
        file_type        = "json",
        observation_start = as.character(Sys.Date() - 30),
        sort_order       = "desc",
        limit            = 5
      ),
      httr::timeout(15)
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) return(FALSE)

  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body), error = function(e) NULL)

  ok <- !is.null(parsed$observations) && nrow(parsed$observations) > 0
  ok
}

download_fred_series <- function(series_id,
                                  start_date = "2010-01-01",
                                  api_key    = NULL,
                                  verbose    = TRUE) {
  if (is.null(api_key)) api_key <- get_fred_key()
  if (is.null(api_key)) return(NULL)

  url <- "https://api.stlouisfed.org/fred/series/observations"

  if (verbose) message("[FRED] Downloading ", series_id, " ...")

  resp <- tryCatch(
    httr::GET(url,
      query = list(
        series_id         = series_id,
        api_key           = api_key,
        file_type         = "json",
        observation_start = start_date
      ),
      httr::timeout(30)
    ),
    error = function(e) {
      warning("[FRED] HTTP error for ", series_id, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning("[FRED] HTTP ",
            if (!is.null(resp)) httr::status_code(resp) else "NULL",
            " for ", series_id)
    return(NULL)
  }

  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body), error = function(e) NULL)

  if (is.null(parsed$observations) || nrow(parsed$observations) == 0) {
    warning("[FRED] No observations for ", series_id)
    return(NULL)
  }

  df <- tibble(
    series_code = series_id,
    date  = as.Date(parsed$observations$date),
    value = suppressWarnings(as.numeric(parsed$observations$value))
  ) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date)

  if (verbose) message("[FRED] OK: ", series_id, " — ", nrow(df), " obs")
  df
}

# ==============================================================
# YAHOO FINANCE — AUXILIARY FALLBACK
# ==============================================================

download_yahoo_series <- function(ticker,
                                   start_date = "2010-01-01",
                                   verbose    = TRUE,
                                   max_retries = 2) {
  sd_unix <- as.numeric(as.POSIXct(start_date, tz = "UTC"))
  ed_unix <- as.numeric(as.POSIXct(Sys.Date() + 1, tz = "UTC"))

  url <- paste0(
    "https://query1.finance.yahoo.com/v7/finance/download/",
    utils::URLencode(ticker),
    "?period1=", floor(sd_unix),
    "&period2=", floor(ed_unix),
    "&interval=1d&events=history"
  )

  if (verbose) message("[YAHOO] Downloading ", ticker, " ...")

  resp <- NULL
  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      httr::GET(url, httr::timeout(20),
                httr::user_agent("Mozilla/5.0 (pipeline_fx_forecast_cr)")),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) break
    Sys.sleep(1)
  }

  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning("[YAHOO] Failed for ", ticker, " after ", max_retries, " attempts")
    return(NULL)
  }

  raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")

  df <- tryCatch({
    readr::read_csv(raw_text, show_col_types = FALSE) %>%
      transmute(
        series_code = ticker,
        date  = as.Date(Date),
        value = as.numeric(`Adj Close`)
      ) %>%
      filter(!is.na(date), !is.na(value)) %>%
      arrange(date)
  }, error = function(e) {
    warning("[YAHOO] Parse error for ", ticker, ": ", e$message)
    NULL
  })

  if (!is.null(df) && verbose)
    message("[YAHOO] OK: ", ticker, " — ", nrow(df), " obs")
  df
}

# ==============================================================
# MANUAL CSV LOADER — TERTIARY SOURCE
# ==============================================================

load_manual_csv <- function(filepath, start_date = "2010-01-01") {
  if (!file.exists(filepath)) {
    warning("[CSV] Not found: ", filepath); return(NULL)
  }
  df <- tryCatch(readr::read_csv(filepath, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(df)) return(NULL)
  names(df) <- tolower(names(df))
  if (!all(c("date","value") %in% names(df))) {
    warning("[CSV] Need 'date' and 'value' columns"); return(NULL)
  }
  df %>%
    mutate(date = as.Date(date), value = as.numeric(value),
           series_code = basename(filepath)) %>%
    filter(!is.na(date), !is.na(value), date >= as.Date(start_date)) %>%
    select(series_code, date, value) %>%
    arrange(date)
}

# ==============================================================
# BATCH DOWNLOADER (dispatches by source)
# ==============================================================

download_external_batch <- function(catalog,
                                     cache_dir = "data_raw/external",
                                     force_refresh = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  results <- list()

  for (i in seq_len(nrow(catalog))) {
    row <- catalog[i, ]
    if (!isTRUE(row$include)) next

    cache_file <- file.path(cache_dir, paste0("ext_", row$var_name, ".csv"))

    # Check cache
    if (file.exists(cache_file) && !force_refresh) {
      age_h <- as.numeric(difftime(Sys.time(),
                                    file.info(cache_file)$mtime, units="hours"))
      if (age_h < 12) {
        message("[EXT] Cache hit: ", row$var_name, " (",round(age_h,1),"h)")
        results[[row$var_name]] <- readr::read_csv(cache_file,
                                                    show_col_types = FALSE)
        next
      }
    }

    sd <- as.character(row$start_date)

    df <- switch(as.character(row$source),
      "fred"  = download_fred_series(row$ticker, sd),
      "yahoo" = download_yahoo_series(row$ticker, sd),
      "csv"   = load_manual_csv(row$ticker, sd),
      {
        warning("[EXT] Unknown source '", row$source, "' for ", row$var_name)
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      readr::write_csv(df, cache_file)
      results[[row$var_name]] <- df
    } else {
      warning("[EXT] FAIL: ", row$var_name, " (source: ", row$source, ")")
    }
    Sys.sleep(0.3)
  }
  results
}

# ------ COVERAGE REPORT ---------------------------------------

external_coverage_report <- function(data_list, catalog) {
  active <- dplyr::filter(catalog, include == TRUE)
  tibble(
    var_name = active$var_name,
    source   = active$source,
    ticker   = active$ticker,
    downloaded = var_name %in% names(data_list),
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
