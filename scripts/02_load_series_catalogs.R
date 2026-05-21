# ============================================================
# 02_load_series_catalogs.R — Parse config CSVs
# ============================================================

log_msg("=== 02: Loading catalogs ===")

bool_cols <- c("include","benchmark","multivariate","risk",
               "regime","contemporaneous")

catalog_bccr <- readr::read_csv("config/series_bccr_template.csv",
                                 show_col_types = FALSE)
for (col in intersect(bool_cols, names(catalog_bccr)))
  catalog_bccr[[col]] <- as.logical(catalog_bccr[[col]])

catalog_ext <- readr::read_csv("config/series_external_template.csv",
                                show_col_types = FALSE)
for (col in intersect(bool_cols, names(catalog_ext)))
  catalog_ext[[col]] <- as.logical(catalog_ext[[col]])

# Disable FRED series if key is invalid
if (!CREDENTIALS_STATUS$fred) {
  fred_rows <- catalog_ext$source == "fred"
  if (any(fred_rows)) {
    log_msg(paste("Disabling", sum(fred_rows), "FRED series (no key)."), "WARN")
    catalog_ext$include[fred_rows] <- FALSE
  }
}

n_bccr <- sum(catalog_bccr$include, na.rm=TRUE)
n_ext  <- sum(catalog_ext$include, na.rm=TRUE)
log_msg(paste("Active series: BCCR=", n_bccr, " External=", n_ext))
log_msg("=== 02 done ===")
