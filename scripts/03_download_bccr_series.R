# ============================================================
# 03_download_bccr_series.R — BCCR SDDE download
# ============================================================

log_msg("=== 03: Downloading BCCR series ===")
FORCE_REFRESH_BCCR <- FALSE

if (!CREDENTIALS_STATUS$bccr) {
  log_msg("Skipping BCCR (credentials invalid).", "WARN")
  data_bccr_raw <- list()
} else {
  data_bccr_raw <- download_bccr_batch(catalog_bccr,
                                         cache_dir = "data_raw/bccr",
                                         force_refresh = FORCE_REFRESH_BCCR)
  log_msg(paste("Downloaded", length(data_bccr_raw), "BCCR series"))
}

coverage_bccr <- bccr_coverage_report(data_bccr_raw, catalog_bccr)
readr::write_csv(coverage_bccr, "data_intermediate/diagnostics/coverage_bccr.csv")
log_msg(paste("BCCR OK:", sum(coverage_bccr$downloaded),
              " Failed:", sum(!coverage_bccr$downloaded)))
log_msg("=== 03 done ===")
