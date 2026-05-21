# ============================================================
# 04_download_external_series.R — External series (FRED-first)
# ============================================================
# Downloads FRED series first (structural), then Yahoo (aux).
# Pipeline continues even if auxiliary sources fail.
# ============================================================

log_msg("=== 04: Downloading external series ===")
FORCE_REFRESH_EXT <- FALSE

n_active <- sum(catalog_ext$include, na.rm = TRUE)

if (n_active == 0) {
  log_msg("No active external series.", "WARN")
  data_ext_raw <- list()
} else {
  # Log source breakdown
  src_counts <- table(catalog_ext$source[catalog_ext$include])
  for (s in names(src_counts))
    log_msg(paste("  Source", s, ":", src_counts[s], "series"))

  data_ext_raw <- download_external_batch(catalog_ext,
                                            cache_dir = "data_raw/external",
                                            force_refresh = FORCE_REFRESH_EXT)
  log_msg(paste("Downloaded", length(data_ext_raw), "/", n_active, "external series"))
}

coverage_ext <- external_coverage_report(data_ext_raw, catalog_ext)
readr::write_csv(coverage_ext, "data_intermediate/diagnostics/coverage_external.csv")

# Combined coverage
coverage_all <- bind_rows(
  coverage_bccr %>% mutate(source_type = "bccr"),
  coverage_ext  %>% mutate(source_type = "external")
)
readr::write_csv(coverage_all, "data_intermediate/diagnostics/coverage_all.csv")
log_msg(paste("Total available:", sum(coverage_all$downloaded), "/", nrow(coverage_all)))
log_msg("=== 04 done ===")
