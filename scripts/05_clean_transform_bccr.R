# ============================================================
# 05_clean_transform_bccr.R — Clean BCCR series
# ============================================================

log_msg("=== 05: Cleaning BCCR series ===")
clean_bccr <- list()

for (vname in names(data_bccr_raw)) {
  raw <- data_bccr_raw[[vname]]
  if (is.null(raw) || nrow(raw) == 0) next
  cat_row <- catalog_bccr %>% filter(var_name == vname)
  if (nrow(cat_row) == 0) next

  df <- raw %>% arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
    mutate(
      roll_med = zoo::rollmedian(value, k=21, fill=NA, align="center"),
      roll_sd  = zoo::rollapply(value, 63, sd, fill=NA, align="center"),
      outlier  = !is.na(roll_med) & !is.na(roll_sd) &
                 abs(value - roll_med) > 6 * roll_sd
    )
  n_out <- sum(df$outlier, na.rm=TRUE)
  if (n_out > 0) { df$value[df$outlier] <- NA_real_; log_msg(paste(" ", vname, ": removed", n_out, "outliers"), "WARN") }
  df$value <- zoo::na.approx(df$value, maxgap=5, na.rm=FALSE)

  tr <- cat_row$transform
  if (!is.na(tr) && tr != "none") {
    df$value <- switch(tr,
      "log" = log(df$value), "diff" = c(NA, diff(df$value)),
      "logdiff" = c(NA, diff(log(df$value))),
      "pct" = c(NA, diff(df$value)/head(df$value, -1)),
      df$value)
  }

  clean_bccr[[vname]] <- df %>% select(date, value) %>%
    rename(!!vname := value) %>% filter(date >= GLOBAL_START_DATE)
  log_msg(paste("  Cleaned:", vname, "—", nrow(clean_bccr[[vname]]), "obs"))
}

saveRDS(clean_bccr, "data_intermediate/clean/clean_bccr.rds")
log_msg(paste("Saved", length(clean_bccr), "clean BCCR series"))
log_msg("=== 05 done ===")
