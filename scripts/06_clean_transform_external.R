# ============================================================
# 06_clean_transform_external.R — Clean external series
# ============================================================

log_msg("=== 06: Cleaning external series ===")
clean_ext <- list()

for (vname in names(data_ext_raw)) {
  raw <- data_ext_raw[[vname]]
  if (is.null(raw) || nrow(raw) == 0) next
  cat_row <- catalog_ext %>% filter(var_name == vname)
  if (nrow(cat_row) == 0) next

  df <- raw %>% arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
    mutate(
      roll_med = zoo::rollmedian(value, k=21, fill=NA, align="center"),
      roll_sd  = zoo::rollapply(value, 63, sd, fill=NA, align="center"),
      outlier  = !is.na(roll_med) & !is.na(roll_sd) &
                 abs(value - roll_med) > 6 * roll_sd
    )
  n_out <- sum(df$outlier, na.rm=TRUE)
  if (n_out > 0) { df$value[df$outlier] <- NA_real_; log_msg(paste(" ", vname, ":", n_out, "outliers"), "WARN") }
  df$value <- zoo::na.approx(df$value, maxgap=5, na.rm=FALSE)

  tr <- cat_row$transform
  if (!is.na(tr) && tr != "none") {
    df$value <- switch(tr,
      "log" = log(df$value), "diff" = c(NA, diff(df$value)),
      "logdiff" = c(NA, diff(log(df$value))),
      df$value)
  }

  clean_ext[[vname]] <- df %>% dplyr::select(date, value) %>%
    dplyr::rename(!!vname := value) %>% dplyr::filter(date >= GLOBAL_START_DATE)
  log_msg(paste("  Cleaned:", vname, "—", nrow(clean_ext[[vname]]), "obs"))
}

saveRDS(clean_ext, "data_intermediate/clean/clean_external.rds")
log_msg(paste("Saved", length(clean_ext), "clean external series"))
log_msg("=== 06 done ===")
