# ============================================================
# 18_export_outputs.R — Final datasets & summary
# ============================================================

log_msg("=== 18: Exporting ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")
mn <- readRDS("data_intermediate/features/features_monthly.rds")

readr::write_csv(wk, "data_final/modeling_dataset_weekly.csv")
readr::write_csv(mn, "data_final/modeling_dataset_monthly.csv")

regime_cols <- intersect(
  c("date","fx_sell","regime_raw","regime_rules","regime_label",
    "regime_composite","pressure_raw","pressure_index","regime_transition",
    "prob_abundance","prob_compression","prob_stress",
    grep("^event_", names(wk), value = TRUE)),
  names(wk))
readr::write_csv(wk[, regime_cols], "data_final/regime_dataset.csv")

log_msg(paste("Weekly:", nrow(wk), "x", ncol(wk)))
log_msg(paste("Monthly:", nrow(mn), "x", ncol(mn)))

# ------ SUMMARY -----------------------------------------------
s <- c(
  "============================================================",
  "  pipeline_fx_forecast_cr — Execution Summary",
  paste("  Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "  Architecture: WEEKLY-FIRST (Friday close)",
  "  Regime smoothing: 4-week persistence + EMA pressure index",
  "============================================================", ""
)

s <- c(s, paste("  Weekly obs:", nrow(wk), " cols:", ncol(wk)),
       paste("  Date range:", min(wk$date, na.rm=TRUE), "to", max(wk$date, na.rm=TRUE)),
       paste("  Tables:", length(list.files("output/tables", "\\.csv$"))),
       paste("  Figures:", length(list.files("output/figures", "\\.png$"))),
       paste("  Models:", length(list.files("models", "\\.rds$"))), "")

# Current regime
if (file.exists("output/tables/regime_probability_summary.csv")) {
  rp <- readr::read_csv("output/tables/regime_probability_summary.csv",
                          show_col_types = FALSE)
  s <- c(s, "  --- CURRENT REGIME ---",
    paste("  As of:", rp$as_of_date),
    paste("  Classification:", rp$regime_rules),
    paste("  P(Abundance):", round(rp$prob_abundance, 3)),
    paste("  P(Compression):", round(rp$prob_compression, 3)),
    paste("  P(Stress):", round(rp$prob_stress, 3)),
    paste("  Pressure index:", round(rp$pressure_index, 3)), "")
}

# Best models
if (file.exists("output/tables/best_model_by_horizon.csv")) {
  bm <- readr::read_csv("output/tables/best_model_by_horizon.csv",
                          show_col_types = FALSE)
  s <- c(s, "  --- BEST MODELS (RMSE) ---")
  for (i in seq_len(nrow(bm)))
    s <- c(s, sprintf("  %sw: %s (RMSE=%.6f  DA=%.3f  vs_RW=%+.1f%%)",
                      bm$horizon[i], bm$model[i], bm$rmse[i],
                      bm$dir_acc[i], bm$rmse_vs_rw[i]))
  s <- c(s, "")
}

# Conditional risk
if (file.exists("output/tables/risk_conditional_current.csv")) {
  rc <- readr::read_csv("output/tables/risk_conditional_current.csv",
                          show_col_types = FALSE)
  s <- c(s, "  --- CONDITIONAL RISK (current) ---")
  for (i in seq_len(nrow(rc))) {
    q5  <- if ("q5"  %in% names(rc)) round(rc$q5[i] * 100, 1) else "NA"
    q50 <- if ("q50" %in% names(rc)) round(rc$q50[i] * 100, 1) else "NA"
    q95 <- if ("q95" %in% names(rc)) round(rc$q95[i] * 100, 1) else "NA"
    s <- c(s, sprintf("  %s: Down95=%s%%  Median=%s%%  Up95=%s%%",
                      rc$horizon[i], q5, q50, q95))
  }
  s <- c(s, "")
}

# Audit outputs
s <- c(s, "  --- AUDIT OUTPUTS ---",
  "  variable_glossary.csv",
  "  pipeline_audit_summary.csv",
  "  variable_lineage.csv",
  "  model_input_map.csv",
  "  feature_pool.csv",
  "  risk_comparison.csv",
  "  horse_race_subperiods.csv",
  "  ranking_stability.csv")

txt <- paste(s, collapse = "\n")
writeLines(txt, "output/logs/pipeline_summary.txt")
cat("\n", txt, "\n")

log_msg("=== 18 done. Pipeline complete. ===")
