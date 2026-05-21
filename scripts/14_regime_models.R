# ============================================================
# 14_regime_models.R — Regime detection & early warning
# ============================================================
# A. Logit: predict regime transitions
# B. Logit: early warning for downside/upside events
# C. Regime-conditional return statistics
# D. Current regime assessment
# ============================================================

log_msg("=== 14: Regime models ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")

# ------ COMMON PREDICTORS ------------------------------------
# Use only variables that are backward-looking and actually exist.
ew_candidates <- c(
  "fx_sell_ret4","fx_sell_ret12",
  "fx_sell_ret1_vol8","fx_sell_ret1_vol12",
  "vix_z","dxy_ret12",
  "pressure_index",
  "rate_diff_cr_us","term_spread",
  "vix_high","usd_rally",
  "fin_pc1","fin_pc2"
)
ew_vars <- intersect(ew_candidates, names(wk))
log_msg(paste("Regime/EW predictors available:", length(ew_vars)))

# ==============================================================
# A. LOGIT: REGIME TRANSITION
# ==============================================================

if ("regime_transition" %in% names(wk) &&
    sum(!is.na(wk$regime_transition)) >= 50 &&
    length(ew_vars) >= 3) {

  log_msg("Fitting logit for regime transition ...")
  fml <- as.formula(paste("regime_transition ~", paste(ew_vars, collapse=" + ")))

  logit_trans <- tryCatch(
    glm(fml, data=wk, family=binomial, na.action=na.omit),
    error = function(e) { log_msg(paste("Logit error:", e$message), "WARN"); NULL })

  if (!is.null(logit_trans)) {
    lt_tidy <- broom::tidy(logit_trans) %>% arrange(p.value)
    readr::write_csv(lt_tidy, "output/tables/logit_regime_transition.csv")
    saveRDS(logit_trans, "models/logit_regime_transition.rds")
    log_msg("Logit transition — top predictors:")
    print(head(lt_tidy, 6))
  }
} else {
  log_msg("Insufficient data for regime transition logit.", "WARN")
}

# ==============================================================
# B. EARLY WARNING: DOWNSIDE / UPSIDE EVENTS
# ==============================================================

event_cols <- grep("^event_down_|^event_up_", names(wk), value=TRUE)
ew_results <- list()

for (ev in event_cols) {
  n_events <- sum(wk[[ev]] == 1, na.rm=TRUE)
  n_valid  <- sum(!is.na(wk[[ev]]))
  if (n_valid < 50 || n_events < 5) next

  fml <- as.formula(paste(ev, "~", paste(ew_vars, collapse=" + ")))
  fit <- tryCatch(glm(fml, data=wk, family=binomial, na.action=na.omit),
                   error = function(e) NULL)
  if (is.null(fit)) next

  tidy_fit <- broom::tidy(fit) %>% arrange(p.value) %>%
    mutate(event = ev)
  ew_results[[ev]] <- tidy_fit

  # In-sample AUC approximation
  pp <- predict(fit, type="response")
  ae <- fit$model[[ev]]
  if (length(unique(ae)) == 2) {
    pos <- pp[ae==1]; neg <- pp[ae==0]
    if (length(pos)>0 && length(neg)>0) {
      auc <- mean(sapply(pos, function(p) mean(p > neg)))
      log_msg(paste(" ", ev, "— n_events:", n_events,
                    " AUC:", round(auc, 3)))
    }
  }
}

if (length(ew_results) > 0) {
  readr::write_csv(bind_rows(ew_results),
                    "output/tables/early_warning_logit_coefs.csv")
}

# ==============================================================
# C. REGIME-CONDITIONAL RETURN STATS
# ==============================================================

if ("regime_label" %in% names(wk) && "fx_sell_ret1" %in% names(wk)) {
  regime_stats <- wk %>%
    filter(!is.na(regime_label), !is.na(fx_sell_ret1)) %>%
    group_by(regime_label) %>%
    summarise(
      n_weeks   = n(),
      mean_ret  = mean(fx_sell_ret1, na.rm=TRUE),
      sd_ret    = sd(fx_sell_ret1, na.rm=TRUE),
      pct_neg   = mean(fx_sell_ret1 < 0, na.rm=TRUE),
      q05       = quantile(fx_sell_ret1, 0.05, na.rm=TRUE),
      q95       = quantile(fx_sell_ret1, 0.95, na.rm=TRUE),
      .groups = "drop"
    )
  readr::write_csv(regime_stats,
                    "output/tables/regime_conditional_return_stats.csv")
  log_msg("Regime-conditional stats:"); print(regime_stats)
}

# ==============================================================
# D. CURRENT REGIME ASSESSMENT
# ==============================================================

if (all(c("prob_abundance","prob_compression","prob_stress") %in% names(wk))) {
  latest <- wk %>% filter(!is.na(prob_abundance)) %>% slice_tail(n=1)

  regime_now <- tibble(
    as_of_date       = latest$date,
    regime_rules     = as.character(latest$regime_label),
    prob_abundance   = latest$prob_abundance,
    prob_compression = latest$prob_compression,
    prob_stress      = latest$prob_stress,
    pressure_index   = if("pressure_index" %in% names(latest))
                         latest$pressure_index else NA_real_
  )
  readr::write_csv(regime_now, "output/tables/regime_probability_summary.csv")
  log_msg("Current regime:"); print(regime_now)
}

log_msg("=== 14 done ===")
