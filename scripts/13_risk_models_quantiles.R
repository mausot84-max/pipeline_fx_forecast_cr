# ============================================================
# 13_risk_models_quantiles.R — Conditional tail-risk models
# ============================================================
# FIX: impute X_now using medians from X_train (not from itself).
# ADDS: conditional_risk_debug.csv with full traceability.
# FALLBACK: if full QR fails, tries parsimonious QR with top 5
#           features by coverage. Reported transparently in debug.
# ============================================================

log_msg("=== 13: Risk models ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")

# ------ REGRESSOR POOL (stationary features only) -----------
# Only returns, z-scores, vols, YoY, drawdowns, PCA factors,
# and bounded indicators — no raw levels.
stationary_patterns <- c("_ret[0-9]", "_z$", "_yoy$", "_vol[0-9]",
                          "_dd[0-9]", "^fin_pc[0-9]",
                          "^pressure_raw$", "^vix_high$",
                          "^usd_rally$", "^trade_ratio$",
                          "^term_spread$", "^rate_diff_cr_us$")
qr_regs <- unique(unlist(lapply(stationary_patterns, function(p)
  grep(p, names(wk), value = TRUE))))
qr_regs <- qr_regs[!grepl("^fx_fwd_", qr_regs)]
qr_regs <- qr_regs[qr_regs %in% names(wk)]
cov_pct <- colMeans(!is.na(wk[, qr_regs, drop = FALSE]))
qr_regs <- names(cov_pct[cov_pct > 0.50])
if (length(qr_regs) > 20)
  qr_regs <- names(sort(cov_pct[qr_regs], decreasing = TRUE))[1:20]
log_msg(paste("QR regressor pool:", length(qr_regs)))

# ==============================================================
# A. UNCONDITIONAL HISTORICAL RISK
# ==============================================================

risk_hist <- list()
for (h in HORIZONS_W) {
  col <- paste0("fx_fwd_ret_", h, "w")
  if (!col %in% names(wk)) next
  vals <- wk[[col]][!is.na(wk[[col]])]
  if (length(vals) < 20) next
  risk_hist[[length(risk_hist) + 1]] <- tibble(
    horizon = paste0(h, "w"), type = "unconditional", n_obs = length(vals),
    mean_ret = mean(vals), sd_ret = sd(vals),
    downside_95 = quantile(vals, 0.05), downside_99 = quantile(vals, 0.01),
    median_ret  = median(vals),
    upside_95   = quantile(vals, 0.95), upside_99 = quantile(vals, 0.99)
  )
}
risk_hist_df <- bind_rows(risk_hist)
readr::write_csv(risk_hist_df, "output/tables/risk_unconditional.csv")

# ==============================================================
# HELPER: impute X_new using training medians
# ==============================================================

impute_with_train_medians <- function(X_new, X_train) {
  meds <- apply(X_train, 2, function(v) median(v, na.rm = TRUE))
  for (j in seq_len(ncol(X_new))) {
    v <- X_new[, j]
    if (is.na(v) || is.nan(v) || is.infinite(v)) {
      X_new[, j] <- if (!is.na(meds[j]) && is.finite(meds[j])) meds[j] else 0
    }
  }
  X_new
}

# ==============================================================
# B. OOS CONDITIONAL QR (backtest)
# ==============================================================

log_msg("Running OOS conditional QR ...")
taus    <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
n_obs   <- nrow(wk)
origins <- seq(BT_MIN_TRAIN, n_obs - max(HORIZONS_W), by = BT_STEP)

results_qr <- list()
for (h in HORIZONS_W) {
  target_col <- paste0("fx_fwd_ret_", h, "w")
  if (!target_col %in% names(wk)) next
  for (orig in origins) {
    if (orig + h > n_obs) next
    y_train <- wk[[target_col]][1:orig]
    X_train_raw <- as.matrix(wk[1:orig, qr_regs, drop = FALSE])
    X_train <- impute_median(X_train_raw)
    X_new_raw <- as.matrix(wk[orig, qr_regs, drop = FALSE])
    X_new <- impute_with_train_medians(X_new_raw, X_train)
    actual <- wk[[target_col]][orig]
    if (is.na(actual)) next
    for (tau in taus) {
      pred <- fit_qr(X_train, y_train, X_new, tau = tau)
      results_qr[[length(results_qr) + 1]] <- tibble(
        origin_date = wk$date[orig], horizon = h,
        tau = tau, actual = actual, predicted = pred
      )
    }
  }
}

bt_qr <- bind_rows(results_qr)
saveRDS(bt_qr, "models/bt_quantile_regression.rds")

qr_metrics <- bt_qr %>%
  dplyr::filter(!is.na(predicted)) %>%
  group_by(horizon, tau) %>%
  summarise(n = n(),
            q_loss = calc_quantile_loss(actual, predicted, first(tau)),
            hit_rate = mean(actual <= predicted), .groups = "drop")
readr::write_csv(qr_metrics, "output/tables/quantile_regression_metrics.csv")

# ==============================================================
# C. CONDITIONAL RISK — CURRENT (with traceable fallback)
# ==============================================================

log_msg("Building current conditional risk assessment ...")

cond_current <- list()
debug_log <- list()

for (h in HORIZONS_W) {
  target_col <- paste0("fx_fwd_ret_", h, "w")
  if (!target_col %in% names(wk)) next

  valid_rows <- which(!is.na(wk[[target_col]]))
  if (length(valid_rows) < 50) {
    debug_log[[length(debug_log)+1]] <- tibble(
      horizon = paste0(h,"w"), as_of_date = NA,
      n_train_effective = length(valid_rows),
      n_features_attempted = length(qr_regs),
      n_features_used = 0L, x_now_complete = FALSE,
      fallback_used = TRUE,
      fallback_reason = "insufficient training obs",
      q5 = NA_real_, q50 = NA_real_, q95 = NA_real_)
    next
  }
  last_train <- max(valid_rows)

  # Find the most informative "current" row: last row where
  # the stationary features are mostly available.
  # Fallback cascade: try last row; if too many NAs in X_now,
  # step back up to 8 weeks until we have a complete-enough state.
  last_row <- nrow(wk)
  x_now_complete <- FALSE
  step_back <- 0
  for (back in 0:8) {
    test_row <- last_row - back
    if (test_row < 1) break
    row_vals <- as.numeric(wk[test_row, qr_regs, drop = TRUE])
    pct_ok <- mean(!is.na(row_vals) & is.finite(row_vals))
    if (pct_ok >= 0.80) {
      last_row <- test_row
      step_back <- back
      x_now_complete <- (pct_ok == 1)
      break
    }
  }

  y_train_full <- wk[[target_col]][1:last_train]
  X_train_full <- impute_median(as.matrix(wk[1:last_train, qr_regs, drop = FALSE]))
  X_now_raw    <- as.matrix(wk[last_row, qr_regs, drop = FALSE])
  X_now        <- impute_with_train_medians(X_now_raw, X_train_full)

  row <- tibble(horizon = paste0(h, "w"), as_of_date = wk$date[last_row])

  # Primary attempt: full QR on all 20 regressors
  preds_primary <- sapply(taus, function(tau)
    fit_qr(X_train_full, y_train_full, X_now, tau = tau))

  fallback_used <- FALSE
  fallback_reason <- ""
  n_features_used <- ncol(X_train_full)

  # If ANY tau returned NA, try parsimonious fallback
  if (any(is.na(preds_primary))) {
    fallback_used <- TRUE
    fallback_reason <- "primary QR returned NA for some tau — retrying with top-5 features by coverage"

    top5 <- names(sort(colMeans(!is.na(wk[1:last_train, qr_regs, drop = FALSE])),
                        decreasing = TRUE))[1:5]
    X_train_small <- impute_median(as.matrix(wk[1:last_train, top5, drop = FALSE]))
    X_now_small   <- impute_with_train_medians(
      as.matrix(wk[last_row, top5, drop = FALSE]), X_train_small)

    preds_primary <- sapply(taus, function(tau)
      fit_qr(X_train_small, y_train_full, X_now_small, tau = tau))
    n_features_used <- length(top5)

    if (any(is.na(preds_primary))) {
      fallback_reason <- paste(fallback_reason,
                                "| parsimonious also failed — using univariate QR on pressure_raw")
      # Last-resort fallback: univariate on pressure_raw
      if ("pressure_raw" %in% names(wk)) {
        X_univ <- impute_median(as.matrix(wk[1:last_train, "pressure_raw", drop = FALSE]))
        X_univ_now <- impute_with_train_medians(
          as.matrix(wk[last_row, "pressure_raw", drop = FALSE]), X_univ)
        preds_primary <- sapply(taus, function(tau)
          fit_qr(X_univ, y_train_full, X_univ_now, tau = tau))
        n_features_used <- 1
      }
    }
  }

  for (k in seq_along(taus))
    row[[paste0("q", round(taus[k] * 100))]] <- as.numeric(preds_primary[k])

  cond_current[[length(cond_current) + 1]] <- row

  debug_log[[length(debug_log)+1]] <- tibble(
    horizon = paste0(h, "w"),
    as_of_date = as.character(wk$date[last_row]),
    n_train_effective = last_train,
    n_features_attempted = length(qr_regs),
    n_features_used = n_features_used,
    x_now_complete = x_now_complete,
    step_back_weeks = step_back,
    fallback_used = fallback_used,
    fallback_reason = fallback_reason,
    q5  = as.numeric(preds_primary[1]),
    q50 = as.numeric(preds_primary[4]),
    q95 = as.numeric(preds_primary[7])
  )
}

cond_current_df <- bind_rows(cond_current)
debug_df        <- bind_rows(debug_log)

readr::write_csv(cond_current_df, "output/tables/risk_conditional_current.csv")
readr::write_csv(debug_df,        "output/tables/conditional_risk_debug.csv")

log_msg("Conditional risk (current):"); print(cond_current_df)
log_msg("Debug trace:"); print(debug_df)

# ==============================================================
# D. FAN CHART DATA
# ==============================================================

if (nrow(cond_current_df) > 0) {
  last_fx   <- tail(wk$fx_sell[!is.na(wk$fx_sell)], 1)
  last_date <- max(wk$date[!is.na(wk$fx_sell)])

  fan_data <- cond_current_df %>%
    mutate(
      horizon_weeks = as.integer(gsub("w", "", horizon)),
      target_date   = last_date + horizon_weeks * 7,
      point    = last_fx * (1 + q50),
      lower_80 = last_fx * (1 + q10),
      upper_80 = last_fx * (1 + q90),
      lower_95 = last_fx * (1 + q5),
      upper_95 = last_fx * (1 + q95)
    ) %>%
    dplyr::select(horizon, target_date, point, lower_80, upper_80, lower_95, upper_95)

  readr::write_csv(fan_data, "output/tables/fan_chart_data.csv")
  log_msg("Fan chart data saved.")
}

# ==============================================================
# E. COMPARISON: UNCONDITIONAL vs CONDITIONAL
# ==============================================================

if (nrow(cond_current_df) > 0 && nrow(risk_hist_df) > 0) {
  comparison <- risk_hist_df %>%
    dplyr::select(horizon, uncond_down95 = downside_95,
                  uncond_median = median_ret, uncond_up95 = upside_95) %>%
    left_join(
      cond_current_df %>%
        dplyr::select(horizon, cond_down95 = q5,
                      cond_median = q50, cond_up95 = q95),
      by = "horizon"
    )
  readr::write_csv(comparison, "output/tables/risk_comparison.csv")
  log_msg("Risk comparison (uncond vs cond):"); print(comparison)
}

# ==============================================================
# F. ROLLING CONDITIONAL DOWNSIDE (4w)
# ==============================================================

cond_down_4w <- bt_qr %>%
  dplyr::filter(horizon == 4, tau == 0.05, !is.na(predicted)) %>%
  dplyr::select(origin_date, actual, cond_downside_05 = predicted)

if (nrow(cond_down_4w) > 10)
  readr::write_csv(cond_down_4w, "output/tables/rolling_conditional_downside_4w.csv")

log_msg("=== 13 done ===")
