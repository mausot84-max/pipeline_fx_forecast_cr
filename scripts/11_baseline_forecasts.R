# ============================================================
# 11_baseline_forecasts.R — Univariate benchmarks
# ============================================================
# Benchmarks produce LEVEL forecasts at each horizon h.
# Evaluation converts to RETURN space so that directional
# accuracy is meaningful: predicted_return = (pred_level -
# current_level) / current_level  vs  actual forward return.
# ============================================================

log_msg("=== 11: Baseline forecasts ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")
wk_clean <- wk %>% filter(!is.na(fx_sell))
n_obs <- nrow(wk_clean)

log_msg(paste("Backtest on", n_obs, "weekly obs"))

origins <- seq(BT_MIN_TRAIN, n_obs - max(HORIZONS_W), by = BT_STEP)
log_msg(paste("Origins:", length(origins)))

results <- list()

for (orig in origins) {
  y_train <- wk_clean$fx_sell[1:orig]
  current_level <- y_train[orig]

  # Produce level forecasts at max horizon
  max_h <- min(max(HORIZONS_W), n_obs - orig)
  if (max_h < 1) next

  fc_list <- list(
    rw       = forecast_rw(y_train, max_h),
    rw_drift = forecast_rw_drift(y_train, max_h),
    ma_4     = forecast_ma(y_train, max_h, 4),
    ma_12    = forecast_ma(y_train, max_h, 12),
    arima    = forecast_arima_auto(y_train, max_h),
    ets      = forecast_ets(y_train, max_h)
  )

  for (h in HORIZONS_W) {
    if (h > max_h) next
    actual_level <- wk_clean$fx_sell[orig + h]
    if (is.na(actual_level)) next

    # Actual forward return
    actual_fwd_ret <- (actual_level - current_level) / current_level

    for (mname in names(fc_list)) {
      fc <- fc_list[[mname]]
      if (length(fc$point) < h) next

      pred_level   <- fc$point[h]
      pred_fwd_ret <- (pred_level - current_level) / current_level

      row <- tibble(
        origin      = orig,
        origin_date = wk_clean$date[orig],
        horizon     = h,
        target_date = wk_clean$date[orig + h],
        model       = mname,
        actual_level    = actual_level,
        predicted_level = pred_level,
        actual_fwd_ret    = actual_fwd_ret,
        predicted_fwd_ret = pred_fwd_ret
      )

      # Add prediction intervals if available
      if (!is.null(fc$lower_95) && length(fc$lower_95) >= h) {
        row$lower_95 <- fc$lower_95[h]
        row$upper_95 <- fc$upper_95[h]
      }

      results[[length(results) + 1]] <- row
    }
  }
}

bt_baseline <- bind_rows(results)

# ------ METRICS -----------------------------------------------
metrics_baseline <- bt_baseline %>%
  filter(!is.na(predicted_fwd_ret)) %>%
  group_by(model, horizon) %>%
  summarise(
    n    = n(),
    rmse_ret = calc_rmse(actual_fwd_ret, predicted_fwd_ret),
    mae_ret  = calc_mae(actual_fwd_ret, predicted_fwd_ret),
    rmse_lvl = calc_rmse(actual_level, predicted_level),
    dir_acc  = calc_directional_accuracy(actual_fwd_ret, predicted_fwd_ret),
    .groups = "drop"
  ) %>%
  arrange(horizon, rmse_ret)

saveRDS(bt_baseline, "models/bt_baseline_weekly.rds")
readr::write_csv(metrics_baseline, "output/tables/baseline_metrics_weekly.csv")
log_msg("Baseline metrics:"); print(metrics_baseline)
log_msg("=== 11 done ===")
