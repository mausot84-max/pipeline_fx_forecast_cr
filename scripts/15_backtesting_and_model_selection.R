# ============================================================
# 15_backtesting_and_model_selection.R — Horse race
# ============================================================
# IMPROVEMENTS:
#   1. Consolidated table with ranking + RW improvement
#   2. Subperiod stability (pre/post 2020)
#   3. Magnitude assessment of improvement
# ============================================================

log_msg("=== 15: Model selection ===")

bt_base <- if (file.exists("models/bt_baseline_weekly.rds")) {
  readRDS("models/bt_baseline_weekly.rds") %>% mutate(family = "benchmark")
} else { tibble() }

bt_multi <- if (file.exists("models/bt_multivariate_weekly.rds")) {
  readRDS("models/bt_multivariate_weekly.rds") %>% mutate(family = "multivariate")
} else { tibble() }

bt_all <- bind_rows(bt_base, bt_multi)

if (nrow(bt_all) == 0) {
  log_msg("No backtest results. Skipping.", "WARN")
} else {

  # ==============================================================
  # FULL-SAMPLE HORSE RACE
  # ==============================================================

  # NOTE on directional accuracy:
  # The plain random walk predicts "no change" (predicted_fwd_ret = 0),
  # so sign(0) does not match any non-zero actual direction. This yields
  # dir_acc ~ 0, which is technically correct but misleading as a benchmark.
  # We flag it as NA in the output to avoid confusion; the RW benchmark
  # should be read in RMSE space, not directional space.
  horse_race <- bt_all %>%
    dplyr::filter(!is.na(predicted_fwd_ret), !is.na(actual_fwd_ret)) %>%
    group_by(model, family, horizon) %>%
    summarise(
      n       = n(),
      rmse    = calc_rmse(actual_fwd_ret, predicted_fwd_ret),
      mae     = calc_mae(actual_fwd_ret, predicted_fwd_ret),
      dir_acc = calc_directional_accuracy(actual_fwd_ret, predicted_fwd_ret),
      .groups = "drop"
    ) %>%
    mutate(dir_acc = ifelse(model == "rw" & dir_acc < 0.05,
                             NA_real_, dir_acc))

  # Add ranking and RW comparison
  rw_met <- horse_race %>%
    dplyr::filter(model == "rw") %>%
    dplyr::select(horizon, rw_rmse = rmse, rw_mae = mae, rw_da = dir_acc)

  horse_race_full <- horse_race %>%
    left_join(rw_met, by = "horizon") %>%
    group_by(horizon) %>%
    mutate(rank_rmse = rank(rmse)) %>%
    ungroup() %>%
    mutate(
      rmse_vs_rw = round((rmse / rw_rmse - 1) * 100, 1),   # % change vs RW
      mae_vs_rw  = round((mae / rw_mae - 1) * 100, 1),
      da_diff    = round(dir_acc - rw_da, 3),
      improvement_magnitude = case_when(
        rmse_vs_rw < -10 ~ "strong",
        rmse_vs_rw < -3  ~ "moderate",
        rmse_vs_rw < 3   ~ "marginal",
        TRUE             ~ "worse"
      )
    ) %>%
    dplyr::select(model, family, horizon, rank_rmse, n,
                  rmse, mae, dir_acc,
                  rmse_vs_rw, mae_vs_rw, da_diff,
                  improvement_magnitude) %>%
    arrange(horizon, rank_rmse)

  readr::write_csv(horse_race_full, "output/tables/model_horse_race.csv")
  log_msg("Horse race (consolidated):"); print(horse_race_full)

  # Best per horizon
  best <- horse_race_full %>%
    group_by(horizon) %>%
    slice_min(rmse, n = 1, with_ties = FALSE) %>%
    ungroup()
  readr::write_csv(best, "output/tables/best_model_by_horizon.csv")

  # ==============================================================
  # SUBPERIOD STABILITY: PRE vs POST 2020
  # ==============================================================

  log_msg("Subperiod analysis: pre/post 2020 ...")

  cutoff <- as.Date("2020-01-01")

  subperiod_fn <- function(bt_df, period_label, date_filter) {
    bt_df %>%
      dplyr::filter(date_filter, !is.na(predicted_fwd_ret), !is.na(actual_fwd_ret)) %>%
      group_by(model, family, horizon) %>%
      summarise(
        n = n(), rmse = calc_rmse(actual_fwd_ret, predicted_fwd_ret),
        mae = calc_mae(actual_fwd_ret, predicted_fwd_ret),
        dir_acc = calc_directional_accuracy(actual_fwd_ret, predicted_fwd_ret),
        .groups = "drop"
      ) %>%
      mutate(period = period_label)
  }

  # Determine which date column to use
  date_col_bt <- if ("origin_date" %in% names(bt_all)) "origin_date" else "target_date"

  sub_pre  <- subperiod_fn(bt_all, "pre_2020",
                            bt_all[[date_col_bt]] < cutoff)
  sub_post <- subperiod_fn(bt_all, "post_2020",
                            bt_all[[date_col_bt]] >= cutoff)

  subperiod_table <- bind_rows(sub_pre, sub_post) %>%
    arrange(horizon, period, rmse)

  readr::write_csv(subperiod_table, "output/tables/horse_race_subperiods.csv")
  log_msg("Subperiod table saved.")

  # Stability check: does ranking change?
  stability <- subperiod_table %>%
    group_by(horizon, period) %>%
    mutate(rank_rmse = rank(rmse)) %>%
    ungroup() %>%
    dplyr::select(model, horizon, period, rank_rmse) %>%
    tidyr::pivot_wider(names_from = period, values_from = rank_rmse,
                        names_prefix = "rank_")

  readr::write_csv(stability, "output/tables/ranking_stability.csv")

  # ==============================================================
  # PREDICTION INTERVAL COVERAGE
  # ==============================================================

  if ("lower_95" %in% names(bt_base) && "upper_95" %in% names(bt_base)) {
    pi_eval <- bt_base %>%
      dplyr::filter(!is.na(lower_95), !is.na(upper_95)) %>%
      group_by(model, horizon) %>%
      summarise(
        n = n(),
        coverage95 = calc_coverage(actual_level, lower_95, upper_95),
        width95    = mean(upper_95 - lower_95, na.rm = TRUE),
        .groups = "drop"
      )
    if (nrow(pi_eval) > 0)
      readr::write_csv(pi_eval, "output/tables/prediction_interval_eval.csv")
  }
}

log_msg("=== 15 done ===")
