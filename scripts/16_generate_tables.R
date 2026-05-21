# ============================================================
# 16_generate_tables.R — Tables + audit infrastructure
# ============================================================
# PRODUCES:
#   - Standard summary tables (coverage, regime overview, taxonomy)
#   - variable_glossary.csv       (5.1)
#   - pipeline_audit_summary.csv  (5.2)
#   - variable_lineage.csv        (5.3)
#   - model_input_map.csv         (5.4)
# ============================================================

log_msg("=== 16: Tables & audit ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")

# ==============================================================
# STANDARD TABLES
# ==============================================================

# Coverage summary
if (file.exists("data_intermediate/diagnostics/coverage_all.csv")) {
  cov <- readr::read_csv("data_intermediate/diagnostics/coverage_all.csv",
                           show_col_types = FALSE)
  cov_sum <- cov %>% group_by(source_type) %>%
    summarise(total = n(), ok = sum(downloaded, na.rm = TRUE), .groups = "drop")
  readr::write_csv(cov_sum, "output/tables/data_coverage_summary.csv")
}

# Regime overview
if (file.exists("data_intermediate/diagnostics/regime_chronology.csv")) {
  ch <- readr::read_csv("data_intermediate/diagnostics/regime_chronology.csv",
                          show_col_types = FALSE)
  rovw <- ch %>% group_by(regime) %>%
    summarise(n_episodes = n(), total_weeks = sum(n_weeks),
              avg_dur = round(mean(n_weeks), 1),
              max_dur = max(n_weeks), .groups = "drop")
  readr::write_csv(rovw, "output/tables/regime_overview.csv")
}

# Variable taxonomy
if (exists("catalog_bccr") && exists("catalog_ext")) {
  tax <- bind_rows(
    catalog_bccr %>% dplyr::filter(include) %>%
      dplyr::select(var_name, concept_name, source, family, frequency),
    catalog_ext %>% dplyr::filter(include) %>%
      dplyr::select(var_name, concept_name, source, family, frequency)
  )
  readr::write_csv(tax, "output/tables/variable_taxonomy.csv")
}

# ==============================================================
# 5.1 VARIABLE GLOSSARY
# ==============================================================

log_msg("Building variable glossary ...")

# Gather metadata from catalogs
all_catalog <- bind_rows(
  catalog_bccr %>% mutate(source_system = "BCCR",
                           source_code = as.character(series_code),
                           series_code = as.character(series_code)),
  catalog_ext  %>% mutate(source_system = source,
                           source_code = as.character(ticker),
                           series_code = NA_character_)
)

# Get stats from the weekly dataset
wk_vars <- setdiff(names(wk), "date")
miss_info <- missingness_report(wk)

glossary <- tibble(variable_name = wk_vars) %>%
  left_join(
    all_catalog %>% dplyr::select(
      variable_name = var_name, display_name = concept_name,
      block = family, source_system, source_code,
      frequency_raw = frequency, transform_config = transform,
      benchmark, multivariate, risk, regime,
      contemporaneous, min_coverage, notes
    ),
    by = "variable_name"
  ) %>%
  left_join(
    miss_info %>% dplyr::select(variable_name = variable,
                                 n_obs = n_valid, pct_missing,
                                 first_valid, last_valid),
    by = "variable_name"
  ) %>%
  mutate(
    frequency_model = "weekly",
    # Classify derived variables
    source_system = case_when(
      !is.na(source_system) ~ source_system,
      grepl("^fx_fwd_|^event_|^regime_|^prob_|^pressure", variable_name) ~ "derived",
      grepl("_ret[0-9]|_lag[0-9]|_vol[0-9]|_ma[0-9]|_z$|_yoy$|_dd", variable_name) ~ "derived",
      grepl("^fin_pc|^trade_|^rate_diff|^term_spread$|usd_rally|vix_high", variable_name) ~ "derived",
      TRUE ~ "unknown"
    ),
    block = case_when(
      !is.na(block) ~ block,
      grepl("^fx_fwd_", variable_name) ~ "target",
      grepl("^event_", variable_name) ~ "event_target",
      grepl("^regime_|^prob_|pressure", variable_name) ~ "regime",
      grepl("^fin_pc", variable_name) ~ "pca_factor",
      TRUE ~ "derived_feature"
    ),
    used_in_pressure_index = variable_name %in%
      c("fx_sell_ret12", "vix_z", "dxy_ret12", "rin_yoy", "tpm_ret6"),
    included_final = n_obs > 0 & !is.na(n_obs)
  )

readr::write_csv(glossary, "output/tables/variable_glossary.csv")
log_msg(paste("Glossary:", nrow(glossary), "variables"))

# ==============================================================
# 5.2 PIPELINE AUDIT SUMMARY
# ==============================================================

log_msg("Building audit summary ...")

n_config_bccr <- nrow(catalog_bccr)
n_config_ext  <- nrow(catalog_ext)
n_active_bccr <- sum(catalog_bccr$include, na.rm = TRUE)
n_active_ext  <- sum(catalog_ext$include, na.rm = TRUE)

cov_all <- if (file.exists("data_intermediate/diagnostics/coverage_all.csv")) {
  readr::read_csv("data_intermediate/diagnostics/coverage_all.csv", show_col_types = FALSE)
} else { tibble(downloaded = logical()) }

n_downloaded <- sum(cov_all$downloaded, na.rm = TRUE)
n_failed     <- sum(!cov_all$downloaded, na.rm = TRUE)

# Count derived features
n_derived <- sum(glossary$source_system == "derived", na.rm = TRUE)

# Model counts
hr_file <- "output/tables/model_horse_race.csv"
if (file.exists(hr_file)) {
  hr <- readr::read_csv(hr_file, show_col_types = FALSE)
  best_models <- hr %>% group_by(horizon) %>%
    slice_min(rmse, n = 1, with_ties = FALSE)
} else {
  hr <- tibble()
  best_models <- tibble()
}

# Current regime
rp <- if (file.exists("output/tables/regime_probability_summary.csv")) {
  readr::read_csv("output/tables/regime_probability_summary.csv", show_col_types = FALSE)
} else { tibble() }

audit <- tibble(
  metric = c(
    "run_timestamp",
    "n_series_configured_bccr", "n_series_configured_ext",
    "n_series_active_bccr", "n_series_active_ext",
    "n_downloaded_ok", "n_downloaded_fail",
    "n_vars_in_weekly_dataset", "n_derived_features",
    "n_used_in_forecasting", "n_used_in_risk", "n_used_in_regime",
    "n_used_in_pressure_index",
    "n_models_evaluated",
    "best_model_1w", "best_model_4w", "best_model_12w", "best_model_24w",
    "current_regime",
    "prob_abundance", "prob_compression", "prob_stress",
    "pressure_index_latest",
    "last_date_weekly", "last_fx_value"
  ),
  value = c(
    format(Sys.time()),
    as.character(n_config_bccr), as.character(n_config_ext),
    as.character(n_active_bccr), as.character(n_active_ext),
    as.character(n_downloaded), as.character(n_failed),
    as.character(length(wk_vars)), as.character(n_derived),
    as.character(sum(glossary$multivariate == TRUE, na.rm = TRUE)),
    as.character(sum(glossary$risk == TRUE, na.rm = TRUE)),
    as.character(sum(glossary$regime == TRUE, na.rm = TRUE)),
    as.character(sum(glossary$used_in_pressure_index, na.rm = TRUE)),
    as.character(if (nrow(hr) > 0) length(unique(hr$model)) else 0),
    # Best models per horizon
    as.character(if (nrow(best_models) > 0 && 1 %in% best_models$horizon)
      best_models$model[best_models$horizon == 1] else "NA"),
    as.character(if (nrow(best_models) > 0 && 4 %in% best_models$horizon)
      best_models$model[best_models$horizon == 4] else "NA"),
    as.character(if (nrow(best_models) > 0 && 12 %in% best_models$horizon)
      best_models$model[best_models$horizon == 12] else "NA"),
    as.character(if (nrow(best_models) > 0 && 24 %in% best_models$horizon)
      best_models$model[best_models$horizon == 24] else "NA"),
    # Regime
    as.character(if (nrow(rp) > 0) rp$regime_rules[1] else "NA"),
    as.character(if (nrow(rp) > 0) round(rp$prob_abundance[1], 3) else "NA"),
    as.character(if (nrow(rp) > 0) round(rp$prob_compression[1], 3) else "NA"),
    as.character(if (nrow(rp) > 0) round(rp$prob_stress[1], 3) else "NA"),
    as.character(if ("pressure_index" %in% names(wk))
      round(tail(wk$pressure_index[!is.na(wk$pressure_index)], 1), 3) else "NA"),
    as.character(max(wk$date, na.rm = TRUE)),
    as.character(round(tail(wk$fx_sell[!is.na(wk$fx_sell)], 1), 2))
  )
)

readr::write_csv(audit, "output/tables/pipeline_audit_summary.csv")
log_msg("Audit summary saved.")

# ==============================================================
# 5.3 VARIABLE LINEAGE
# ==============================================================

log_msg("Building variable lineage ...")

lineage <- tribble(
  ~variable_final,       ~source_variables,                          ~transformation,               ~script,                           ~stage,
  "fx_sell_ret1",        "fx_sell",                                  "1-period pct return",          "08_feature_engineering.R",        "features",
  "fx_sell_ret4",        "fx_sell",                                  "4-period pct return",          "08_feature_engineering.R",        "features",
  "fx_sell_ret12",       "fx_sell",                                  "12-period pct return",         "08_feature_engineering.R",        "features",
  "fx_sell_ret1_vol8",   "fx_sell_ret1",                             "rolling 8-period SD",          "08_feature_engineering.R",        "features",
  "vix_z",               "vix",                                      "rolling 52-week z-score",      "08_feature_engineering.R",        "features",
  "dxy_ret12",           "dxy",                                      "12-period pct return",         "08_feature_engineering.R",        "features",
  "rin_yoy",             "rin",                                      "52-period YoY change",         "08_feature_engineering.R",        "features",
  "tpm_ret6",            "tpm",                                      "6-period pct return",          "08_feature_engineering.R",        "features",
  "term_spread",         "ust10y, ust2y",                            "ust10y - ust2y",               "08_feature_engineering.R",        "features",
  "rate_diff_cr_us",     "tpm, fedfunds",                            "tpm - fedfunds",               "08_feature_engineering.R",        "features",
  "pressure_raw",        "fx_sell_ret12, vix_z, dxy_ret12, rin_yoy, tpm_ret6", "weighted z-score composite", "09_construct_regime_indicators.R", "regime",
  "pressure_index",      "pressure_raw",                             "EMA(span=8)",                  "09_construct_regime_indicators.R", "regime",
  "regime_raw",          "fx_sell_ret12, vix, dxy_ret12",            "rule-based thresholds",        "09_construct_regime_indicators.R", "regime",
  "regime_rules",        "regime_raw",                               "min-persistence(4w)",          "09_construct_regime_indicators.R", "regime",
  "prob_abundance",      "regime_rules",                             "rolling 20w frequency",        "09_construct_regime_indicators.R", "regime",
  "fx_fwd_ret_4w",       "fx_sell",                                  "(x_{t+4} - x_t) / x_t",       "08_feature_engineering.R",        "targets",
  "event_down_4w",       "fx_fwd_ret_4w",                            "binary: < -0.015",             "09_construct_regime_indicators.R", "targets",
  "fin_pc1",             "vix, dxy, sp500_ret4, ust10y, wti, nfci",  "PCA component 1",             "08_feature_engineering.R",        "features",
  "trade_balance",       "exports_fob, imports_cif",                  "exports - imports",            "08_feature_engineering.R",        "features"
)

readr::write_csv(lineage, "output/tables/variable_lineage.csv")
log_msg(paste("Lineage:", nrow(lineage), "entries"))

# ==============================================================
# 5.4 MODEL INPUT MAP
# ==============================================================

log_msg("Building model input map ...")

# Reconstruct which features each model family uses
fwd_cols_all <- grep("^fx_fwd_|^event_|^regime_|^prob_", names(wk), value = TRUE)
exclude_all  <- c("date","fx_sell","fx_level","log_fx", fwd_cols_all,
                   "regime_label","regime_composite","pressure_index",
                   "pressure_raw","regime_raw","regime_transition")
pool <- setdiff(names(wk), exclude_all)
pool <- pool[sapply(pool, function(v) is.numeric(wk[[v]]))]
cov_p <- colMeans(!is.na(wk[, pool, drop = FALSE]))
pool_used <- names(cov_p[cov_p > 0.50])
if (length(pool_used) > 40) pool_used <- names(sort(cov_p[pool_used], decreasing = TRUE))[1:40]

# Read variable importance if available
vi_file <- "output/tables/variable_importance_lasso.csv"
lasso_vars <- if (file.exists(vi_file)) {
  readr::read_csv(vi_file, show_col_types = FALSE)$variable
} else { character() }

model_map <- tribble(
  ~model,       ~family,         ~target_pattern,       ~n_features,          ~feature_selection, ~key_params,
  "rw",         "benchmark",     "fx_sell (level)",     0,                    "none",             "h-step naive",
  "rw_drift",   "benchmark",     "fx_sell (level)",     0,                    "none",             "linear drift",
  "ma_4",       "benchmark",     "fx_sell (level)",     0,                    "none",             "window=4",
  "ma_12",      "benchmark",     "fx_sell (level)",     0,                    "none",             "window=12",
  "arima",      "benchmark",     "fx_sell (level)",     0,                    "auto.arima",       "max p=5 q=5 d=2",
  "ets",        "benchmark",     "fx_sell (level)",     0,                    "auto",             "exponential smoothing",
  "lasso",      "multivariate",  "fx_fwd_ret_{h}w",    length(pool_used),    "L1 shrinkage",     "alpha=1, cv lambda",
  "ridge",      "multivariate",  "fx_fwd_ret_{h}w",    length(pool_used),    "L2 shrinkage",     "alpha=0, cv lambda",
  "enet",       "multivariate",  "fx_fwd_ret_{h}w",    length(pool_used),    "L1+L2",            "alpha=0.5, cv lambda",
  "local_proj", "multivariate",  "fx_fwd_ret_{h}w",    min(10, length(pool_used)), "top N by coverage", "OLS, direct h-step"
)

readr::write_csv(model_map, "output/tables/model_input_map.csv")

# Also save the actual feature list used
readr::write_csv(tibble(feature = pool_used, position = seq_along(pool_used),
                          selected_by_lasso = pool_used %in% lasso_vars),
                  "output/tables/feature_pool.csv")

log_msg(paste("Model map:", nrow(model_map), "models"))

# ==============================================================
# FINAL COUNT
# ==============================================================

all_t <- list.files("output/tables", "\\.csv$")
log_msg(paste("Total tables:", length(all_t)))
log_msg("=== 16 done ===")
