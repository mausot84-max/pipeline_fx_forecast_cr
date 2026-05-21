# ============================================================
# 08_feature_engineering.R — Features & targets
# ============================================================
# Builds all derived variables using the naming convention
# documented in utils_features.R.  Every variable referenced
# by regime_rules.yml is constructed explicitly here.
#
# NAMING REMINDER:
#   fx_sell_ret12   — 12-period return of fx_sell
#   dxy_ret12       — 12-period return of dxy
#   vix_z           — z-score of vix
#   rin_yoy         — year-on-year change of rin
#   tpm_ret6        — 6-period return of tpm
#   fx_sell_ret1_vol8 — rolling 8-period vol of fx_sell_ret1
# ============================================================

log_msg("=== 08: Feature engineering ===")

wk <- readRDS("data_intermediate/clean/master_weekly.rds") %>% arrange(date)
mn <- readRDS("data_intermediate/clean/master_monthly.rds") %>% arrange(date)

# ==============================================================
# WEEKLY FEATURES
# ==============================================================
log_msg("Weekly features ...")

# --- Forward-looking targets ---
wk <- build_forward_targets(wk, "fx_sell", HORIZONS_W)

# --- Returns for ALL key level variables ----------------------
# These generate: fx_sell_ret1, fx_sell_ret4, fx_sell_ret12, etc.
return_periods <- c(1, 4, 6, 8, 12, 24)
level_vars <- intersect(c("fx_sell","sp500","dxy","wti","rin","tpm",
                            "ust10y","ust2y","vix"), names(wk))

for (v in level_vars) {
  for (n in return_periods) {
    if (nrow(wk) > n) wk <- add_return(wk, v, n)
  }
}

# --- Lags for backward-looking variables ----------------------
lag_vars <- setdiff(names(wk),
                     c("date", grep("^fx_fwd_", names(wk), value=TRUE),
                       "fx_level", "log_fx"))
for (v in lag_vars) {
  if (all(is.na(wk[[v]]))) next
  wk <- add_lags(wk, v, lags = 1:4)
}

# --- Rolling volatility of 1-week return ---------------------
if ("fx_sell_ret1" %in% names(wk))
  wk <- add_rolling_sd(wk, "fx_sell_ret1", windows = c(4, 8, 12, 26))

# --- Rolling means of FX level --------------------------------
if ("fx_sell" %in% names(wk))
  wk <- add_rolling_mean(wk, "fx_sell", windows = c(4, 12, 26, 52))

# --- Rolling drawdown ----------------------------------------
if ("fx_sell" %in% names(wk))
  wk <- add_rolling_drawdown(wk, "fx_sell", window = 52)

# --- Z-scores (rolling 52-week) for regime variables ----------
for (v in intersect(c("vix","dxy","tpm","nfci"), names(wk)))
  wk <- add_zscore(wk, v, window = 52)

# --- YoY changes for slow macro variables ---------------------
# These are carried-forward monthlies in the weekly panel.
for (v in intersect(c("rin","exports_fob","imports_cif","imae_tc",
                        "monetary_base"), names(wk)))
  wk <- add_yoy(wk, v, freq_per_year = 52)

# --- Spreads --------------------------------------------------
wk <- add_spread(wk, "ust10y", "ust2y", "term_spread")
wk <- add_spread(wk, "tpm", "fedfunds", "rate_diff_cr_us")

# --- Trade balance proxy --------------------------------------
if (all(c("exports_fob","imports_cif") %in% names(wk))) {
  wk$trade_balance <- wk$exports_fob - wk$imports_cif
  wk$trade_ratio   <- wk$exports_fob / pmax(wk$imports_cif, 1)
}

# --- Binary stress indicators --------------------------------
if ("vix" %in% names(wk)) {
  wk$vix_high      <- as.integer(wk$vix > 25)
  wk$vix_very_high <- as.integer(wk$vix > 30)
}
if ("dxy_ret12" %in% names(wk))
  wk$usd_rally <- as.integer(wk$dxy_ret12 > 0.03)

# --- PCA of financial block -----------------------------------
pca_vars <- intersect(c("vix","dxy","sp500_ret4","ust10y","wti","nfci"),
                        names(wk))
if (length(pca_vars) >= 3)
  wk <- compute_pca_features(wk, pca_vars, n_comp=3, prefix="fin_pc")

# --- VERIFY regime_rules.yml variables exist ------------------
# These are the variables the composite index needs:
regime_vars_needed <- c("fx_sell_ret12","vix_z","dxy_ret12","rin_yoy","tpm_ret6")
present  <- regime_vars_needed[regime_vars_needed %in% names(wk)]
missing_ <- setdiff(regime_vars_needed, names(wk))
log_msg(paste("Regime vars present:", paste(present, collapse=", ")))
if (length(missing_) > 0)
  log_msg(paste("Regime vars MISSING:", paste(missing_, collapse=", ")), "WARN")

log_msg(paste("Weekly columns:", ncol(wk)))

# ==============================================================
# MONTHLY FEATURES (auxiliary)
# ==============================================================
log_msg("Monthly features ...")

mn <- build_forward_targets(mn, "fx_sell", c(1,3,6))

for (v in intersect(c("fx_sell","sp500","dxy","wti","rin","imae_tc","tpm"),
                      names(mn))) {
  for (n in c(1,3,6,12)) mn <- add_return(mn, v, n)
}

for (v in intersect(c("rin","exports_fob","imports_cif","imae_tc"),
                      names(mn)))
  mn <- add_yoy(mn, v, freq_per_year = 12)

if (all(c("exports_fob","imports_cif") %in% names(mn))) {
  mn$trade_balance <- mn$exports_fob - mn$imports_cif
}

for (v in intersect(c("vix","dxy","tpm"), names(mn)))
  mn <- add_zscore(mn, v, window = 24)

log_msg(paste("Monthly columns:", ncol(mn)))

# ------ SAVE --------------------------------------------------
saveRDS(wk, "data_intermediate/features/features_weekly.rds")
saveRDS(mn, "data_intermediate/features/features_monthly.rds")
log_msg("=== 08 done ===")
