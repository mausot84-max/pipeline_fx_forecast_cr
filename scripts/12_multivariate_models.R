# ============================================================
# 12_multivariate_models.R — Penalised regression + LP
# ============================================================
# DIRECT FORECASTING: for each horizon h, the model is trained
# on fx_fwd_ret_{h}w as the target.  This ensures the predicted
# quantity matches the evaluation metric at that horizon.
# No iterated 1-step predictions.
# ============================================================

log_msg("=== 12: Multivariate models ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")

# ------ REGRESSOR POOL ----------------------------------------
# Use only backward-looking features (lags, past returns, z-scores).
# Exclude forward targets and the FX level itself.

fwd_cols <- grep("^fx_fwd_|^event_|^regime_|^prob_", names(wk), value=TRUE)
exclude  <- c("date","fx_sell","fx_level","log_fx", fwd_cols,
              "regime_label","regime_composite","pressure_index",
              "regime_transition")

candidates <- setdiff(names(wk), exclude)
candidates <- candidates[sapply(candidates, function(v) is.numeric(wk[[v]]))]

# Filter: keep columns with > 50% non-NA
cov_pct <- colMeans(!is.na(wk[, candidates, drop=FALSE]))
regs_pool <- names(cov_pct[cov_pct > 0.50])

# Cap at 40 to keep glmnet manageable
if (length(regs_pool) > 40)
  regs_pool <- names(sort(cov_pct[regs_pool], decreasing=TRUE))[1:40]

log_msg(paste("Regressor pool:", length(regs_pool), "features"))

# ------ BACKTEST: DIRECT FORECAST PER HORIZON -----------------

n_obs   <- nrow(wk)
origins <- seq(BT_MIN_TRAIN, n_obs - max(HORIZONS_W), by = BT_STEP)

results_multi <- list()

for (h in HORIZONS_W) {
  target_col <- paste0("fx_fwd_ret_", h, "w")
  if (!target_col %in% names(wk)) {
    log_msg(paste("Target", target_col, "not found, skipping"), "WARN")
    next
  }

  log_msg(paste("  Horizon", h, "w — target:", target_col))

  for (orig in origins) {
    if (orig + h > n_obs) next

    train_idx <- 1:orig
    y_train   <- wk[[target_col]][train_idx]
    X_train   <- as.matrix(wk[train_idx, regs_pool, drop=FALSE])
    X_train   <- impute_median(X_train)

    # Prediction point: latest available row
    X_new <- matrix(wk[orig, regs_pool, drop=TRUE], nrow=1)
    X_new <- impute_median(X_new)
    colnames(X_new) <- regs_pool

    actual_ret <- wk[[target_col]][orig]  # forward return known at t+h
    if (is.na(actual_ret)) next

    # --- Lasso (alpha=1) ---
    fc_lasso <- fit_glmnet_direct(X_train, y_train, X_new, alpha=1.0)

    # --- Ridge (alpha=0) ---
    fc_ridge <- fit_glmnet_direct(X_train, y_train, X_new, alpha=0.0)

    # --- Elastic Net (alpha=0.5) ---
    fc_enet  <- fit_glmnet_direct(X_train, y_train, X_new, alpha=0.5)

    # --- OLS with top N regressors (Local Projection style) ---
    lp_n <- min(10, length(regs_pool))
    lp_regs <- regs_pool[1:lp_n]
    df_lp <- data.frame(y = y_train, X_train[, lp_regs, drop=FALSE])
    lp_fit <- tryCatch(
      lm(y ~ ., data=df_lp, na.action=na.omit),
      error = function(e) NULL)
    lp_pred <- if (!is.null(lp_fit)) {
      nd <- as.data.frame(X_new[, lp_regs, drop=FALSE])
      names(nd) <- lp_regs
      tryCatch(as.numeric(predict(lp_fit, newdata=nd)), error=function(e) NA_real_)
    } else NA_real_

    forecasts <- list(
      lasso      = fc_lasso$point,
      ridge      = fc_ridge$point,
      enet       = fc_enet$point,
      local_proj = lp_pred
    )

    for (mname in names(forecasts)) {
      results_multi[[length(results_multi)+1]] <- tibble(
        origin      = orig,
        origin_date = wk$date[orig],
        horizon     = h,
        model       = mname,
        actual_fwd_ret    = actual_ret,
        predicted_fwd_ret = as.numeric(forecasts[[mname]])
      )
    }
  }
}

bt_multi <- bind_rows(results_multi)

# ------ METRICS -----------------------------------------------
metrics_multi <- bt_multi %>%
  filter(!is.na(predicted_fwd_ret)) %>%
  group_by(model, horizon) %>%
  summarise(
    n       = n(),
    rmse    = calc_rmse(actual_fwd_ret, predicted_fwd_ret),
    mae     = calc_mae(actual_fwd_ret, predicted_fwd_ret),
    dir_acc = calc_directional_accuracy(actual_fwd_ret, predicted_fwd_ret),
    .groups = "drop"
  ) %>%
  arrange(horizon, rmse)

# ------ VARIABLE IMPORTANCE (latest full-sample Lasso) --------
target_4w <- "fx_fwd_ret_4w"
if (target_4w %in% names(wk)) {
  X_full <- impute_median(as.matrix(wk[, regs_pool, drop=FALSE]))
  y_full <- wk[[target_4w]]
  ok <- !is.na(y_full)

  lasso_full <- tryCatch(
    glmnet::cv.glmnet(X_full[ok,], y_full[ok], alpha=1, nfolds=10),
    error = function(e) NULL)

  if (!is.null(lasso_full)) {
    coefs <- coef(lasso_full, s="lambda.min")
    vi <- tibble(variable=rownames(coefs)[-1],
                  coefficient=as.numeric(coefs[-1,1])) %>%
      filter(coefficient != 0) %>%
      arrange(desc(abs(coefficient)))
    readr::write_csv(vi, "output/tables/variable_importance_lasso.csv")
    log_msg(paste("Lasso selected", nrow(vi), "vars at 4w horizon"))
  }
}

# ------ SAVE --------------------------------------------------
saveRDS(bt_multi, "models/bt_multivariate_weekly.rds")
readr::write_csv(metrics_multi, "output/tables/multivariate_metrics_weekly.csv")
log_msg("Multivariate metrics:"); print(metrics_multi)
log_msg("=== 12 done ===")
