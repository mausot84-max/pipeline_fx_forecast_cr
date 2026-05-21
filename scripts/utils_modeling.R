# ============================================================
# utils_modeling.R — Modelling & backtesting utilities
# ============================================================
# KEY DESIGN DECISIONS:
#   1. DIRECT FORECASTING: each model trains on the h-step-
#      ahead target directly (Jorda-style).  No iterated 1-step.
#   2. DIRECTIONAL ACCURACY: sign(predicted_fwd_return_h)
#      vs sign(actual_fwd_return_h).  Economically interpretable.
#   3. CONDITIONAL RISK: quantile regression OOS gives the
#      conditional downside/upside at each forecast origin.
# ============================================================

library(dplyr)
library(forecast)
library(glmnet)
library(quantreg)

# ==============================================================
# BENCHMARK FORECASTERS
# ==============================================================

# All benchmarks produce an h-step point forecast for the FX
# LEVEL.  The backtest wrapper converts to returns for comparison.

forecast_rw <- function(y_train, h = 1) {
  list(point = rep(tail(y_train, 1), h), model = "rw")
}

forecast_rw_drift <- function(y_train, h = 1) {
  n <- length(y_train)
  drift <- (y_train[n] - y_train[1]) / (n - 1)
  list(point = tail(y_train, 1) + drift * (1:h), model = "rw_drift")
}

forecast_ma <- function(y_train, h = 1, window = 4) {
  ma <- mean(tail(y_train, window), na.rm = TRUE)
  list(point = rep(ma, h), model = paste0("ma_", window))
}

forecast_arima_auto <- function(y_train, h = 1) {
  fit <- tryCatch(
    forecast::auto.arima(y_train, max.p=5, max.q=5, max.d=2,
                          seasonal=FALSE, stepwise=TRUE),
    error = function(e) NULL)
  if (is.null(fit))
    return(list(point = rep(NA_real_, h), model = "arima_fail"))

  fc <- forecast::forecast(fit, h = h)
  list(
    point    = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[,1]),
    upper_80 = as.numeric(fc$upper[,1]),
    lower_95 = as.numeric(fc$lower[,2]),
    upper_95 = as.numeric(fc$upper[,2]),
    model    = paste0("arima_", paste(fit$arma[c(1,6,2)], collapse=","))
  )
}

forecast_ets <- function(y_train, h = 1) {
  fit <- tryCatch(forecast::ets(y_train), error = function(e) NULL)
  if (is.null(fit))
    return(list(point = rep(NA_real_, h), model = "ets_fail"))
  fc <- forecast::forecast(fit, h = h)
  list(point = as.numeric(fc$mean),
       lower_95 = as.numeric(fc$lower[,2]),
       upper_95 = as.numeric(fc$upper[,2]),
       model = fit$method)
}

# ==============================================================
# PENALISED REGRESSION (direct h-step forecast)
# ==============================================================

fit_glmnet_direct <- function(X_train, y_train, X_new,
                               alpha = 1.0, nfolds = 5) {
  # y_train is the h-step-ahead forward return at each training row
  ok <- complete.cases(X_train) & !is.na(y_train)
  if (sum(ok) < 30)
    return(list(point = NA_real_, model = paste0("glmnet_a",alpha,"_fail"),
                coefs = NULL))

  Xc <- as.matrix(X_train[ok, , drop = FALSE])
  yc <- y_train[ok]

  fit <- tryCatch(
    glmnet::cv.glmnet(Xc, yc, alpha = alpha,
                       nfolds = min(nfolds, floor(length(yc)/3)),
                       type.measure = "mse"),
    error = function(e) NULL)

  if (is.null(fit))
    return(list(point = NA_real_, model = paste0("glmnet_a",alpha,"_fail"),
                coefs = NULL))

  pred <- as.numeric(predict(fit, newx = as.matrix(X_new), s = "lambda.min"))

  coefs_raw <- coef(fit, s = "lambda.min")
  list(point  = pred,
       model  = paste0("glmnet_a", alpha),
       lambda = fit$lambda.min,
       coefs  = coefs_raw)
}

# ==============================================================
# QUANTILE REGRESSION
# ==============================================================

fit_qr <- function(X_train, y_train, X_new, tau = 0.05) {
  ok <- complete.cases(X_train) & !is.na(y_train)
  if (sum(ok) < 30) return(NA_real_)

  df_train <- as.data.frame(cbind(y = y_train[ok], X_train[ok, , drop=FALSE]))
  fml <- as.formula(paste("y ~", paste(names(df_train)[-1], collapse=" + ")))

  fit <- tryCatch(
    quantreg::rq(fml, data = df_train, tau = tau),
    error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)

  df_new <- as.data.frame(X_new)
  names(df_new) <- names(df_train)[-1]

  tryCatch(as.numeric(predict(fit, newdata = df_new)),
           error = function(e) NA_real_)
}

# ==============================================================
# EVALUATION METRICS
# ==============================================================

calc_rmse <- function(a, p) {
  ok <- !is.na(a) & !is.na(p)
  if (sum(ok)==0) return(NA_real_)
  sqrt(mean((a[ok]-p[ok])^2))
}

calc_mae <- function(a, p) {
  ok <- !is.na(a) & !is.na(p)
  if (sum(ok)==0) return(NA_real_)
  mean(abs(a[ok]-p[ok]))
}

calc_mape <- function(a, p) {
  ok <- !is.na(a) & !is.na(p) & a != 0
  if (sum(ok)==0) return(NA_real_)
  mean(abs((a[ok]-p[ok])/a[ok])) * 100
}

# DIRECTIONAL ACCURACY
# For a given horizon h, compare:
#   sign(predicted forward return at h) vs sign(actual forward return at h)
# This answers: "did the model correctly predict the direction of the
# FX move over the next h weeks?"
calc_directional_accuracy <- function(actual_fwd_ret, predicted_fwd_ret) {
  ok <- !is.na(actual_fwd_ret) & !is.na(predicted_fwd_ret)
  if (sum(ok) < 5) return(NA_real_)
  mean(sign(actual_fwd_ret[ok]) == sign(predicted_fwd_ret[ok]))
}

calc_coverage <- function(actual, lower, upper) {
  ok <- !is.na(actual) & !is.na(lower) & !is.na(upper)
  if (sum(ok)==0) return(NA_real_)
  mean(actual[ok] >= lower[ok] & actual[ok] <= upper[ok])
}

calc_quantile_loss <- function(actual, predicted, tau) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok)==0) return(NA_real_)
  e <- actual[ok] - predicted[ok]
  mean(e * (tau - (e < 0)))
}

# ==============================================================
# IMPUTE HELPER (column-median, for glmnet)
# ==============================================================

impute_median <- function(X) {
  for (j in seq_len(ncol(X))) {
    nas <- is.na(X[, j])
    if (any(nas)) X[nas, j] <- median(X[, j], na.rm = TRUE)
  }
  X
}
