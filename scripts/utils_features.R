# ============================================================
# utils_features.R — Feature engineering utilities
# ============================================================
# NAMING CONVENTION (used consistently across the pipeline):
#   Lags:        {var}_lag{n}          fx_sell_lag1
#   Diff:        {var}_d{n}            fx_sell_d1
#   Return:      {var}_ret{n}          fx_sell_ret12
#   Rolling vol: {var}_ret1_vol{n}     fx_sell_ret1_vol8
#   Rolling mean:{var}_ma{n}           fx_sell_ma12
#   Z-score:     {var}_z               vix_z
#   YoY:         {var}_yoy             rin_yoy
#   Drawdown:    {var}_dd{n}           fx_sell_dd52
#   Fwd target:  fx_fwd_ret_{h}w       fx_fwd_ret_4w
#   Events:      event_down_{h}w       event_down_4w
#   Spreads:     rate_diff_cr_us, term_spread
#
# WEEK DEFINITION: Friday close (financial convention).
# ============================================================

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)

# ==============================================================
# WEEKLY AGGREGATION — FRIDAY CLOSE
# ==============================================================

to_friday <- function(d) {

  # Map any date to the Friday of its week.
  # wday: 1=Sun .. 7=Sat (default lubridate)
  wd <- lubridate::wday(d)
  # Days to add/subtract to reach Friday (wday=6)
  offset <- (6L - wd) %% 7L
  # If Saturday (7), go back 1; if Sunday (1), go forward 5
  # Actually: we want the PREVIOUS or SAME Friday.
  # For days Mon-Fri: map to that Friday.
  # For Sat-Sun: map to the preceding Friday.
  adj <- ifelse(wd == 7L, -1L,         # Saturday -> prev Friday
         ifelse(wd == 1L, -2L,          # Sunday   -> prev Friday
                6L - wd))               # Mon-Fri  -> that week's Friday
  d + adj
}

aggregate_to_weekly <- function(df, date_col = "date",
                                 value_col = "value",
                                 method = "last") {
  df %>%
    mutate(week_end = to_friday(.data[[date_col]])) %>%
    group_by(week_end) %>%
    summarise(
      value = switch(method,
        "last"  = dplyr::last(na.omit(.data[[value_col]])),
        "mean"  = mean(.data[[value_col]], na.rm = TRUE),
        "sum"   = sum(.data[[value_col]], na.rm = TRUE),
        "first" = dplyr::first(na.omit(.data[[value_col]])),
        dplyr::last(na.omit(.data[[value_col]]))
      ),
      .groups = "drop"
    ) %>%
    rename(date = week_end) %>%
    arrange(date)
}

aggregate_to_monthly <- function(df, date_col = "date",
                                  value_col = "value",
                                  method = "last") {
  df %>%
    mutate(month_end = lubridate::ceiling_date(.data[[date_col]],
                                                "month") - 1) %>%
    group_by(month_end) %>%
    summarise(
      value = switch(method,
        "last"  = dplyr::last(na.omit(.data[[value_col]])),
        "mean"  = mean(.data[[value_col]], na.rm = TRUE),
        "sum"   = sum(.data[[value_col]], na.rm = TRUE),
        dplyr::last(na.omit(.data[[value_col]]))
      ),
      .groups = "drop"
    ) %>%
    rename(date = month_end) %>%
    arrange(date)
}

# ==============================================================
# BASIC TRANSFORMS
# ==============================================================

add_lags <- function(df, var, lags = 1:4) {
  for (k in lags) {
    df[[paste0(var, "_lag", k)]] <- dplyr::lag(df[[var]], n = k)
  }
  df
}

add_diff <- function(df, var, n = 1) {
  df[[paste0(var, "_d", n)]] <- c(rep(NA_real_, n), diff(df[[var]], differences = n))
  df
}

add_return <- function(df, var, n = 1) {
  # Percentage return over n periods: (x_t - x_{t-n}) / x_{t-n}
  col <- paste0(var, "_ret", n)
  vals <- df[[var]]
  df[[col]] <- c(rep(NA_real_, n),
                  (vals[(n+1):length(vals)] - vals[1:(length(vals)-n)]) /
                    abs(vals[1:(length(vals)-n)]))
  df
}

add_yoy <- function(df, var, freq_per_year = 12) {
  # Year-on-year change
  col <- paste0(var, "_yoy")
  vals <- df[[var]]
  n <- length(vals)
  if (n <= freq_per_year) { df[[col]] <- NA_real_; return(df) }
  df[[col]] <- c(rep(NA_real_, freq_per_year),
                  (vals[(freq_per_year+1):n] - vals[1:(n-freq_per_year)]) /
                    abs(vals[1:(n-freq_per_year)]))
  df
}

# ==============================================================
# ROLLING STATISTICS
# ==============================================================

add_rolling_mean <- function(df, var, windows = c(4,8,12)) {
  for (w in windows) {
    df[[paste0(var, "_ma", w)]] <- zoo::rollmean(df[[var]], k = w,
                                                   fill = NA, align = "right")
  }
  df
}

add_rolling_sd <- function(df, var, windows = c(4,8,12)) {
  for (w in windows) {
    df[[paste0(var, "_vol", w)]] <- zoo::rollapply(df[[var]], width = w,
                                                     FUN = sd, fill = NA,
                                                     align = "right")
  }
  df
}

add_rolling_drawdown <- function(df, var, window = 52) {
  col <- paste0(var, "_dd", window)
  vals <- df[[var]]
  dd <- rep(NA_real_, length(vals))
  for (i in window:length(vals)) {
    seg <- vals[(i-window+1):i]
    peak <- cummax(seg)
    dd[i] <- min((seg - peak) / peak, na.rm = TRUE)
  }
  df[[col]] <- dd
  df
}

# ==============================================================
# Z-SCORES
# ==============================================================

add_zscore <- function(df, var, window = NULL) {
  col <- paste0(var, "_z")
  if (is.null(window)) {
    df[[col]] <- (df[[var]] - mean(df[[var]], na.rm=TRUE)) /
      sd(df[[var]], na.rm=TRUE)
  } else {
    df[[col]] <- zoo::rollapply(
      df[[var]], width = window,
      FUN = function(x) (x[length(x)] - mean(x, na.rm=TRUE)) /
        sd(x, na.rm=TRUE),
      fill = NA, align = "right"
    )
  }
  df
}

# ==============================================================
# TARGETS — FORWARD-LOOKING
# ==============================================================

build_forward_targets <- function(df, fx_var = "fx_sell",
                                   horizons = c(1,4,8,12,24)) {
  df$fx_level <- df[[fx_var]]
  df$log_fx   <- log(df[[fx_var]])

  for (h in horizons) {
    col <- paste0("fx_fwd_ret_", h, "w")
    vals <- df[[fx_var]]
    n <- length(vals)
    if (n <= h) { df[[col]] <- NA_real_; next }
    # Forward return: (x_{t+h} - x_t) / x_t
    df[[col]] <- c((vals[(h+1):n] - vals[1:(n-h)]) / vals[1:(n-h)],
                    rep(NA_real_, h))
  }
  df
}

build_event_targets <- function(df, event_cfg) {
  for (direction in c("downside","upside")) {
    thresholds <- event_cfg[[direction]]$thresholds
    for (h_label in names(thresholds)) {
      thresh <- thresholds[[h_label]]
      fwd_col <- paste0("fx_fwd_ret_", h_label)
      evt_col <- paste0("event_", substr(direction,1,4), "_", h_label)

      if (fwd_col %in% names(df)) {
        if (direction == "downside") {
          df[[evt_col]] <- as.integer(df[[fwd_col]] < thresh)
        } else {
          df[[evt_col]] <- as.integer(df[[fwd_col]] > thresh)
        }
      }
    }
  }
  df
}

# ==============================================================
# SPREADS
# ==============================================================

add_spread <- function(df, var1, var2, name) {
  if (all(c(var1, var2) %in% names(df)))
    df[[name]] <- df[[var1]] - df[[var2]]
  df
}

# ==============================================================
# PCA FEATURES
# ==============================================================

compute_pca_features <- function(df, vars, n_comp = 3, prefix = "fin_pc") {
  mat <- as.matrix(df[, vars, drop = FALSE])
  ok <- complete.cases(mat)
  if (sum(ok) < n_comp * 5) {
    warning("[PCA] Too few complete obs"); return(df)
  }
  pca <- prcomp(scale(mat[ok, ]), center = FALSE, scale. = FALSE)
  scores <- matrix(NA_real_, nrow(df), n_comp)
  scores[ok, ] <- pca$x[, 1:n_comp]
  for (j in 1:n_comp) df[[paste0(prefix, j)]] <- scores[, j]
  attr(df, paste0(prefix, "_loadings")) <- pca$rotation[, 1:n_comp]
  df
}

# ==============================================================
# MISSINGNESS REPORT
# ==============================================================

missingness_report <- function(df, date_col = "date") {
  vars <- setdiff(names(df), date_col)
  tibble(
    variable    = vars,
    n_total     = nrow(df),
    n_valid     = sapply(vars, function(v) sum(!is.na(df[[v]]))),
    pct_missing = round((1 - n_valid / n_total) * 100, 1),
    first_valid = sapply(vars, function(v) {
      idx <- which(!is.na(df[[v]]))[1]
      if (is.na(idx)) NA_character_ else as.character(df[[date_col]][idx])
    }),
    last_valid  = sapply(vars, function(v) {
      idx <- rev(which(!is.na(df[[v]])))[1]
      if (is.na(idx)) NA_character_ else as.character(df[[date_col]][idx])
    })
  ) %>% arrange(desc(pct_missing))
}
