# ============================================================
# 09_construct_regime_indicators.R — Regime classification
# ============================================================
# IMPROVEMENTS IN THIS VERSION:
#   1. Smoothed pressure index (EMA with span=8 weeks)
#   2. Minimum persistence rule: regime must hold for >= 4
#      weeks before a transition is recorded
#   3. Rolling probabilities use wider window (20 weeks)
#   4. Chronology uses proper Date objects (fixes axis bug)
# ============================================================

log_msg("=== 09: Regime indicators ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")
mn <- readRDS("data_intermediate/features/features_monthly.rds")

# ==============================================================
# RULE-BASED REGIME (raw, before smoothing)
# ==============================================================

classify_regime_raw <- function(df) {
  df$regime_raw <- NA_integer_

  fr <- if ("fx_sell_ret12" %in% names(df)) df$fx_sell_ret12 else rep(NA, nrow(df))
  vx <- if ("vix" %in% names(df)) df$vix else rep(NA, nrow(df))
  dr <- if ("dxy_ret12" %in% names(df)) df$dxy_ret12 else rep(NA, nrow(df))

  for (i in seq_len(nrow(df))) {
    if (is.na(fr[i])) next
    if (fr[i] > 0.01 && ((!is.na(vx[i]) && vx[i] > 25) ||
                          (!is.na(dr[i]) && dr[i] > 0.03))) {
      df$regime_raw[i] <- 3L   # Stress
    } else if (fr[i] < -0.01) {
      df$regime_raw[i] <- 1L   # Abundance
    } else {
      df$regime_raw[i] <- 2L   # Compression
    }
  }
  df
}

wk <- classify_regime_raw(wk)

# ==============================================================
# MINIMUM PERSISTENCE SMOOTHING
# ==============================================================
# A regime change is only confirmed if the new state persists
# for at least MIN_PERSIST consecutive weeks. Otherwise the
# classification reverts to the previous confirmed regime.

apply_persistence <- function(regime_vec, min_persist = 4) {
  smoothed <- regime_vec
  n <- length(smoothed)
  if (n < min_persist) return(smoothed)

  confirmed <- smoothed[which(!is.na(smoothed))[1]]  # first non-NA
  run_state  <- confirmed
  run_count  <- 0

  for (i in seq_len(n)) {
    if (is.na(smoothed[i])) next

    if (smoothed[i] == run_state) {
      run_count <- run_count + 1
    } else {
      run_state <- smoothed[i]
      run_count <- 1
    }

    # Only update confirmed if run is long enough
    if (run_count >= min_persist) {
      confirmed <- run_state
    }
    smoothed[i] <- confirmed
  }
  smoothed
}

wk$regime_rules <- apply_persistence(wk$regime_raw, min_persist = 4)
wk$regime_label <- factor(wk$regime_rules, levels = 1:3,
                           labels = c("Abundancia", "Compresion", "Estres"))
wk$regime_transition <- c(NA_integer_,
                            as.integer(diff(wk$regime_rules) != 0))
wk$regime_transition[is.na(wk$regime_rules)] <- NA_integer_

log_msg(paste("Regime distribution (smoothed):",
              paste(names(table(wk$regime_label)),
                    table(wk$regime_label), sep="=", collapse=", ")))

# ==============================================================
# COMPOSITE PRESSURE INDEX (with EMA smoothing)
# ==============================================================

build_pressure_index <- function(df, cfg) {
  components <- cfg$composite_index$components
  df$pressure_raw <- 0
  n_used <- 0

  for (comp in components) {
    v <- comp$var
    if (!v %in% names(df) || all(is.na(df[[v]]))) {
      log_msg(paste("  Composite: SKIPPING", v, "(not available)"), "WARN")
      next
    }
    z <- (df[[v]] - mean(df[[v]], na.rm = TRUE)) / sd(df[[v]], na.rm = TRUE)
    if (comp$direction == "negative") z <- -z
    contrib <- z * comp$weight
    contrib[is.na(contrib)] <- 0
    df$pressure_raw <- df$pressure_raw + contrib
    n_used <- n_used + 1
    log_msg(paste("  Composite: using", v, "(w:", comp$weight, ")"))
  }

  if (n_used == 0) {
    df$pressure_raw <- NA_real_
    df$pressure_index <- NA_real_
    log_msg("Composite: NO components available", "WARN")
  } else {
    log_msg(paste("Composite built from", n_used, "components"))
    # EMA smoothing (span = 8 weeks -> alpha = 2/(8+1) ≈ 0.22)
    alpha <- 2 / (8 + 1)
    pi_smooth <- df$pressure_raw
    for (i in 2:length(pi_smooth)) {
      if (!is.na(pi_smooth[i]) && !is.na(pi_smooth[i-1]))
        pi_smooth[i] <- alpha * pi_smooth[i] + (1 - alpha) * pi_smooth[i-1]
    }
    df$pressure_index <- pi_smooth
  }

  thr_a <- cfg$composite_index$thresholds$abundance
  thr_s <- cfg$composite_index$thresholds$stress

  df$regime_composite <- case_when(
    df$pressure_index < thr_a ~ 1L,
    df$pressure_index > thr_s ~ 3L,
    TRUE ~ 2L
  )
  df
}

wk <- build_pressure_index(wk, regime_rules)

# ==============================================================
# SMOOTHED ROLLING REGIME PROBABILITIES (20-week window)
# ==============================================================

build_regime_probs <- function(df, window = 20) {
  df$prob_abundance   <- NA_real_
  df$prob_compression <- NA_real_
  df$prob_stress      <- NA_real_

  for (i in window:nrow(df)) {
    seg <- df$regime_rules[(i - window + 1):i]
    seg <- seg[!is.na(seg)]
    if (length(seg) == 0) next
    df$prob_abundance[i]   <- mean(seg == 1)
    df$prob_compression[i] <- mean(seg == 2)
    df$prob_stress[i]      <- mean(seg == 3)
  }
  df
}

wk <- build_regime_probs(wk, window = 20)

# ==============================================================
# EVENT TARGETS
# ==============================================================

wk <- build_event_targets(wk, regime_rules$events)

# ==============================================================
# APPLY TO MONTHLY (auxiliary)
# ==============================================================

mn <- classify_regime_raw(mn)
mn$regime_rules <- apply_persistence(mn$regime_raw, min_persist = 3)
mn$regime_label <- factor(mn$regime_rules, levels = 1:3,
                           labels = c("Abundancia", "Compresion", "Estres"))
mn <- build_pressure_index(mn, regime_rules)
mn <- build_regime_probs(mn, window = 6)

# ==============================================================
# REGIME CHRONOLOGY (with proper Date objects)
# ==============================================================

chrono <- wk %>%
  dplyr::filter(!is.na(regime_label)) %>%
  mutate(new_spell = is.na(lag(regime_rules)) | regime_rules != lag(regime_rules),
         spell_id  = cumsum(new_spell)) %>%
  group_by(spell_id) %>%
  summarise(
    regime     = first(as.character(regime_label)),
    start_date = as.Date(min(date)),     # ensure Date class
    end_date   = as.Date(max(date)),     # ensure Date class
    n_weeks    = n(),
    mean_fx    = mean(fx_sell, na.rm = TRUE),
    fx_change  = (last(fx_sell) - first(fx_sell)) / first(fx_sell),
    .groups = "drop"
  )

readr::write_csv(chrono, "data_intermediate/diagnostics/regime_chronology.csv")
log_msg(paste("Regime chronology:", nrow(chrono), "spells (smoothed)"))

# ------ SAVE --------------------------------------------------
saveRDS(wk, "data_intermediate/features/features_weekly.rds")
saveRDS(mn, "data_intermediate/features/features_monthly.rds")
log_msg("=== 09 done ===")
