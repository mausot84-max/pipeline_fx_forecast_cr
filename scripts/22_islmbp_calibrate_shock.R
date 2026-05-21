# ============================================================
# 22_islmbp_calibrate_shock.R — Sendas de exógenas para los tres
# escenarios del choque de Ormuz
# ============================================================
# Lee shock_scenarios.yml y produce, para cada escenario y para
# el baseline, una tibble mensual con la senda de variables
# exógenas necesarias para la simulación:
#
#   us_ip_yoy_log    — demanda externa
#   tot_yoy_log      — términos de intercambio
#   wti_yoy_log      — petróleo (proxy de canal de costos)
#   vix_d1           — choque de riesgo
#   fedfunds         — respuesta de la Fed
#   ipc_ext_yoy_log  — inflación externa (para identidad ITCER)
#
# Outputs:
#   data_intermediate/islmbp/exog_path_baseline.rds
#   data_intermediate/islmbp/exog_path_scenario_A.rds
#   data_intermediate/islmbp/exog_path_scenario_B.rds
#   data_intermediate/islmbp/exog_path_scenario_C.rds
# ============================================================

log_msg("=== 22: ISLMBP shock calibration ===")

if (!exists("shock_scenarios_cfg"))
  stop("[ISLMBP] config/shock_scenarios.yml no cargado. Revisar 00_setup.R")

base <- shock_scenarios_cfg$baseline
H <- islmbp_specs$simulation$horizon_months %||% 18L
decay <- islmbp_specs$simulation$decay_months %||% 6L

# ------ 1. BASELINE PATH ----------------------------------------
# Sin choque: las exógenas se mantienen en su nivel pre-Ormuz
# durante todo el horizonte H.

baseline_path <- tibble::tibble(
  h               = 1:H,
  us_ip_yoy_log   = 0.02,                  # crecimiento externo modesto ~2%
  tot_yoy_log     = 0.00,                  # ToT estables
  wti_yoy_log     = log(base$brent_usd) - log(base$brent_usd),  # = 0
  vix_d1          = 0.00,
  fedfunds        = base$fedfunds,
  ipc_ext_yoy_log = base$ipc_ext_yoy %||% 0.025
)

saveRDS(baseline_path, "data_intermediate/islmbp/exog_path_baseline.rds")
log_msg("Baseline path guardado.")

# ------ 2. SHOCK PATHS PARA A, B, C -----------------------------
# Estructura común: durante `duration_months`, las exógenas se
# desplazan según el escenario.  Después, retornan linealmente
# al baseline a lo largo de `decay` meses.

build_shock_path <- function(sc, H_total, decay_m, base_cfg) {
  p <- sc$parameters
  dur <- as.integer(sc$duration_months)
  if (dur + decay_m > H_total) dur <- max(1, H_total - decay_m)
  if (dur < 1) dur <- 1

  # Niveles del choque
  wti_log_delta <- log(p$brent_usd) - log(base_cfg$brent_usd)
  tot_log_delta <- log(1 + p$tot_delta)  # tot_delta es proporción
  vix_jump      <- p$vix_delta
  ff_delta      <- p$fedfunds_delta

  # Senda mensual con decay lineal:
  #   - Meses 1..dur:           peso = 1 (choque pleno)
  #   - Meses dur+1..dur+decay: decae linealmente de 1 a 0
  #   - Meses dur+decay+1..H:   peso = 0
  decay_w <- numeric(H_total)
  for (i in seq_len(H_total)) {
    if (i <= dur) {
      decay_w[i] <- 1
    } else if (i <= dur + decay_m) {
      decay_w[i] <- 1 - (i - dur) / decay_m
    } else {
      decay_w[i] <- 0
    }
  }

  tibble::tibble(
    h               = 1:H_total,
    us_ip_yoy_log   = 0.02 - 0.015 * decay_w,            # demanda ext. cae
    tot_yoy_log     = tot_log_delta * decay_w,
    wti_yoy_log     = wti_log_delta * decay_w,
    vix_d1          = c(vix_jump, rep(0, H_total - 1)) * decay_w,
    fedfunds        = base_cfg$fedfunds + ff_delta * decay_w,
    ipc_ext_yoy_log = (base_cfg$ipc_ext_yoy %||% 0.025) +
                       0.5 * wti_log_delta * decay_w     # IPC externo sube por oil
  )
}

for (sc in shock_scenarios_cfg$scenarios) {
  pth <- build_shock_path(sc, H, decay, base)
  fname <- paste0("data_intermediate/islmbp/exog_path_scenario_", sc$id, ".rds")
  saveRDS(pth, fname)
  log_msg(sprintf("Escenario %s (%s) guardado: dur=%d, Brent=%d, ΔToT=%.2f, ΔVIX=%d",
                  sc$id, sc$label,
                  sc$duration_months,
                  sc$parameters$brent_usd,
                  sc$parameters$tot_delta,
                  sc$parameters$vix_delta))
}

# ------ 3. RESUMEN COMPARATIVO ----------------------------------
summary_tbl <- tibble::tibble(
  id              = sapply(shock_scenarios_cfg$scenarios, function(s) s$id),
  label           = sapply(shock_scenarios_cfg$scenarios, function(s) s$label),
  brent_usd       = sapply(shock_scenarios_cfg$scenarios, function(s) s$parameters$brent_usd),
  duration_months = sapply(shock_scenarios_cfg$scenarios, function(s) s$duration_months),
  tot_delta       = sapply(shock_scenarios_cfg$scenarios, function(s) s$parameters$tot_delta),
  vix_delta       = sapply(shock_scenarios_cfg$scenarios, function(s) s$parameters$vix_delta),
  fedfunds_delta  = sapply(shock_scenarios_cfg$scenarios, function(s) s$parameters$fedfunds_delta),
  risk_premium_bp = sapply(shock_scenarios_cfg$scenarios, function(s) s$parameters$risk_premium_bp)
)
readr::write_csv(summary_tbl, "output/tables/islmbp_shock_scenarios.csv")
log_msg("Resumen de escenarios:")
print(summary_tbl)

log_msg("=== 22 done ===")
