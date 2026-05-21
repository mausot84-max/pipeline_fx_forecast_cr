# ============================================================
# 23_islmbp_simulate.R — Simulación contrafactual baseline vs.
# tres escenarios del choque de Ormuz (motor de cuatro cuadrantes)
# ============================================================
# Lee:
#   eq_estimates_standard.rds
#   exog_path_baseline.rds + exog_path_scenario_{A,B,C}.rds
#   panel_islmbp_monthly.rds (estado inicial)
#   islmbp_specs.yml (pesos PIB)
#
# Produce:
#   data_intermediate/islmbp/simulations.rds
#   output/tables/islmbp_trajectories.csv
#   output/tables/islmbp_silent_tax_revealed.csv
#   output/tables/islmbp_revealed_summary.csv
# ============================================================

log_msg("=== 23: ISLMBP counterfactual simulation (4 cuadrantes) ===")

# ------ 1. CARGAR INSUMOS ---------------------------------------
panel <- readRDS("data_intermediate/islmbp/panel_islmbp_monthly.rds")
eq    <- readRDS("data_intermediate/islmbp/eq_estimates_standard.rds")

baseline_path  <- readRDS("data_intermediate/islmbp/exog_path_baseline.rds")
scenarios_avail <- c("A","B","C")
shock_paths <- lapply(scenarios_avail, function(id)
  readRDS(paste0("data_intermediate/islmbp/exog_path_scenario_", id, ".rds")))
names(shock_paths) <- scenarios_avail

# Pesos PIB por cuadrante
pw <- islmbp_specs$pib_weights %||% list(TV=0.20, TH=0.30, NH=0.30, NB=0.20)
pib_weights <- c(TV = pw$TV, TH = pw$TH, NH = pw$NH, NB = pw$NB)
log_msg(paste("Pesos PIB:",
              paste(names(pib_weights), round(pib_weights, 2), sep="=", collapse=", ")))

# ------ 2. ESTADO INICIAL ---------------------------------------
sim_start_date <- as.Date(islmbp_specs$simulation$baseline_start %||% "2026-01-01")

init_row <- panel %>%
  filter(date <= sim_start_date) %>%
  slice_tail(n = 1)

if (nrow(init_row) == 0) {
  init_row <- panel %>% slice_tail(n = 1)
  log_msg(paste("ADVERTENCIA: sim_start_date no en muestra; usando última obs:",
                format(init_row$date)), "WARN")
}

log_msg(paste("Estado inicial:", format(init_row$date)))

initial_state <- list(
  y_TV_yoy_log     = init_row$y_TV_yoy_log,
  y_TH_yoy_log     = init_row$y_TH_yoy_log,
  y_NH_yoy_log     = init_row$y_NH_yoy_log,
  y_NB_yoy_log     = init_row$y_NB_yoy_log,
  tpm              = init_row$tpm,
  inflation_yoy    = init_row$inflation_yoy,
  inflation_yoy_d1 = init_row$inflation_yoy_d1 %||% 0,
  fx_sell_yoy_log  = init_row$fx_sell_yoy_log,
  itcer_yoy_log    = init_row$itcer_yoy_log,
  r_real           = init_row$r_real,
  output_gap       = init_row$output_gap %||% 0,
  cr_col_yoy_log   = init_row$cr_col_yoy_log %||% 0,
  q_gap            = init_row$q_gap %||% 0,
  us_ip_yoy_log    = init_row$us_ip_yoy_log %||% 0.02,
  fedfunds         = init_row$fedfunds %||% 4.25
)

for (nm in names(initial_state)) {
  if (is.na(initial_state[[nm]])) {
    log_msg(paste("Estado inicial NA en", nm, "— reemplazando por 0"), "WARN")
    initial_state[[nm]] <- 0
  }
}

H <- nrow(baseline_path)

# ------ 3. SIMULAR BASELINE -------------------------------------
log_msg("Simulando baseline (sin choque)...")
sim_baseline <- simulate_counterfactual(eq, initial_state, baseline_path,
                                         horizon = H, pib_weights = pib_weights)
sim_baseline$scenario <- "baseline"

# ------ 4. SIMULAR LOS TRES ESCENARIOS --------------------------
sim_shocks <- lapply(scenarios_avail, function(id) {
  log_msg(paste("Simulando escenario", id, "..."))
  s <- simulate_counterfactual(eq, initial_state, shock_paths[[id]],
                                horizon = H, pib_weights = pib_weights)
  s$scenario <- paste0("shock_", id)
  s
})

# ------ 5. APILAR Y GUARDAR -------------------------------------
all_sims <- bind_rows(c(list(sim_baseline), sim_shocks)) %>%
  mutate(start_date = sim_start_date,
         sim_date   = sim_start_date %m+% months(h))

saveRDS(all_sims, "data_intermediate/islmbp/simulations.rds")
readr::write_csv(all_sims, "output/tables/islmbp_trajectories.csv")
log_msg(paste("Trayectorias guardadas:", nrow(all_sims), "filas x",
              ncol(all_sims), "vars"))

# ------ 6. IMPUESTO SILENCIOSO REVELADO --------------------------
revealed <- all_sims %>%
  filter(scenario != "baseline") %>%
  select(scenario, h,
         y_TV_shock     = y_TV_yoy_log,
         y_TH_shock     = y_TH_yoy_log,
         y_NH_shock     = y_NH_yoy_log,
         y_NB_shock     = y_NB_yoy_log,
         squeeze_shock  = tax_squeeze_TV,
         subsidy_shock  = tax_subsidy_NB,
         amplitude_shock= silent_tax_amplitude,
         tpm_shock      = tpm,
         inflation_shock= inflation_yoy,
         fx_shock       = fx_sell_yoy_log) %>%
  left_join(
    all_sims %>% filter(scenario == "baseline") %>%
      select(h,
             y_TV_base     = y_TV_yoy_log,
             y_TH_base     = y_TH_yoy_log,
             y_NH_base     = y_NH_yoy_log,
             y_NB_base     = y_NB_yoy_log,
             squeeze_base  = tax_squeeze_TV,
             subsidy_base  = tax_subsidy_NB,
             amplitude_base= silent_tax_amplitude,
             tpm_base      = tpm,
             inflation_base= inflation_yoy,
             fx_base       = fx_sell_yoy_log),
    by = "h"
  ) %>%
  mutate(
    d_y_TV       = y_TV_shock      - y_TV_base,
    d_y_TH       = y_TH_shock      - y_TH_base,
    d_y_NH       = y_NH_shock      - y_NH_base,
    d_y_NB       = y_NB_shock      - y_NB_base,
    d_squeeze    = squeeze_shock   - squeeze_base,
    d_subsidy    = subsidy_shock   - subsidy_base,
    d_amplitude  = amplitude_shock - amplitude_base,
    d_tpm        = tpm_shock       - tpm_base,
    d_inflation  = inflation_shock - inflation_base,
    d_fx         = fx_shock        - fx_base
  )

readr::write_csv(revealed, "output/tables/islmbp_silent_tax_revealed.csv")

# Resumen a horizontes clave
key_h <- c(3, 6, 9, 12, 18)
summary_revealed <- revealed %>%
  filter(h %in% key_h) %>%
  select(scenario, h,
         d_y_TV, d_y_TH, d_y_NH, d_y_NB,
         d_squeeze, d_subsidy, d_amplitude,
         d_tpm, d_inflation, d_fx) %>%
  arrange(scenario, h)

readr::write_csv(summary_revealed, "output/tables/islmbp_revealed_summary.csv")
log_msg("Impuesto silencioso revelado (diferencias vs. baseline):")
print(summary_revealed, n = Inf)

log_msg("=== 23 done ===")
