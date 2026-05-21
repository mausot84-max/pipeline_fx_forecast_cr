# ============================================================
# 24_islmbp_tables.R — Tablas finales para el paper Lizano
# (motor de cuatro cuadrantes)
# ============================================================
# Produce:
#   T1. Coeficientes estimados de las 7 ecuaciones
#   T2. Comparación Taylor estándar vs. extendida
#   T3. Tests Chow de estabilidad
#   T4. Escenarios del choque
#   T5a. Impuesto silencioso revelado por cuadrante (Δy por cuadrante)
#   T5b. Impuesto silencioso revelado: métricas integradas
#   T6. Matriz de cuadrantes (provenance del mapeo sectorial)
#   T7. Validación de signos esperados vs. observados
#   Hechos estilizados (Sección 2 del paper)
# ============================================================

log_msg("=== 24: ISLMBP tables (4 cuadrantes) ===")

# ------ T6. Matriz sectorial / mapeo de cuadrantes --------------
mapping_file <- "data_intermediate/islmbp/quadrant_mapping.csv"
if (file.exists(mapping_file)) {
  tabla6 <- readr::read_csv(mapping_file, show_col_types = FALSE)
  readr::write_csv(tabla6, "output/tables/paper_tabla6_cuadrante_mapping.csv")
} else {
  log_msg("Mapeo de cuadrantes no disponible.", "WARN")
}

# ------ T1. Coeficientes (variante estándar) --------------------
coefs_full <- readr::read_csv("output/tables/islmbp_coefficients.csv",
                                show_col_types = FALSE)
tabla1 <- coefs_full %>%
  filter(taylor_variant == "standard") %>%
  select(equation, term, estimate, std_error, statistic, p_value, stars, n, r2)
readr::write_csv(tabla1, "output/tables/paper_tabla1_coeficientes.csv")

# ------ T2. Taylor estándar vs. extendida -----------------------
tabla2 <- coefs_full %>%
  filter(equation == "taylor") %>%
  select(taylor_variant, term, estimate, std_error, p_value, stars, n, r2)
readr::write_csv(tabla2, "output/tables/paper_tabla2_taylor_compare.csv")

# ------ T3. Tests Chow ------------------------------------------
chow <- readr::read_csv("output/tables/islmbp_chow_stability.csv",
                          show_col_types = FALSE)
tabla3 <- chow %>% select(equation, break_date, F, p, note)
readr::write_csv(tabla3, "output/tables/paper_tabla3_chow.csv")

# ------ T4. Escenarios del choque -------------------------------
tabla4 <- readr::read_csv("output/tables/islmbp_shock_scenarios.csv",
                            show_col_types = FALSE)
readr::write_csv(tabla4, "output/tables/paper_tabla4_escenarios.csv")

# ------ T5a/T5b. Impuesto silencioso revelado -------------------
revealed <- readr::read_csv("output/tables/islmbp_revealed_summary.csv",
                              show_col_types = FALSE)

tabla5a <- revealed %>%
  mutate(
    scenario_label = case_when(
      scenario == "shock_A" ~ "A — Moderado",
      scenario == "shock_B" ~ "B — Medio",
      scenario == "shock_C" ~ "C — Severo",
      TRUE                  ~ scenario
    ),
    horizon = paste0("h+", h, "m"),
    d_y_TV_pp = round(d_y_TV * 100, 2),
    d_y_TH_pp = round(d_y_TH * 100, 2),
    d_y_NH_pp = round(d_y_NH * 100, 2),
    d_y_NB_pp = round(d_y_NB * 100, 2)
  ) %>%
  select(scenario_label, horizon,
         d_y_TV_pp, d_y_TH_pp, d_y_NH_pp, d_y_NB_pp)
readr::write_csv(tabla5a, "output/tables/paper_tabla5a_cuadrantes.csv")

tabla5b <- revealed %>%
  mutate(
    scenario_label = case_when(
      scenario == "shock_A" ~ "A — Moderado",
      scenario == "shock_B" ~ "B — Medio",
      scenario == "shock_C" ~ "C — Severo",
      TRUE                  ~ scenario
    ),
    horizon = paste0("h+", h, "m"),
    d_squeeze_pp   = round(d_squeeze   * 100, 2),
    d_subsidy_pp   = round(d_subsidy   * 100, 2),
    d_amplitude_pp = round(d_amplitude * 100, 2),
    d_tpm_pp       = round(d_tpm, 2),
    d_inflation_pp = round(d_inflation, 2),
    d_fx_pp        = round(d_fx * 100, 2)
  ) %>%
  select(scenario_label, horizon,
         d_squeeze_pp, d_subsidy_pp, d_amplitude_pp,
         d_tpm_pp, d_inflation_pp, d_fx_pp)
readr::write_csv(tabla5b, "output/tables/paper_tabla5b_metricas.csv")

# ------ T7. Validación de signos --------------------------------
val_file <- "output/tables/islmbp_sign_validation.csv"
if (file.exists(val_file)) {
  tabla7 <- readr::read_csv(val_file, show_col_types = FALSE)
  readr::write_csv(tabla7, "output/tables/paper_tabla7_signos.csv")
}

# ------ HECHOS ESTILIZADOS (Sección 2) --------------------------
hechos <- tribble(
  ~indicador,                       ~valor_obs, ~periodo,                  ~fuente,
  "ITCER promedio",                 104,        "2000-2005",               "BCCR",
  "ITCER promedio",                 83,         "2015-2019",               "BCCR",
  "ITCER fin de periodo",           77,         "diciembre 2025",          "BCCR",
  "Tipo de cambio nominal",         689,        "pico 2022",               "BCCR",
  "Tipo de cambio nominal",         500,        "diciembre 2025",          "BCCR",
  "Inflacion i.a.",                 -1.2,       "diciembre 2025",          "BCCR",
  "TPM real ex-post promedio",      5.2,        "2023-2025",               "BCCR",
  "IMAE TIC (cuadrante TH)",        67,         "2015-2025 % crecim.",     "BCCR (CIIU J)",
  "IMAE Profesional (TH)",          109,        "2015-2025 % crecim.",     "BCCR (CIIU M-N)",
  "IMAE Manufactura (mixto)",       63,         "2015-2025 % crecim.",     "BCCR (CIIU C)",
  "IMAE Construccion (TV)",         -13,        "2015-2025 % crecim.",     "BCCR (CIIU F)",
  "IMAE Alojamiento (TV)",          3,          "2015-2025 % crecim.",     "BCCR (CIIU I)",
  "IMAE Agropecuario (TV)",         18,         "2015-2025 % crecim.",     "BCCR (CIIU A)",
  "IMAE Comercio (NB)",             22,         "2015-2025 % crecim.",     "BCCR (CIIU G)",
  "Indice externo dinamico",        79,         "2015-2025 % crecim.",     "Calculo propio",
  "Indice domestico col-int.",      13,         "2015-2025 % crecim.",     "Calculo propio",
  "Cartera credito USD expuesta",   69.29,      "21 enero 2025 % total",   "SUGEF / La Nacion"
)
readr::write_csv(hechos, "output/tables/paper_hechos_estilizados.csv")

log_msg("Tablas para paper Lizano (output/tables/paper_*.csv):")
log_msg("  - paper_hechos_estilizados.csv")
log_msg("  - paper_tabla1_coeficientes.csv")
log_msg("  - paper_tabla2_taylor_compare.csv")
log_msg("  - paper_tabla3_chow.csv")
log_msg("  - paper_tabla4_escenarios.csv")
log_msg("  - paper_tabla5a_cuadrantes.csv")
log_msg("  - paper_tabla5b_metricas.csv")
log_msg("  - paper_tabla6_cuadrante_mapping.csv")
log_msg("  - paper_tabla7_signos.csv")

log_msg("=== 24 done ===")
