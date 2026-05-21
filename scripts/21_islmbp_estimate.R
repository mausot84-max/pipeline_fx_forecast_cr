# ============================================================
# 21_islmbp_estimate.R — Estimación del IS-LM-BP de cuatro cuadrantes
# ============================================================
# Estima las 7 ecuaciones del motor (4 IS sectoriales + Taylor +
# Phillips + FX) con OLS+HAC Newey-West.  Estima dos variantes
# de la regla de Taylor (con y sin q_gap).
#
# Outputs:
#   data_intermediate/islmbp/eq_estimates_standard.rds
#   data_intermediate/islmbp/eq_estimates_extended.rds
#   output/tables/islmbp_coefficients.csv
#   output/tables/islmbp_chow_stability.csv
#   output/tables/islmbp_taylor_compare.csv
# ============================================================

log_msg("=== 21: ISLMBP estimation (4 cuadrantes) ===")

panel <- readRDS("data_intermediate/islmbp/panel_islmbp_monthly.rds")
log_msg(paste("Panel:", nrow(panel), "obs"))

# ------ 1. ESTIMAR LAS 7 ECUACIONES × 2 VARIANTES TAYLOR ---------
log_msg("Estimando las 7 ecuaciones con Taylor ESTÁNDAR...")
eq_standard <- estimate_all(panel, include_q_gap = FALSE)

log_msg("Estimando las 7 ecuaciones con Taylor EXTENDIDA (con q_gap)...")
eq_extended <- estimate_all(panel, include_q_gap = TRUE)

# ------ 2. TABLA DE COEFICIENTES --------------------------------
tbl_std <- format_coefs_table(eq_standard) %>% mutate(taylor_variant = "standard")
tbl_ext <- format_coefs_table(eq_extended) %>% mutate(taylor_variant = "extended")
tbl_all <- bind_rows(tbl_std, tbl_ext) %>%
  select(taylor_variant, equation, term, estimate, std_error, statistic,
         p_value, stars, n, r2)

readr::write_csv(tbl_all, "output/tables/islmbp_coefficients.csv")
log_msg("Coeficientes guardados.")
print(tbl_all, n = Inf)

# ------ 3. SIGNOS ESPERADOS Y VALIDACIÓN ------------------------
# Tabla de signos esperados según la hipótesis de cuadrantes.
expected_signs <- tribble(
  ~equation, ~term,                ~expected_sign, ~rationale,
  "IS_TV",   "us_ip_yoy_log",      "+",            "demanda externa alimenta exportadores TV",
  "IS_TV",   "itcer_yoy_log",      "-",            "apreciacion asfixia margenes (la trampa)",
  "IS_TV",   "tot_yoy_log",        "+",            "ToT favorables a productos exportados",
  "IS_TH",   "us_ip_yoy_log",      "+",            "demanda externa alimenta TH",
  "IS_TH",   "tot_yoy_log",        "+",            "ToT favorables",
  "IS_NH",   "r_real",             "-",            "TPM contrae demanda interna",
  "IS_NH",   "cr_col_yoy_log",     "+",            "credito alimenta consumo/inversion",
  "IS_NB",   "r_real",             "-",            "TPM contrae demanda interna",
  "IS_NB",   "itcer_yoy_log",      "+",            "apreciacion abarata insumos importados",
  "taylor",  "inflation_yoy",      "+",            "respuesta a inflacion",
  "taylor",  "output_gap",         "+",            "respuesta procyclica al ciclo",
  "phillips","output_gap",         "+",            "presion de demanda sobre precios",
  "phillips","fx_sell_yoy_log",    "+",            "pass-through cambiario",
  "phillips","wti_yoy_log",        "+",            "pass-through de costos energeticos",
  "fx",      "rate_diff_cr_us",    "-",            "diferencial favorable aprecia colon (signo negativo en fx_yoy)",
  "fx",      "vix_d1",             "+",            "salto de riesgo deprecia colon"
)

# Comparar contra estimados (variant standard)
validation <- tbl_std %>%
  inner_join(expected_signs, by = c("equation","term")) %>%
  mutate(
    actual_sign     = ifelse(estimate > 0, "+", ifelse(estimate < 0, "-", "0")),
    sign_consistent = actual_sign == expected_sign,
    significant_5   = p_value < 0.05
  ) %>%
  select(equation, term, expected_sign, actual_sign, estimate, std_error,
         sign_consistent, significant_5, rationale)

readr::write_csv(validation, "output/tables/islmbp_sign_validation.csv")
log_msg("Validación de signos:")
print(validation)

# ------ 4. TESTS CHOW DE ESTABILIDAD ----------------------------
log_msg("Tests Chow de estabilidad...")

formulas_to_test <- list(
  IS_TV  = y_TV_yoy_log ~ us_ip_yoy_log + itcer_yoy_log + tot_yoy_log + y_TV_yoy_log_lag1,
  IS_TH  = y_TH_yoy_log ~ us_ip_yoy_log + tot_yoy_log + y_TH_yoy_log_lag1,
  IS_NH  = y_NH_yoy_log ~ r_real + cr_col_yoy_log + y_NH_yoy_log_lag1,
  IS_NB  = y_NB_yoy_log ~ r_real + itcer_yoy_log + y_NB_yoy_log_lag1,
  taylor = tpm ~ tpm_lag1 + inflation_yoy + output_gap,
  fx     = fx_sell_yoy_log ~ rate_diff_cr_us + tot_yoy_log + vix_d1
)

chow_results <- list()
for (nm in names(formulas_to_test)) {
  res <- chow_split_test(formulas_to_test[[nm]], panel,
                          break_dates = islmbp_specs$estimation$chow_breaks
                                        %||% c("2018-01-01","2020-03-01"))
  res$equation <- nm
  chow_results[[nm]] <- res
}
chow_df <- bind_rows(chow_results) %>%
  select(equation, break_date, F, p, note)

readr::write_csv(chow_df, "output/tables/islmbp_chow_stability.csv")
log_msg("Tests Chow guardados.")
print(chow_df)

# ------ 5. COMPARACIÓN ENTRE VARIANTES TAYLOR -------------------
compare_taylor <- bind_rows(
  tbl_std %>% filter(equation == "taylor"),
  tbl_ext %>% filter(equation == "taylor")
) %>%
  select(taylor_variant, term, estimate, std_error, p_value, stars, n, r2)

readr::write_csv(compare_taylor, "output/tables/islmbp_taylor_compare.csv")

# ------ 6. GUARDAR OBJETOS DE ESTIMACIÓN ------------------------
saveRDS(eq_standard, "data_intermediate/islmbp/eq_estimates_standard.rds")
saveRDS(eq_extended, "data_intermediate/islmbp/eq_estimates_extended.rds")

log_msg("=== 21 done ===")
