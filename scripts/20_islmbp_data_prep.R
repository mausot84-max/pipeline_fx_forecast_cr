# ============================================================
# 20_islmbp_data_prep.R — Preparación del panel mensual ISLMBP
# de cuatro cuadrantes
# ============================================================
# Lee features_monthly.rds y construye el panel listo para
# estimación del IS-LM-BP de cuatro cuadrantes.
#
# Diferencia clave vs versión previa (dos sectores T/N):
#   - Construye índices por cuadrante (y_TV, y_TH, y_NH, y_NB)
#   - Si hay data del cuadro 144 BCCR (crédito × actividad × moneda)
#     y del cuadro 82 (exportaciones × actividad), construye shares
#     empíricos.  Si no, usa shares a priori del SECTOR_MATRIX.
#   - Produce tres métricas del impuesto silencioso (squeeze,
#     subsidy, amplitude).
#
# Produce:
#   data_intermediate/islmbp/panel_islmbp_monthly.rds
#   data_intermediate/islmbp/quadrant_mapping.csv
#   data_intermediate/islmbp/coverage_report.csv
# ============================================================

log_msg("=== 20: ISLMBP data preparation (4 cuadrantes) ===")

mn_path <- "data_intermediate/features/features_monthly.rds"
if (!file.exists(mn_path))
  stop("[ISLMBP] features_monthly.rds no existe. Correr scripts 01-08 primero.")

mn <- readRDS(mn_path) %>% arrange(date)
log_msg(paste("Panel mensual cargado:", nrow(mn), "obs x", ncol(mn), "vars"))

# ------ 1. CONSTRUCCIÓN DE ÍNDICES POR CUADRANTE ----------------
mn <- build_quadrant_indices(mn, sector_matrix = SECTOR_MATRIX,
                              exclude_mixed = islmbp_specs$sector_aggregation$exclude_mixed_sectors %||% TRUE)

mapping <- attr(mn, "quadrant_mapping")
if (!is.null(mapping)) {
  readr::write_csv(mapping, "data_intermediate/islmbp/quadrant_mapping.csv")
  log_msg("Asignación de cuadrantes:")
  print(mapping)
} else {
  log_msg("Asignación de cuadrantes no disponible (probablemente faltan IMAE sectoriales).", "WARN")
}

# ------ 2. MÉTRICAS DEL IMPUESTO SILENCIOSO ---------------------
mn <- compute_silent_tax_metrics(mn)

# ------ 3. OUTPUT GAP (HP, λ=14400) -----------------------------
if ("imae_tc" %in% names(mn)) {
  log_imae <- log(pmax(mn$imae_tc, 1e-6))
  hp <- hp_filter(log_imae, lambda = islmbp_specs$output_gap$lambda %||% 14400)
  mn$imae_trend <- hp$trend
  mn$output_gap <- hp$cycle
} else {
  log_msg("imae_tc no disponible — output_gap = NA", "WARN")
  mn$output_gap <- NA_real_
}

# Brecha del TCR
if ("itcer" %in% names(mn)) {
  log_q <- log(pmax(mn$itcer, 1e-6))
  hp_q <- hp_filter(log_q, lambda = islmbp_specs$output_gap$lambda %||% 14400)
  mn$itcer_trend <- hp_q$trend
  mn$q_gap       <- hp_q$cycle
} else {
  mn$q_gap <- NA_real_
}

# ------ 4. TASAS REALES Y DIFERENCIALES -------------------------
if (all(c("tpm","inflation_yoy") %in% names(mn))) {
  mn$r_real <- mn$tpm - mn$inflation_yoy
}
if (all(c("tpm","fedfunds") %in% names(mn))) {
  mn$rate_diff_cr_us <- mn$tpm - mn$fedfunds
}

# ------ 5. VARIACIONES LOG INTERANUALES -------------------------
yoy_vars <- intersect(c("itcer","tot","wti","fx_sell","imae_tc","us_ip"),
                       names(mn))
for (v in yoy_vars) {
  log_col <- paste0(v, "_log")
  mn[[log_col]] <- log(pmax(mn[[v]], 1e-6))
  mn <- add_log_yoy_m(mn, log_col, periods = 12)
  old <- paste0(log_col, "_yoy_log")
  new <- paste0(v, "_yoy_log")
  if (old %in% names(mn)) names(mn)[names(mn) == old] <- new
}

# Crédito real en colones deflactado
if (all(c("credit_col","inflation_yoy") %in% names(mn))) {
  ipc_index <- cumprod(1 + (mn$inflation_yoy / 100) / 12)
  mn$credit_col_real <- mn$credit_col / pmax(ipc_index, 1e-6)
  mn$credit_col_real_log <- log(pmax(mn$credit_col_real, 1e-6))
  mn <- add_log_yoy_m(mn, "credit_col_real_log", periods = 12)
  if ("credit_col_real_log_yoy_log" %in% names(mn))
    names(mn)[names(mn) == "credit_col_real_log_yoy_log"] <- "cr_col_yoy_log"
} else {
  mn$cr_col_yoy_log <- NA_real_
}

# ------ 6. DIFERENCIAS Y LAGS ESTRUCTURALES ---------------------
mn <- add_diff_m(mn, "vix", n = 1)
mn <- add_diff_m(mn, "inflation_yoy", n = 1)
mn <- add_lag_m(mn, "inflation_yoy_d1", n = 1)

mn <- add_lag_m(mn, "tpm", n = 1)
for (q in c("TV","TH","NH","NB")) {
  yvar <- paste0("y_", q, "_yoy_log")
  if (yvar %in% names(mn)) mn <- add_lag_m(mn, yvar, n = 1)
}

# ------ 7. MUESTRA DE ESTIMACIÓN --------------------------------
sample_start <- as.Date(islmbp_specs$model$sample_start %||% "2010-01-01")
sample_end   <- as.Date(islmbp_specs$model$sample_end   %||% "2024-12-31")

panel <- mn %>%
  filter(date >= sample_start, date <= sample_end) %>%
  arrange(date)

log_msg(paste("Muestra ISLMBP:", nrow(panel), "obs",
              format(min(panel$date)), "a", format(max(panel$date))))

# ------ 8. REPORTE DE COBERTURA ---------------------------------
needed <- c(
  "y_TV_yoy_log","y_TH_yoy_log","y_NH_yoy_log","y_NB_yoy_log",
  "y_TV_yoy_log_lag1","y_TH_yoy_log_lag1","y_NH_yoy_log_lag1","y_NB_yoy_log_lag1",
  "tax_squeeze_TV","tax_subsidy_NB","silent_tax_amplitude",
  "tpm","tpm_lag1","inflation_yoy","inflation_yoy_d1","inflation_yoy_d1_lag1",
  "output_gap","q_gap","r_real",
  "itcer_yoy_log","fx_sell_yoy_log","tot_yoy_log","wti_yoy_log",
  "rate_diff_cr_us","vix_d1","us_ip_yoy_log","cr_col_yoy_log"
)

coverage <- tibble::tibble(
  variable = needed,
  present  = needed %in% names(panel),
  n_valid  = sapply(needed, function(v) {
    if (v %in% names(panel)) sum(!is.na(panel[[v]])) else 0L
  })
)
readr::write_csv(coverage, "data_intermediate/islmbp/coverage_report.csv")
log_msg("Cobertura por variable:")
print(coverage)

# ------ 9. GUARDAR PANEL ----------------------------------------
saveRDS(panel, "data_intermediate/islmbp/panel_islmbp_monthly.rds")
log_msg("Panel ISLMBP guardado: data_intermediate/islmbp/panel_islmbp_monthly.rds")
log_msg("=== 20 done ===")
