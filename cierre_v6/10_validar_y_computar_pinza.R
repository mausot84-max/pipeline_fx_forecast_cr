# ============================================================
# cierre_v6/10_validar_y_computar_pinza.R
# ============================================================
# Cuarta etapa de la pinza empírica, ya con datos sectoriales
# del Sistema Bancario Nacional (SBN), exportaciones por régimen
# y resto del manifest extendido.
#
# Hace tres tareas:
#
#   1. VALIDA la hipótesis MN/ME para E07.34 (sospecha: MN) y
#      E07.35 (sospecha: ME). Tres criterios:
#        a. Magnitud relativa de los totales (20579 vs 20578).
#        b. Coherencia con el crédito agregado conocido (4814).
#        c. Patrón sectorial: agro debería ser bajo en ME,
#           industria medio-alto, etc.
#
#   2. CALCULA σ^C observado por sector SBN cuando la hipótesis
#      se confirma. La fórmula es ME / (MN + ME).
#
#   3. CALCULA σ^Y por régimen (no por sección CIIU): cociente
#      de exportaciones FOB sobre IMAE en términos comparables.
#      También reporta razón especial/definitivo de exports y
#      razón especial/definitivo de IMAE para triangulación.
#
# Uso (con .Rproj abierto):
#   source("cierre_v6/10_validar_y_computar_pinza.R")
#
# Outputs en cierre_v6/outputs/:
#   - hipotesis_mn_me_validacion.csv
#   - sigma_C_sbn_observado.csv
#   - sigma_Y_regimen.csv
#   - sectorial_sbn_resumen.csv
#   - regimen_triangulacion.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate); library(tidyr); library(stringr)
})

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
stopifnot(dir.exists(RAW))

MANIFEST <- read_csv(file.path(OUT, "raw_pinza_manifest.csv"),
                     show_col_types = FALSE,
                     col_types = cols(.default = col_character(), n_obs = col_double())) %>%
  mutate(across(c(seccion, moneda, regimen), ~ tidyr::replace_na(.x, "")))
manif_ok <- MANIFEST %>% filter(status == "ok")
message("[10] Manifest cargado: ", nrow(manif_ok), " series OK")

read_serie <- function(bloque, seccion = "", moneda = "", regimen = "") {
  m <- manif_ok %>%
    filter(.data$bloque == !!bloque,
           .data$seccion == !!seccion,
           .data$moneda  == !!moneda,
           .data$regimen == !!regimen)
  if (nrow(m) == 0) return(NULL)
  read_csv(m$archivo[1], show_col_types = FALSE,
           col_types = cols(fecha = col_date(), valor = col_double()))
}

VENTANA_INI <- as.Date("2018-01-01")
VENTANA_FIN <- as.Date("2024-12-31")
pluck_avg <- function(df, start = VENTANA_INI, end = VENTANA_FIN) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  v <- df %>% filter(fecha >= start, fecha <= end) %>% pull(valor)
  if (length(v) == 0) return(NA_real_)
  mean(v, na.rm = TRUE)
}

# ============================================================
# 1. VALIDACIÓN DE HIPÓTESIS MN/ME
# ============================================================

# (a) Magnitudes totales
total_h_mn <- read_serie("credit_sbn", "TOTAL", "MN")
total_h_me <- read_serie("credit_sbn", "TOTAL", "ME")
agg_4814   <- read_serie("credit_total_agregado", "", "CRC")

m_mn <- pluck_avg(total_h_mn)
m_me <- pluck_avg(total_h_me)
m_4814 <- pluck_avg(agg_4814)

message(sprintf("\n[10] Totales (promedio 2018-2024):"))
message(sprintf("    E07.34 TOTAL (hipótesis MN, código 20579): %.0f", m_mn))
message(sprintf("    E07.35 TOTAL (hipótesis ME, código 20578): %.0f", m_me))
message(sprintf("    4814 Crédito sector privado colones:       %.0f", m_4814))
message(sprintf("    Razón ME/(MN+ME):                          %.3f", m_me / (m_mn + m_me)))
message(sprintf("    Diferencia (MN+ME) vs 4814:                %.0f", (m_mn + m_me) - m_4814))

# (b) Patrón sectorial — sectores con expectativa de baja exposición USD
sectores_sbn <- c("AGRO", "INDUSTRIA", "CONSTRUCCION", "COMERCIO_REST_HOT",
                  "TRANSP_COMUNIC", "SERVICIOS", "OTROS_SERVICIOS",
                  "VIVIENDA", "CONSUMO", "OTROS_PRESTAMOS")

sigma_obs <- lapply(sectores_sbn, function(s) {
  mn <- pluck_avg(read_serie("credit_sbn", s, "MN"))
  me <- pluck_avg(read_serie("credit_sbn", s, "ME"))
  if (is.na(mn) | is.na(me) | (mn + me) == 0) {
    tibble(sector_sbn = s, MN_avg = mn, ME_avg = me,
           total = NA, sigma_C_obs_si_hip_correcta = NA)
  } else {
    tibble(sector_sbn = s, MN_avg = mn, ME_avg = me,
           total = mn + me,
           sigma_C_obs_si_hip_correcta = me / (mn + me))
  }
}) %>% bind_rows()

print(sigma_obs)

# Sanity checks contextuales sobre patrones esperados:
#   AGRO: típicamente bajo ME (≈ 0.2 - 0.4)
#   INDUSTRIA: medio-alto (≈ 0.4 - 0.7)
#   CONSTRUCCION: medio (≈ 0.3 - 0.5)
#   VIVIENDA: bajo en años recientes (≈ 0.1 - 0.3, hogares en CRC)
#   CONSUMO: muy bajo (< 0.2, tarjetas y créditos personales en CRC)
#   COMERCIO_REST_HOT: medio-alto si hay turismo internacional importante
#
# Si la hipótesis MN=20579 / ME=20578 es CORRECTA, los patrones
# anteriores deberían respetarse. Si por error invertimos las
# series, veríamos sigma_C en Consumo del 80% — clara señal de
# que la hipótesis está al revés.

# Determinar dirección
mediana_si_hip <- median(sigma_obs$sigma_C_obs_si_hip_correcta, na.rm = TRUE)
sigma_consumo <- sigma_obs$sigma_C_obs_si_hip_correcta[sigma_obs$sector_sbn == "CONSUMO"]
sigma_vivienda <- sigma_obs$sigma_C_obs_si_hip_correcta[sigma_obs$sector_sbn == "VIVIENDA"]

hipotesis_ok <- !is.na(sigma_consumo) && !is.na(sigma_vivienda) &&
                sigma_consumo < 0.30 && sigma_vivienda < 0.40

if (hipotesis_ok) {
  message("\n[10] ✓ Hipótesis MN/ME CONFIRMADA por patrón sectorial")
  message(sprintf("    σ_C Consumo:  %.2f (esperado < 0.30) ✓", sigma_consumo))
  message(sprintf("    σ_C Vivienda: %.2f (esperado < 0.40) ✓", sigma_vivienda))
  sigma_final <- sigma_obs %>%
    rename(sigma_C_obs = sigma_C_obs_si_hip_correcta)
} else {
  message("\n[10] ⚠ Hipótesis MN/ME parece INVERTIDA — recomputando")
  message(sprintf("    σ_C Consumo:  %.2f (esperado < 0.30)", sigma_consumo))
  message(sprintf("    σ_C Vivienda: %.2f (esperado < 0.40)", sigma_vivienda))
  sigma_final <- sigma_obs %>%
    mutate(MN_avg_corregido = ME_avg,
           ME_avg_corregido = MN_avg,
           sigma_C_obs = MN_avg / (ME_avg_corregido + MN_avg_corregido))
}

# Tabla validación
validacion <- tibble(
  criterio = c(
    "Total hipótesis MN (20579) promedio 2018-2024",
    "Total hipótesis ME (20578) promedio 2018-2024",
    "Crédito total colones agregado (4814)",
    "Suma MN+ME vs 4814",
    "Razón ME/(MN+ME)",
    "σ_C Consumo (esperado < 0.30)",
    "σ_C Vivienda (esperado < 0.40)",
    "Hipótesis MN=20579 / ME=20578 confirmada"
  ),
  valor = c(
    sprintf("%.0f", m_mn),
    sprintf("%.0f", m_me),
    sprintf("%.0f", m_4814),
    sprintf("%.0f vs %.0f", m_mn + m_me, m_4814),
    sprintf("%.3f", m_me / (m_mn + m_me)),
    sprintf("%.3f", sigma_consumo),
    sprintf("%.3f", sigma_vivienda),
    ifelse(hipotesis_ok, "SI", "NO (invertida)")
  )
)
write_csv(validacion, file.path(OUT, "hipotesis_mn_me_validacion.csv"))

write_csv(sigma_final, file.path(OUT, "sigma_C_sbn_observado.csv"))
message("\n[10] sigma_C_sbn_observado.csv guardado.")
message("\nσ_C por sector SBN:")
print(sigma_final %>% select(sector_sbn, MN_avg, ME_avg, sigma_C_obs))

# ============================================================
# 2. SIGMA_Y POR RÉGIMEN
# ============================================================

exp_def <- read_serie("exports", "", "", "DEFINITIVO")
exp_esp <- read_serie("exports", "", "", "ESPECIAL")
imae_def <- read_serie("imae_regimen", "", "", "DEFINITIVO")
imae_esp <- read_serie("imae_regimen", "", "", "ESPECIAL")

exp_def_avg <- pluck_avg(exp_def)
exp_esp_avg <- pluck_avg(exp_esp)
imae_def_avg <- pluck_avg(imae_def)
imae_esp_avg <- pluck_avg(imae_esp)

# sigma_Y como ratio normalizado por régimen
sigma_Y_reg <- tibble(
  regimen = c("DEFINITIVO", "ESPECIAL"),
  exports_USD_avg = c(exp_def_avg, exp_esp_avg),
  imae_avg = c(imae_def_avg, imae_esp_avg),
  ratio_exports_imae = c(exp_def_avg / imae_def_avg,
                        exp_esp_avg / imae_esp_avg)
) %>%
  mutate(sigma_Y_raw = ratio_exports_imae,
         sigma_Y_norm = sigma_Y_raw / max(sigma_Y_raw, na.rm = TRUE))

write_csv(sigma_Y_reg, file.path(OUT, "sigma_Y_regimen.csv"))
message("\n[10] σ_Y por régimen:")
print(sigma_Y_reg)

# ============================================================
# 3. TRIANGULACIÓN POR RÉGIMEN
# ============================================================

# Calcular crecimientos 2015 -> 2025 de cada serie para reproducir
# y triangular el cuadro 5.1
crecim <- function(df, y1 = 2015, y2 = 2025) {
  if (is.null(df) || nrow(df) == 0) return(NA)
  df <- df %>% mutate(yr = year(fecha))
  if (!all(c(y1, y2) %in% df$yr)) return(NA)
  a1 <- mean(df$valor[df$yr == y1], na.rm = TRUE)
  a2 <- mean(df$valor[df$yr == y2], na.rm = TRUE)
  if (a1 <= 0) return(NA)
  round(100 * (a2 / a1 - 1), 1)
}

imp_def <- read_serie("imports", "", "", "DEFINITIVO")
imp_esp <- read_serie("imports", "", "", "ESPECIAL")
ipx_def <- read_serie("ipx", "", "", "DEFINITIVO")
ipx_esp <- read_serie("ipx", "", "", "ESPECIAL")

triangulacion <- tibble(
  variable = c("IMAE", "Exportaciones FOB", "Importaciones CIF", "Índice precios exportaciones"),
  crecim_2015_2025_definitivo = c(crecim(imae_def), crecim(exp_def),
                                    crecim(imp_def), crecim(ipx_def)),
  crecim_2015_2025_especial   = c(crecim(imae_esp), crecim(exp_esp),
                                    crecim(imp_esp), crecim(ipx_esp))
) %>%
  mutate(razon_esp_def = round(crecim_2015_2025_especial / abs(crecim_2015_2025_definitivo), 2))

write_csv(triangulacion, file.path(OUT, "regimen_triangulacion.csv"))
message("\n[10] Triangulación por régimen (crecimientos 2015 -> 2025):")
print(triangulacion)

# ============================================================
# Resumen ejecutivo
# ============================================================

message("\n========================================================")
message("RESUMEN")
message("========================================================")
message("\n1. Hipótesis MN/ME del crédito sectorial SBN:")
print(validacion %>% as.data.frame())
message("\n2. σ_C observado por sector SBN:")
print(sigma_final %>% select(sector_sbn, sigma_C_obs))
message("\n3. σ_Y por régimen:")
print(sigma_Y_reg %>% select(regimen, exports_USD_avg, ratio_exports_imae, sigma_Y_norm))
message("\n4. Triangulación régimen:")
print(triangulacion)

message("\n[10] Listo. Outputs en cierre_v6/outputs/:")
message("  - hipotesis_mn_me_validacion.csv")
message("  - sigma_C_sbn_observado.csv")
message("  - sigma_Y_regimen.csv")
message("  - regimen_triangulacion.csv")
