# ============================================================
# cierre_v6/03_compute_pinza_empirica.R
# ============================================================
# Tercera etapa de la pinza empírica. Lee las series descargadas
# por el script 02 desde cierre_v6/outputs/raw/, y produce:
#
#   sigma_Y_observado.csv       proporción de ingresos en USD por sector
#   sigma_C_observado.csv       proporción de costos dolarizados por sector
#   cuadrantes_observados.csv   reasignación con datos vs priors
#   tabla_sectorial_v6.csv      crecimiento sectorial 2015-2025
#   indices_bloques_v6.csv      bloque externamente integrado vs doméstico
#   bloque_growth_v6.csv        crecimiento agregado por bloque
#   regimen_descomposicion.csv  definitivo vs especial (si hay datos)
#
# Se ejecuta desde el R project pipeline_fx_forecast_cr:
#   source("cierre_v6/03_compute_pinza_empirica.R")
#
# Convenciones de los proxies:
#   sigma_Y_s = mean(exports_s) / mean(IMAE_s) en 2018-2023,
#               normalizado al máximo del conjunto.
#               Proxy de proporción de ingresos en USD.
#               Si exports_s no está disponible para la sección,
#               sigma_Y_s queda NA y se usa el prior.
#   sigma_C_s = mean(credit_USD_s) / mean(credit_USD_s + credit_CRC_s)
#               en 2018-2023. Proxy parcial de exposición a
#               costos en dólares: cubre el componente financiero,
#               no planilla ni proveedores.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate); library(tidyr); library(stringr)
})

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
stopifnot(dir.exists(RAW))

MANIFEST <- read_csv(
  file.path(OUT, "raw_pinza_manifest.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character(), n_obs = col_double())
) %>%
  mutate(across(c(seccion, moneda, regimen), ~ tidyr::replace_na(.x, "")))
manif_ok <- MANIFEST %>% filter(status == "ok")
message("[03] Manifest cargado: ", nrow(manif_ok), " series OK")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

read_serie <- function(bloque, seccion = "", moneda = "", regimen = "") {
  m <- manif_ok %>%
    filter(bloque == !!bloque,
           .data$seccion == !!seccion,
           .data$moneda  == !!moneda,
           .data$regimen == !!regimen)
  if (nrow(m) == 0) return(NULL)
  read_csv(m$archivo[1], show_col_types = FALSE,
           col_types = cols(fecha = col_date(), valor = col_double()))
}

SIGMA_START <- as.Date("2018-01-01")
SIGMA_END   <- as.Date("2023-12-31")
pluck_avg <- function(df, start = SIGMA_START, end = SIGMA_END) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  v <- df %>% filter(fecha >= start, fecha <= end) %>% pull(valor)
  if (length(v) == 0) return(NA_real_)
  mean(v, na.rm = TRUE)
}

SECCIONES <- c("A","C","F","G","H","I","J","K","L","MN")

# IMAE general y por sección
imae_general <- read_serie("imae_ciiu", seccion = "TOTAL")
if (is.null(imae_general)) warning("[03] IMAE general no encontrado en el manifest.")

imae_seccion <- lapply(SECCIONES, function(s) {
  df <- read_serie("imae_ciiu", seccion = s)
  tibble(seccion = s, imae_avg_2018_2023 = pluck_avg(df))
}) %>% bind_rows()

# Exports por sección
exports_seccion <- lapply(SECCIONES, function(s) {
  df <- read_serie("exports", seccion = s)
  manif <- manif_ok %>% filter(bloque == "exports", seccion == s) %>% slice(1)
  tibble(seccion = s,
         exports_avg_2018_2023 = pluck_avg(df),
         exports_etiqueta = manif$etiqueta %||% NA_character_)
}) %>% bind_rows()

# Sigma_Y
sigma_Y <- imae_seccion %>%
  left_join(exports_seccion, by = "seccion") %>%
  mutate(
    sigma_Y_raw = exports_avg_2018_2023 / pmax(imae_avg_2018_2023, 1e-6),
    sigma_Y_raw = ifelse(is.na(sigma_Y_raw), NA, sigma_Y_raw),
    sigma_Y_norm = ifelse(all(is.na(sigma_Y_raw)), NA,
                          sigma_Y_raw / max(sigma_Y_raw, na.rm = TRUE))
  )
write_csv(sigma_Y, file.path(OUT, "sigma_Y_observado.csv"))
message("[03] sigma_Y_observado.csv guardado.")

# Sigma_C
credit_seccion <- lapply(SECCIONES, function(s) {
  df_usd <- read_serie("credit", seccion = s, moneda = "USD")
  df_crc <- read_serie("credit", seccion = s, moneda = "CRC")
  usd <- pluck_avg(df_usd); crc <- pluck_avg(df_crc)
  tibble(seccion = s,
         credit_USD_avg = usd,
         credit_CRC_avg = crc,
         credit_total_avg = ifelse(is.na(usd) & is.na(crc), NA,
                                    sum(c(usd, crc), na.rm = TRUE)),
         sigma_C = ifelse(is.na(usd) | is.na(crc) | (usd + crc) == 0, NA,
                          usd / (usd + crc)))
}) %>% bind_rows()
write_csv(credit_seccion, file.path(OUT, "sigma_C_observado.csv"))
message("[03] sigma_C_observado.csv guardado.")

# Cuadrantes (prior vs observado)
priors <- tribble(
  ~seccion, ~ciiu, ~sigma_Y_prior, ~sigma_C_prior,
  "A",  "A",  0.85, 0.30,
  "C",  "C",  0.50, 0.50,
  "F",  "F",  0.55, 0.25,
  "G",  "G",  0.10, 0.60,
  "H",  "H",  0.30, 0.40,
  "I",  "I",  0.75, 0.25,
  "J",  "J",  0.85, 0.70,
  "K",  "K",  0.50, 0.50,
  "L",  "L",  0.40, 0.40,
  "MN", "MN", 0.80, 0.65
)

assign_quad <- function(sy, sc, thr = 0.5) {
  if (any(is.na(c(sy, sc)))) return(NA_character_)
  if (sy >  thr && sc <= thr) return("TV")
  if (sy >  thr && sc >  thr) return("TH")
  if (sy <= thr && sc <= thr) return("NH")
  if (sy <= thr && sc >  thr) return("NB")
  NA_character_
}

quad_table <- priors %>%
  left_join(sigma_Y      %>% select(seccion, sigma_Y_obs = sigma_Y_norm), by = "seccion") %>%
  left_join(credit_seccion %>% select(seccion, sigma_C_obs = sigma_C),     by = "seccion") %>%
  rowwise() %>%
  mutate(cuadrante_prior = assign_quad(sigma_Y_prior, sigma_C_prior),
         cuadrante_obs   = assign_quad(sigma_Y_obs, sigma_C_obs),
         cambio = case_when(
           is.na(cuadrante_obs)                ~ "INDETERMINADO",
           cuadrante_prior == cuadrante_obs    ~ "CONFIRMA",
           TRUE                                ~ "CAMBIA"
         )) %>% ungroup()
write_csv(quad_table, file.path(OUT, "cuadrantes_observados.csv"))
message("[03] cuadrantes_observados.csv guardado.")

# Crecimiento sectorial 2015-2025
crecim <- function(df, y1 = 2015, y2 = 2025) {
  if (is.null(df) || nrow(df) == 0) return(c(NA, NA))
  df <- df %>% mutate(yr = year(fecha))
  if (!all(c(y1, y2) %in% df$yr)) return(c(NA, NA))
  a1 <- mean(df$valor[df$yr == y1], na.rm = TRUE)
  a2 <- mean(df$valor[df$yr == y2], na.rm = TRUE)
  d1 <- df$valor[df$fecha == as.Date(sprintf("%d-12-31", y1))]
  d2 <- df$valor[df$fecha == as.Date(sprintf("%d-12-31", y2))]
  g_avg <- if (a1 > 0) 100 * (a2 / a1 - 1) else NA
  g_dec <- if (length(d1) && length(d2) && d1 > 0) 100 * (d2 / d1 - 1) else NA
  c(round(g_avg, 1), round(g_dec, 1))
}

labels <- c(A="Agricultura, silvicultura y pesca",
            C="Industria manufacturera",
            F="Construccion",
            G="Comercio",
            H="Transporte y almacenamiento",
            I="Alojamiento y servicios de comida",
            J="Informacion y comunicaciones",
            K="Actividades financieras y de seguros",
            L="Actividades inmobiliarias",
            MN="Profesional, cient, tecn, admin")

tabla_sec <- bind_rows(
  tibble(seccion = "TOTAL", sector_label = "IMAE general (base 2017)",
         growth_avg = crecim(imae_general)[1], growth_dec = crecim(imae_general)[2]),
  lapply(SECCIONES, function(s) {
    g <- crecim(read_serie("imae_ciiu", seccion = s))
    tibble(seccion = s, sector_label = labels[[s]], growth_avg = g[1], growth_dec = g[2])
  }) %>% bind_rows()
)
write_csv(tabla_sec, file.path(OUT, "tabla_sectorial_v6.csv"))
message("[03] tabla_sectorial_v6.csv guardado.")

# Bloques data-driven
bloques <- quad_table %>%
  mutate(sigma_Y_efectivo = coalesce(sigma_Y_obs, sigma_Y_prior),
         bloque = ifelse(sigma_Y_efectivo >= 0.5, "EXTERNO", "DOMESTICO"))
write_csv(bloques %>% select(seccion, ciiu, sigma_Y_efectivo, bloque),
          file.path(OUT, "indices_bloques_v6.csv"))
message("[03] indices_bloques_v6.csv guardado.")

bloque_growth <- bloques %>%
  left_join(tabla_sec %>% select(seccion, growth_avg), by = "seccion") %>%
  group_by(bloque) %>%
  summarise(n_secciones = n(),
            secciones = paste(seccion, collapse = ","),
            growth_promedio_simple = round(mean(growth_avg, na.rm = TRUE), 1),
            .groups = "drop")
write_csv(bloque_growth, file.path(OUT, "bloque_growth_v6.csv"))
message("[03] bloque_growth_v6.csv guardado.")

# Régimen
reg_def <- read_serie("imae_regimen", regimen = "DEFINITIVO")
reg_esp <- read_serie("imae_regimen", regimen = "ESPECIAL")
reg_table <- tibble(
  regimen = c("DEFINITIVO", "ESPECIAL"),
  disponible = c(!is.null(reg_def), !is.null(reg_esp)),
  growth_avg_2015_2025 = c(crecim(reg_def)[1], crecim(reg_esp)[1]),
  growth_dec_2015_2025 = c(crecim(reg_def)[2], crecim(reg_esp)[2])
)
write_csv(reg_table, file.path(OUT, "regimen_descomposicion.csv"))
message("[03] regimen_descomposicion.csv guardado.")

message("\n========================================================")
message("PINZA EMPÍRICA — RESULTADOS")
message("========================================================")
message("\nsigma_Y:")
print(sigma_Y %>% select(seccion, sigma_Y_raw, sigma_Y_norm), n = Inf)
message("\nsigma_C:")
print(credit_seccion %>% select(seccion, sigma_C, credit_USD_avg, credit_CRC_avg), n = Inf)
message("\nCuadrantes:")
print(quad_table %>% select(seccion, sigma_Y_prior, sigma_Y_obs, sigma_C_prior, sigma_C_obs,
                              cuadrante_prior, cuadrante_obs, cambio), n = Inf)
message("\nCrecimiento 2015-2025:")
print(tabla_sec, n = Inf)
message("\nBloques:")
print(bloque_growth, n = Inf)
message("\nRégimen:")
print(reg_table, n = Inf)

message("\n[03] Mandame los siguientes archivos para escribir la §5:")
for (f in c("sigma_Y_observado.csv","sigma_C_observado.csv","cuadrantes_observados.csv",
            "tabla_sectorial_v6.csv","indices_bloques_v6.csv","bloque_growth_v6.csv",
            "regimen_descomposicion.csv","raw_pinza_manifest.csv")) {
  message("  cierre_v6/outputs/", f)
}
