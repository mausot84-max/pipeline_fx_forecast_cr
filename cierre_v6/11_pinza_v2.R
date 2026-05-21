# ============================================================
# cierre_v6/11_pinza_v2.R
# ============================================================
# Quinta etapa de la pinza empírica — versión revisada tras
# descubrir que el desglose sector × moneda del crédito SBN
# se publica recién desde enero de 2024.
#
# Tres bloques:
#   (A) Foto contemporánea: σ_C por sector × moneda
#       en la ventana 2024-01 a la última observación.
#       Reconstruye Industria, Servicios y Consumo como suma
#       de sus leaf (los códigos agregadores 20289/20081/20621
#       no son series propias).
#
#   (B) Serie larga selectiva: σ_C 2010-2025 sólo para los
#       dos sectores con cobertura histórica (TOTAL agregado
#       y Tarjetas de crédito).
#
#   (C) Triangulación: comparar σ_C TOTAL agregado vs el
#       σ_C reconstruido como promedio ponderado de sectores
#       en la ventana 2024 — sirve como sanity check.
#
# Outputs:
#   sigma_C_v2_corto.csv   — σ_C 2024+ por sector (foto)
#   sigma_C_v2_largo.csv   — σ_C 2010-2025 (Total y Tarjetas)
#   sigma_C_v2_triangul.csv — sanity check Total vs reconstruido
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(tidyr)
  library(lubridate); library(purrr)
})

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
MANIFEST_FILE <- file.path(OUT, "raw_pinza_manifest.csv")

manifest <- read_csv(MANIFEST_FILE, show_col_types = FALSE,
                     col_types = cols(.default = col_character(),
                                      n_obs = col_integer()))

# Helper: cargar serie por (bloque, seccion, moneda) ----------
cargar_serie <- function(bloque, seccion, moneda) {
  fila <- manifest %>%
    filter(bloque == !!bloque,
           seccion == !!seccion,
           moneda == !!moneda,
           status == "ok")
  if (nrow(fila) == 0) return(NULL)
  df <- read_csv(fila$archivo[1], show_col_types = FALSE,
                 col_types = cols(fecha = col_date(),
                                  valor = col_double()))
  df %>% mutate(sector = seccion, moneda = !!moneda)
}

# Sectores leaf disponibles del crédito SBN
sectores_leaf <- manifest %>%
  filter(bloque == "credit_sbn", status == "ok",
         moneda %in% c("MN", "ME")) %>%
  pull(seccion) %>% unique()

message("[11] Sectores leaf disponibles: ",
        paste(sectores_leaf, collapse = ", "))

# Cargar todos los leaf
datos_leaf <- map_dfr(sectores_leaf, function(s) {
  bind_rows(cargar_serie("credit_sbn", s, "MN"),
            cargar_serie("credit_sbn", s, "ME"))
})

if (nrow(datos_leaf) == 0) stop("[11] No se pudo cargar ningún leaf")

# ===================================================================
# (A) σ_C foto contemporánea (ventana donde TODOS los leaf existen)
# ===================================================================
# Determinamos la ventana común de los leaf de cobertura corta
fecha_min_corta <- datos_leaf %>%
  filter(!sector %in% c("TOTAL", "CONSUMO_TARJETAS")) %>%
  group_by(sector, moneda) %>% summarise(min_f = min(fecha), .groups="drop") %>%
  pull(min_f) %>% max()

fecha_max_corta <- datos_leaf %>%
  group_by(sector, moneda) %>% summarise(max_f = max(fecha), .groups="drop") %>%
  pull(max_f) %>% min()

message(sprintf("[11] Ventana corta (todos los leaf): %s a %s",
                fecha_min_corta, fecha_max_corta))

datos_corto <- datos_leaf %>%
  filter(fecha >= fecha_min_corta & fecha <= fecha_max_corta)

sigma_C_corto_indiv <- datos_corto %>%
  group_by(sector, moneda) %>%
  summarise(media = mean(valor, na.rm = TRUE),
            n_obs = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = moneda, values_from = c(media, n_obs)) %>%
  mutate(sigma_C = media_ME / (media_MN + media_ME),
         ventana = paste0(fecha_min_corta, " a ", fecha_max_corta))

# Reconstruir Industria, Servicios, Consumo como suma de leaf
# Mapeo: agregador BCCR → componentes leaf
mapping <- list(
  INDUSTRIA_AGG = c("CONSTRUCCION", "INDUSTRIA_OTRAS"),
  SERVICIOS_AGG = c("OTROS_SERVICIOS", "TRANSP_COMUNIC"),
  CONSUMO_AGG   = c("CONSUMO_OTROS",  "CONSUMO_TARJETAS")
)

agregados <- map_dfr(names(mapping), function(nombre_agg) {
  comp <- mapping[[nombre_agg]]
  d <- datos_corto %>% filter(sector %in% comp)
  if (nrow(d) == 0) return(NULL)
  d %>%
    group_by(fecha, moneda) %>%
    summarise(valor = sum(valor, na.rm = TRUE),
              n_componentes = n(),
              .groups = "drop") %>%
    mutate(sector = nombre_agg) %>%
    group_by(sector, moneda) %>%
    summarise(media = mean(valor, na.rm = TRUE),
              n_obs = n(),
              .groups = "drop")
}) %>%
  pivot_wider(names_from = moneda, values_from = c(media, n_obs)) %>%
  mutate(sigma_C = media_ME / (media_MN + media_ME),
         ventana = paste0(fecha_min_corta, " a ", fecha_max_corta))

sigma_C_corto_final <- bind_rows(sigma_C_corto_indiv, agregados) %>%
  select(sector, media_MN, media_ME, n_obs_MN, n_obs_ME, sigma_C, ventana) %>%
  arrange(desc(sigma_C))

# ===================================================================
# (B) σ_C serie larga (solo Tarjetas y TOTAL)
# ===================================================================
series_largas <- c("TOTAL", "CONSUMO_TARJETAS")

sigma_C_largo <- map_dfr(series_largas, function(s) {
  d <- bind_rows(cargar_serie("credit_sbn", s, "MN"),
                 cargar_serie("credit_sbn", s, "ME"))
  if (nrow(d) == 0) return(NULL)

  # Anual: promedio por año
  anual <- d %>%
    mutate(anio = year(fecha)) %>%
    group_by(anio, moneda) %>%
    summarise(media = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = moneda, values_from = media) %>%
    mutate(sigma_C = ME / (MN + ME),
           sector = s)
  anual
}) %>% select(sector, anio, MN, ME, sigma_C) %>% arrange(sector, anio)

# Resumen por sector con sigma_C promedio histórico
sigma_C_largo_resumen <- sigma_C_largo %>%
  group_by(sector) %>%
  summarise(anio_inicio = min(anio),
            anio_fin = max(anio),
            sigma_C_2010 = sigma_C[anio == min(anio)],
            sigma_C_2015 = sigma_C[anio == 2015][1],
            sigma_C_2020 = sigma_C[anio == 2020][1],
            sigma_C_2024 = sigma_C[anio == 2024][1],
            sigma_C_max = max(sigma_C, na.rm = TRUE),
            sigma_C_min = min(sigma_C, na.rm = TRUE),
            .groups = "drop")

# ===================================================================
# (C) Triangulación: σ_C TOTAL vs reconstrucción agregada (foto 2024)
# ===================================================================
# σ_C agregado de TOTAL en la ventana corta
sigma_total_corto <- datos_leaf %>%
  filter(sector == "TOTAL",
         fecha >= fecha_min_corta & fecha <= fecha_max_corta) %>%
  group_by(moneda) %>% summarise(media = mean(valor), .groups = "drop") %>%
  pivot_wider(names_from = moneda, values_from = media) %>%
  mutate(sigma_C = ME / (MN + ME),
         metodo = "TOTAL directo (20578/20579)")

# σ_C como suma de TODOS los sectores (excepto TOTAL)
sigma_recon <- datos_corto %>%
  filter(sector != "TOTAL") %>%
  group_by(fecha, moneda) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  group_by(moneda) %>%
  summarise(media = mean(valor), .groups = "drop") %>%
  pivot_wider(names_from = moneda, values_from = media) %>%
  mutate(sigma_C = ME / (MN + ME),
         metodo = "Suma reconstruida de sectores")

triangulacion <- bind_rows(sigma_total_corto, sigma_recon) %>%
  select(metodo, MN, ME, sigma_C) %>%
  mutate(ventana = paste0(fecha_min_corta, " a ", fecha_max_corta))

# ===================================================================
# Escribir outputs
# ===================================================================
write_csv(sigma_C_corto_final,   file.path(OUT, "sigma_C_v2_corto.csv"))
write_csv(sigma_C_largo,         file.path(OUT, "sigma_C_v2_largo.csv"))
write_csv(sigma_C_largo_resumen, file.path(OUT, "sigma_C_v2_largo_resumen.csv"))
write_csv(triangulacion,         file.path(OUT, "sigma_C_v2_triangul.csv"))

# ===================================================================
# Reporte en consola
# ===================================================================
cat("\n===========================================================\n")
cat("[11] σ_C v2 — Foto contemporánea (ventana corta)\n")
cat("===========================================================\n")
print(sigma_C_corto_final, n = Inf)

cat("\n===========================================================\n")
cat("[11] σ_C v2 — Triangulación (sanity check)\n")
cat("===========================================================\n")
print(triangulacion)

cat("\n===========================================================\n")
cat("[11] σ_C v2 — Serie larga (2010-2025) resumen\n")
cat("===========================================================\n")
print(sigma_C_largo_resumen)

cat("\n===========================================================\n")
cat("[11] σ_C v2 — Tarjetas de crédito: anual completo\n")
cat("===========================================================\n")
print(sigma_C_largo %>% filter(sector == "CONSUMO_TARJETAS"), n = Inf)

cat("\n===========================================================\n")
cat("[11] σ_C v2 — TOTAL: anual completo\n")
cat("===========================================================\n")
print(sigma_C_largo %>% filter(sector == "TOTAL"), n = Inf)

cat("\n[11] Outputs escritos en cierre_v6/outputs/:\n")
cat("  - sigma_C_v2_corto.csv\n")
cat("  - sigma_C_v2_largo.csv\n")
cat("  - sigma_C_v2_largo_resumen.csv\n")
cat("  - sigma_C_v2_triangul.csv\n")
