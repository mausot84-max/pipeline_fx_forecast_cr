# =========================================================================
# 28_credit_sectoral_144.R
# -------------------------------------------------------------------------
# Prueba 5 (Registro 2): crédito al sector privado por actividad CIIU rev 4
# y por moneda (BCCR cuadro 144).
# Identifica el patrón cross-sectional de desaceleración 2025 vs 2024.
# -------------------------------------------------------------------------
# Outputs:
#   data_intermediate/credit_sectoral_144.csv         — panel sectorial
#   output/credit_sectoral_growth_rates.csv           — tasas por sector
#   output/credit_sectoral_deceleration_ranking.csv   — ranking ordenado
#   output/fig_credit_sectoral_deceleration.png       — gráfico de barras
# =========================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(stringr)
})

source("scripts/utils_bccr.R")

# -------------------------------------------------------------------------
# 1. Definición de series del cuadro 144 (sectorial + moneda)
# -------------------------------------------------------------------------
# El cuadro 144 publica saldos mensuales por actividad CIIU rev 4 y moneda
# desde enero 2024 (cobertura corta pero suficiente para identificar el
# patrón cross-sectional de desaceleración 2025 vs 2024).
#
# Los códigos exactos del SDDE se descubren con discovery_credit.csv. Aquí
# se mantiene la asociación nombre→código de la versión actual del repo.
#
# Si nuevos códigos aparecen en el SDDE, agregarlos al template
# config/series_bccr_template.csv y este script los recoge automáticamente.

cuadro_144 <- read_csv("config/series_bccr_template.csv", show_col_types = FALSE) %>%
  filter(family == "islmbp_credit") %>%
  mutate(
    ciiu = str_extract(series_code, "(?<=144_)[A-Z]+(?=_)"),
    moneda = str_extract(series_code, "(?<=_)[A-Z]{3}$")
  ) %>%
  select(var_name, series_code, ciiu, moneda)

ciiu_label <- tribble(
  ~ciiu, ~sector_label,
  "A",   "Agropecuario",
  "F",   "Construcción",
  "G",   "Comercio",
  "H",   "Transporte y almacenamiento",
  "I",   "Alojamiento y servicios de comida",
  "J",   "Información y comunicaciones",
  "MN",  "Profesional, científico y técnico"
)

# -------------------------------------------------------------------------
# 2. Descarga cuadro 144
# -------------------------------------------------------------------------
message("[28] Descargando cuadro 144 (crédito sectorial por moneda)…")

creds <- get_bccr_credentials()

raw_list <- list()
for (i in seq_len(nrow(cuadro_144))) {
  s <- cuadro_144[i,]
  message(sprintf("  - %s (%s)", s$var_name, s$series_code))
  res <- tryCatch(
    download_bccr_series(s$series_code,
                         start_date = "2023/01/01",
                         end_date   = format(Sys.Date(), "%Y/%m/%d"),
                         credentials = creds, verbose = FALSE, save_raw = TRUE),
    error = function(e) { message("    error: ", e$message); NULL }
  )
  if (!is.null(res$data) && nrow(res$data) > 0) {
    df <- res$data
    df$var_name <- s$var_name
    df$ciiu     <- s$ciiu
    df$moneda   <- s$moneda
    raw_list[[s$var_name]] <- df
  }
  Sys.sleep(0.7)
}

if (length(raw_list) == 0) {
  warning("[28] No se descargaron series del cuadro 144. Verificar códigos.")
  quit(save = "no")
}

panel <- bind_rows(raw_list) %>%
  mutate(date = as.Date(date)) %>%
  arrange(ciiu, moneda, date)

write_csv(panel, "data_intermediate/credit_sectoral_144.csv")

# -------------------------------------------------------------------------
# 3. Tasas de crecimiento interanual por sector y moneda
# -------------------------------------------------------------------------
growth <- panel %>%
  group_by(ciiu, moneda) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(g_yoy = 100 * (value / lag(value, 12) - 1)) %>%
  ungroup()

# Promedio 2024 vs 2025 por sector y por moneda
g_summary <- growth %>%
  filter(!is.na(g_yoy)) %>%
  mutate(year = year(date)) %>%
  filter(year %in% c(2024, 2025)) %>%
  group_by(ciiu, moneda, year) %>%
  summarise(g_mean = mean(g_yoy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = g_mean, names_prefix = "g_") %>%
  mutate(deceleration = g_2025 - g_2024) %>%
  left_join(ciiu_label, by = "ciiu") %>%
  arrange(deceleration)

write_csv(g_summary, "output/credit_sectoral_growth_rates.csv")

# Sumar por sector (sumando ambas monedas como saldo total)
g_total_sector <- growth %>%
  filter(!is.na(g_yoy)) %>%
  group_by(ciiu, date) %>%
  summarise(value_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(ciiu) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(g_yoy_total = 100 * (value_total / lag(value_total, 12) - 1)) %>%
  ungroup() %>%
  filter(!is.na(g_yoy_total)) %>%
  mutate(year = year(date)) %>%
  filter(year %in% c(2024, 2025)) %>%
  group_by(ciiu, year) %>%
  summarise(g_mean = mean(g_yoy_total, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = g_mean, names_prefix = "g_") %>%
  mutate(deceleration = g_2025 - g_2024) %>%
  left_join(ciiu_label, by = "ciiu") %>%
  arrange(deceleration)

write_csv(g_total_sector, "output/credit_sectoral_deceleration_ranking.csv")

# -------------------------------------------------------------------------
# 4. Gráfico de desaceleración por sector
# -------------------------------------------------------------------------
p <- g_total_sector %>%
  ggplot(aes(x = reorder(sector_label, deceleration), y = deceleration)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  coord_flip() +
  labs(
    title    = "Costa Rica: desaceleración del crédito por sector (2025 vs 2024)",
    subtitle = "Diferencia entre tasa interanual promedio 2025 y 2024, puntos porcentuales",
    x = NULL, y = "Desaceleración (pp)",
    caption  = "Fuente: BCCR, cuadro 144 (crédito SBN por actividad CIIU rev 4)."
  ) +
  theme_minimal(base_size = 11)

ggsave("output/fig_credit_sectoral_deceleration.png", p, width = 9, height = 4.5, dpi = 200)

message("[28] OK — Outputs en output/credit_sectoral_*.csv y figura.")
