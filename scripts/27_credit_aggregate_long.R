# =========================================================================
# 27_credit_aggregate_long.R
# -------------------------------------------------------------------------
# Prueba 5 (Registro 1): serie agregada larga del crédito al sector privado.
# Identificación del cambio de régimen en la tasa de crecimiento mediante:
#   (a) Bai-Perron con fechas endógenas
#   (b) sup-Wald con candidato exógeno en 2022-01-01 (inicio del régimen de
#       abundancia documentado en la sección 5 del paper).
# -------------------------------------------------------------------------
# Outputs:
#   data_intermediate/credit_aggregate_long.csv      — serie panel mensual
#   output/credit_breakpoints_baiperron.csv          — fechas de quiebre
#   output/credit_supwald_2022.csv                   — estadístico y p-valor
#   output/credit_means_subperiods.csv               — medias por sub-período
#   output/fig_credit_aggregate_long.png             — gráfico de la serie
# =========================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
  library(strucchange)
  library(ggplot2)
  library(readr)
})

source("scripts/utils_bccr.R")

# -------------------------------------------------------------------------
# 1. Definición de series largas BCCR (crédito al sector privado, mensual)
# -------------------------------------------------------------------------
# Códigos SDDE (cuadro Estadísticas Monetarias y Financieras, mensual):
#   - Crédito total al sector privado, saldos en CRC nominal: código 1462
#   - Crédito al sector privado en moneda nacional (CRC):     código 1463
#   - Crédito al sector privado en moneda extranjera (USD):   código 1464
# Estos códigos cubren desde la década de 1990 y se actualizan mensualmente.
# Si los códigos no responden, el script intentará alternativos del cuadro 145.

series_to_download <- tribble(
  ~var_name,                ~code,    ~moneda,
  "credit_priv_total_nom",  "1462",   "TOTAL",
  "credit_priv_crc_nom",    "1463",   "CRC",
  "credit_priv_usd_nom",    "1464",   "USD",
  # Alternativos (intentar si los anteriores fallan)
  "credit_priv_total_alt",  "31791",  "TOTAL_ALT",
  "credit_priv_crc_alt",    "31792",  "CRC_ALT",
  "credit_priv_usd_alt",    "31793",  "USD_ALT"
)

# -------------------------------------------------------------------------
# 2. Descarga BCCR SDDE
# -------------------------------------------------------------------------
creds <- get_bccr_credentials()

fetch_bccr_long <- function(code, start = "1995/01/01", end = NULL) {
  if (is.null(end)) end <- format(Sys.Date(), "%Y/%m/%d")
  tryCatch({
    res <- download_bccr_series(code, start_date = start, end_date = end,
                                credentials = creds, verbose = FALSE,
                                save_raw = TRUE)
    if (is.null(res$data) || nrow(res$data) == 0) return(NULL)
    res$data %>% mutate(code = code)
  }, error = function(e) {
    message(sprintf("  [%s] error: %s", code, e$message))
    NULL
  })
}

message("[27] Descargando series largas de crédito BCCR…")
raw_list <- list()
for (i in seq_len(nrow(series_to_download))) {
  s <- series_to_download[i,]
  message(sprintf("  - %s (%s)", s$var_name, s$code))
  df <- fetch_bccr_long(s$code)
  if (!is.null(df)) {
    df$var_name <- s$var_name
    df$moneda   <- s$moneda
    raw_list[[s$var_name]] <- df
  }
  Sys.sleep(0.7)  # respetar throttling SDDE
}

if (length(raw_list) == 0) {
  stop("[27] No se pudo descargar ninguna serie de crédito. Revisar conectividad SDDE.")
}

raw_df <- bind_rows(raw_list) %>%
  mutate(date = as.Date(date)) %>%
  arrange(var_name, date)

write_csv(raw_df, "data_raw/bccr/credit_aggregate_long_raw.csv")

# -------------------------------------------------------------------------
# 3. Construcción del panel mensual (saldos) y deflactación por IPC
# -------------------------------------------------------------------------
# Pivote a panel ancho por var_name; mantener solo series con cobertura >= 200 obs.
panel <- raw_df %>%
  group_by(var_name) %>%
  filter(n() >= 200) %>%
  ungroup() %>%
  select(date, var_name, value) %>%
  pivot_wider(names_from = var_name, values_from = value) %>%
  arrange(date)

# Preferir series no-_alt si están disponibles; si no, caer al alt.
pick_first <- function(panel, primary, alt) {
  if (primary %in% names(panel) && sum(!is.na(panel[[primary]])) > 200) {
    panel[[primary]]
  } else if (alt %in% names(panel) && sum(!is.na(panel[[alt]])) > 200) {
    panel[[alt]]
  } else NA_real_
}

panel$credit_total <- pick_first(panel, "credit_priv_total_nom", "credit_priv_total_alt")
panel$credit_crc   <- pick_first(panel, "credit_priv_crc_nom",   "credit_priv_crc_alt")
panel$credit_usd   <- pick_first(panel, "credit_priv_usd_nom",   "credit_priv_usd_alt")

# IPC base 2015=100 — se asume ya disponible en data_intermediate
ipc <- read_csv("data_raw/bccr/bccr_25485.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  select(date, ipc = value)

panel <- panel %>%
  left_join(ipc, by = "date") %>%
  mutate(
    credit_total_real = credit_total / (ipc / 100),
    credit_crc_real   = credit_crc   / (ipc / 100),
    credit_usd_real   = credit_usd   / (ipc / 100)
  )

write_csv(panel %>% select(date, credit_total, credit_crc, credit_usd,
                            credit_total_real, credit_crc_real, credit_usd_real),
          "data_intermediate/credit_aggregate_long.csv")

# -------------------------------------------------------------------------
# 4. Tasa interanual y suavizada (media móvil 6 meses)
# -------------------------------------------------------------------------
panel <- panel %>%
  arrange(date) %>%
  mutate(
    g_total_yoy = 100 * (credit_total_real / lag(credit_total_real, 12) - 1),
    g_crc_yoy   = 100 * (credit_crc_real   / lag(credit_crc_real,   12) - 1),
    g_usd_yoy   = 100 * (credit_usd_real   / lag(credit_usd_real,   12) - 1)
  ) %>%
  mutate(
    g_total_ma6 = rollmean(g_total_yoy, k = 6, fill = NA, align = "right"),
    g_crc_ma6   = rollmean(g_crc_yoy,   k = 6, fill = NA, align = "right"),
    g_usd_ma6   = rollmean(g_usd_yoy,   k = 6, fill = NA, align = "right")
  )

# -------------------------------------------------------------------------
# 5. (a) Bai-Perron multibreak sobre g_total_ma6, ventana 1996-01 / fin
# -------------------------------------------------------------------------
y <- panel %>% filter(!is.na(g_total_ma6), date >= "1996-01-01") %>%
  select(date, g_total_ma6)
ts_y <- ts(y$g_total_ma6, start = c(year(y$date[1]), month(y$date[1])), frequency = 12)

bp_fit <- breakpoints(ts_y ~ 1, h = 0.15)  # h = mínimo 15% de la muestra por segmento
summary(bp_fit)

bp_dates <- y$date[breakpoints(bp_fit)$breakpoints]
write_csv(tibble(breakpoint_date = bp_dates),
          "output/credit_breakpoints_baiperron.csv")

# -------------------------------------------------------------------------
# 5. (b) sup-Wald sobre la fecha candidata 2022-01-01
# -------------------------------------------------------------------------
y_full <- panel %>% filter(!is.na(g_total_ma6), date >= "2010-01-01") %>%
  select(date, g_total_ma6)
T <- nrow(y_full)
tau <- which(y_full$date >= as.Date("2022-01-01"))[1]
if (is.na(tau)) stop("[27] Fecha 2022-01-01 no presente en la serie.")

# Test de Chow / sup-Wald clásico
ts_full <- ts(y_full$g_total_ma6, start = c(year(y_full$date[1]), month(y_full$date[1])), frequency = 12)
fs <- Fstats(ts_full ~ 1, from = max(0.15, tau / T), to = min(0.85, tau / T))
sw_stat <- sctest(fs, type = "supF")
write_csv(tibble(
  statistic = sw_stat$statistic,
  p_value   = sw_stat$p.value,
  candidate_date = "2022-01-01"
), "output/credit_supwald_2022.csv")

message(sprintf("  sup-Wald @ 2022-01-01: stat=%.3f, p=%.4f",
                sw_stat$statistic, sw_stat$p.value))

# -------------------------------------------------------------------------
# 6. Medias por sub-período (2010–2021 vs 2022–último mes)
# -------------------------------------------------------------------------
pre  <- panel %>% filter(date >= "2010-01-01", date <  "2022-01-01")
post <- panel %>% filter(date >= "2022-01-01")

means <- tribble(
  ~periodo,       ~g_total_mean, ~g_crc_mean, ~g_usd_mean,
  "pre_2022",     mean(pre$g_total_yoy,  na.rm = TRUE),
                  mean(pre$g_crc_yoy,    na.rm = TRUE),
                  mean(pre$g_usd_yoy,    na.rm = TRUE),
  "post_2022",    mean(post$g_total_yoy, na.rm = TRUE),
                  mean(post$g_crc_yoy,   na.rm = TRUE),
                  mean(post$g_usd_yoy,   na.rm = TRUE)
)
write_csv(means, "output/credit_means_subperiods.csv")

# -------------------------------------------------------------------------
# 7. Gráfico
# -------------------------------------------------------------------------
p <- panel %>%
  filter(date >= "1996-01-01", !is.na(g_total_ma6)) %>%
  ggplot(aes(x = date, y = g_total_ma6)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed") +
  labs(
    title    = "Costa Rica: tasa interanual del crédito real al sector privado",
    subtitle = "Media móvil 6 meses; línea vertical = inicio del régimen de abundancia",
    x = NULL, y = "Tasa interanual (%)",
    caption  = "Fuente: BCCR, Estadísticas Monetarias y Financieras. IPC base 2015."
  ) +
  theme_minimal(base_size = 11)

ggsave("output/fig_credit_aggregate_long.png", p, width = 9, height = 4.5, dpi = 200)

message("[27] OK — Outputs en output/credit_*.csv y output/fig_credit_aggregate_long.png")
