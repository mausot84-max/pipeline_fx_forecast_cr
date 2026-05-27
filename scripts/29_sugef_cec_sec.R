# =========================================================================
# 29_sugef_cec_sec.R
# -------------------------------------------------------------------------
# Prueba 5 (Registro 3): exposición al riesgo cambiario por deudor según
# clasificación SUGEF (Acuerdo 2-10):
#   - CEC: deudores Con Exposición Cambiaria — sin generación de divisas
#   - SEC: deudores Sin Exposición Cambiaria — con calce natural
#
# Fuente: SUGEF, reporte "Información financiera del Sistema Financiero
# Nacional" (trimestral, publicado en sugef.fi.cr). Disponible desde
# diciembre 2022 con metodología Acuerdo 2-10.
#
# Tesis: pese a la apreciación nominal sostenida que abarataría el servicio
# de la deuda en USD en términos de CRC, el flujo de nuevo crédito a
# deudores CEC no se está incrementando; al contrario, se estabiliza o cae.
# Esto valida que la demanda crediticia se está comprimiendo en el segmento
# más expuesto, contrario al pronóstico cambiario benigno.
# -------------------------------------------------------------------------
# Outputs:
#   data_intermediate/sugef_cec_sec_panel.csv     — panel trimestral
#   output/sugef_cec_ratios.csv                   — razones y flujos
#   output/fig_sugef_cec_flows.png                — gráfico de flujos
# =========================================================================

suppressPackageStartupMessages({
  library(httr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
})

# -------------------------------------------------------------------------
# 1. Configuración de ingesta SUGEF
# -------------------------------------------------------------------------
# La SUGEF publica los reportes trimestrales en formato Excel desde
# diciembre 2022. El script intentará dos canales:
#
# (a) Endpoint público de "Información financiera" si está disponible
#     (https://www.sugef.fi.cr/...). Caso contrario:
#
# (b) Lectura local desde data_raw/sugef/ donde el operador habrá colocado
#     manualmente los archivos Excel descargados, con nombre del tipo
#     "sugef_cec_sec_YYYY_QN.xlsx" (un archivo por trimestre).
#
# El operador debe asegurarse de tener los archivos en data_raw/sugef/.

sugef_dir <- "data_raw/sugef"
dir.create(sugef_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(sugef_dir, pattern = "sugef_cec_sec_.*\\.xlsx$", full.names = TRUE)
if (length(files) == 0) {
  message(sprintf("[29] No hay archivos en %s.", sugef_dir))
  message("    Descargar manualmente desde https://www.sugef.fi.cr/")
  message("    los reportes trimestrales de exposición cambiaria (CEC/SEC)")
  message("    desde diciembre 2022 y colocarlos con nombre")
  message("    sugef_cec_sec_YYYY_QN.xlsx en data_raw/sugef/.")
  quit(save = "no", status = 1)
}

# -------------------------------------------------------------------------
# 2. Lectura y consolidación
# -------------------------------------------------------------------------
parse_sugef <- function(path) {
  # Se asume una hoja con columnas:
  #   - segmento ("CEC" / "SEC" / "total")
  #   - moneda   ("CRC" / "USD" / "total")
  #   - saldo en millones de CRC
  # El parser real debe ajustarse al layout exacto del Excel SUGEF.
  fname <- basename(path)
  m <- regmatches(fname, regexec("sugef_cec_sec_(\\d{4})_Q(\\d)\\.xlsx", fname))[[1]]
  year <- as.integer(m[2]); q <- as.integer(m[3])
  date <- as.Date(sprintf("%d-%02d-01", year, q * 3))  # fin de trimestre aprox.
  df <- read_excel(path)
  # Normalizar nombres (depende del layout SUGEF real)
  names(df) <- tolower(names(df))
  df <- df %>% mutate(date = date)
  df
}

panel_raw <- bind_rows(lapply(files, parse_sugef)) %>%
  arrange(date)

write_csv(panel_raw, "data_intermediate/sugef_cec_sec_panel.csv")

# -------------------------------------------------------------------------
# 3. Razones CEC/total y CEC/USD; flujos netos trimestrales
# -------------------------------------------------------------------------
# Para que el script corra de extremo a extremo, calculamos métricas
# canónicas sobre un panel tipo wide:
#   - cec_total: saldo total de CEC (CRC + USD equivalente)
#   - sec_total: saldo total de SEC
#   - cec_usd:   saldo CEC en USD equivalente en CRC
#   - me_total:  saldo total cartera en moneda extranjera

# Adaptar según el layout final del Excel SUGEF; placeholder columnar:
required_cols <- c("date", "cec_total", "sec_total", "cec_usd", "me_total")
have <- intersect(required_cols, names(panel_raw))
if (!all(required_cols %in% names(panel_raw))) {
  message(sprintf("[29] Faltan columnas en el panel SUGEF: %s",
                  paste(setdiff(required_cols, names(panel_raw)), collapse = ", ")))
  message("    Ajustar parse_sugef() según layout real del Excel.")
  quit(save = "no", status = 1)
}

ratios <- panel_raw %>%
  arrange(date) %>%
  mutate(
    cec_share_total = cec_total / (cec_total + sec_total),
    cec_share_me    = cec_usd  / me_total,
    cec_flow_qoq    = cec_total - lag(cec_total),
    me_flow_qoq     = me_total  - lag(me_total)
  )

write_csv(ratios, "output/sugef_cec_ratios.csv")

# Resumen "headline":
last_obs <- tail(ratios, 1)
message(sprintf("[29] Última observación %s:", as.character(last_obs$date)))
message(sprintf("    CEC/total       = %.2f%%", 100 * last_obs$cec_share_total))
message(sprintf("    CEC/cartera ME  = %.2f%%", 100 * last_obs$cec_share_me))
message(sprintf("    Flujo CEC trim. = %.0f millones CRC", last_obs$cec_flow_qoq))

# -------------------------------------------------------------------------
# 4. Gráfico de flujos CEC vs ME total
# -------------------------------------------------------------------------
plot_df <- ratios %>%
  select(date, cec_flow_qoq, me_flow_qoq) %>%
  pivot_longer(-date, names_to = "serie", values_to = "valor") %>%
  mutate(serie = recode(serie,
    "cec_flow_qoq" = "Flujo trimestral cartera CEC",
    "me_flow_qoq"  = "Flujo trimestral cartera total ME"
  ))

p <- plot_df %>%
  ggplot(aes(x = date, y = valor, fill = serie)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title    = "Costa Rica: flujos trimestrales de la cartera en moneda extranjera",
    subtitle = "CEC = deudores Con Exposición Cambiaria; ME = moneda extranjera total",
    x = NULL, y = "Flujo trimestral (millones CRC)",
    caption  = "Fuente: SUGEF, Acuerdo 2-10, reportes trimestrales desde diciembre 2022."
  ) +
  scale_fill_manual(values = c("steelblue", "gray60")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("output/fig_sugef_cec_flows.png", p, width = 9, height = 4.5, dpi = 200)

message("[29] OK — Outputs en output/sugef_cec_*.csv y figura.")
