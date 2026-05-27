# =========================================================================
# 34_pinza_cambiaria_sectorial.R
# -------------------------------------------------------------------------
# Pinza cambiaria por actividad económica desde el reporte 1900 del SDDE
# del BCCR ("Operaciones cambiarias de contado"). Procesa tres Excels:
# Compra, Venta y Neto del mercado cambiario de contado en USD, por
# domicilio y actividad económica del cliente, mensual.
#
# Fuente: https://sdd.bccr.fi.cr/es/IndicadoresEconomicos/Inicio/Reporte/1900
# Descarga manual via "Exportar datos → Datos resumidos → xlsx" para los
# tres tipos de negociación. Archivos en data_raw/sdde_reporte_1900/.
#
# Outputs:
#   output/pinza_cambiaria_panel.csv         — panel sector × fecha × tipo
#   output/pinza_cambiaria_sectorial.csv     — saldo acumulado por sector
#   output/pinza_cambiaria_anual.csv         — promedios mensual/diario
#   output/fig_pinza_cambiaria.png           — barras por sector
# =========================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(readr); library(stringr)
})

base <- "data_raw/sdde_reporte_1900"
load_one <- function(fname, tipo) {
  path <- file.path(base, fname)
  if (!file.exists(path)) stop("Falta ", path)
  # Read raw to get year row 1 + month row 2
  raw <- read_excel(path, col_names = FALSE)
  # Row 1 (índice 1) tiene año en algunas columnas; Row 2 los meses; Row 3 col labels;
  # Filas 4+ datos.
  yr_row <- as.character(raw[1, ])
  mo_row <- as.character(raw[2, ])
  # Forward-fill year across columns
  cur <- NA_character_
  yr_filled <- sapply(yr_row, function(x) {
    if (!is.na(x) && nchar(x) > 0 && grepl("^[0-9]{4}$", x)) cur <<- x
    cur
  })
  mes_map <- c(ene=1, feb=2, mar=3, abr=4, may=5, jun=6,
               jul=7, ago=8, sep=9, oct=10, nov=11, dic=12)
  rows <- list()
  current_actividad <- NA_character_
  for (r in 4:(nrow(raw))) {
    act <- as.character(raw[r, 1])
    desc <- as.character(raw[r, 2])
    if (!is.na(act) && nchar(act) > 0 && act != "Total")
      current_actividad <- act
    if (is.na(desc) || desc == "Total" || grepl("Filtros", desc)) next
    for (c in 3:ncol(raw)) {
      v <- raw[r, c][[1]]
      if (is.na(v)) next
      yr <- yr_filled[c]
      mo <- mo_row[c]
      if (is.na(yr) || is.na(mo) || !(mo %in% names(mes_map))) next
      rows[[length(rows)+1]] <- data.frame(
        tipo = tipo,
        actividad = current_actividad,
        desc = desc,
        year = as.integer(yr),
        mes  = as.integer(mes_map[mo]),
        valor_mn_usd = as.numeric(v)
      )
    }
  }
  bind_rows(rows)
}

panel <- bind_rows(
  load_one("reporte_1900_compra.xlsx", "Compra"),
  load_one("reporte_1900_venta.xlsx",  "Venta"),
  load_one("reporte_1900_neto.xlsx",   "Neto")
) %>%
  mutate(fecha = as.Date(sprintf("%04d-%02d-01", year, mes)))

write_csv(panel, "output/pinza_cambiaria_panel.csv")

# 1) Acumulado por sector y tipo
sectorial <- panel %>%
  group_by(desc, tipo) %>%
  summarise(acum_mn_usd = sum(valor_mn_usd, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = tipo, values_from = acum_mn_usd) %>%
  mutate(saldo_neto = Compra - Venta) %>%
  arrange(saldo_neto)
write_csv(sectorial, "output/pinza_cambiaria_sectorial.csv")
cat("\n=== Saldo cambiario acumulado por sector (mil M USD) ===\n")
print(sectorial %>% mutate(across(c(Compra,Venta,Neto,saldo_neto), ~ round(./1000, 2))))

# 2) Promedios mensual y daily
anual <- panel %>%
  group_by(year, tipo) %>%
  summarise(mes_promedio_mn_usd = sum(valor_mn_usd, na.rm = TRUE) /
                                    n_distinct(mes), .groups = "drop") %>%
  pivot_wider(names_from = tipo, values_from = mes_promedio_mn_usd) %>%
  mutate(
    Compra_diaria = Compra / 22,
    Venta_diaria  = Venta  / 22,
    Neto_diaria   = Neto   / 22
  )
write_csv(anual, "output/pinza_cambiaria_anual.csv")
cat("\n=== Promedio mensual y diario por año ===\n")
print(anual %>% mutate(across(-year, ~ round(., 2))))

# 3) Gráfico de barras
p <- sectorial %>%
  filter(!desc %in% c("Sin identificación 2/", "No disponible 1/", "Inactivas o desinscritas")) %>%
  mutate(desc_corto = str_wrap(desc, 30)) %>%
  ggplot(aes(x = reorder(desc_corto, saldo_neto), y = saldo_neto/1000,
             fill = saldo_neto > 0)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    labels = c("TRUE" = "oferente neto", "FALSE" = "demandante neto"),
                    name = NULL) +
  labs(
    title    = "Saldo cambiario acumulado por sector económico, abr-2024 a abr-2026",
    subtitle = "Compra − Venta de USD en el mercado de contado del BCCR (reporte 1900)",
    x = NULL, y = "Saldo acumulado (mil M USD)",
    caption  = "Fuente: BCCR, reporte 1900 — Operaciones cambiarias de contado."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("output/fig_pinza_cambiaria.png", p, width = 9, height = 6, dpi = 200)
message("[34] OK — pinza cambiaria sectorial computada.")
