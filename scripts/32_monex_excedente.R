# =========================================================================
# 32_monex_excedente.R
# -------------------------------------------------------------------------
# Captura las series del Mercado de Monedas Extranjeras (MONEX) y de
# ventanillas autorizadas del BCCR: monto diario negociado, demanda y
# oferta de divisas. Compila el excedente neto del mercado cambiario,
# diario y anual, para validar las cifras citadas en la sección 5 del
# paper:
#   - Oferta diaria promedio 2025 ≈ USD 140 millones
#   - Demanda diaria promedio 2025 ≈ USD 114 millones
#   - Excedente neto diario ≈ USD 26 millones
#   - Multiplicación del excedente anual entre 2015 y 2025 (> 5x)
# -------------------------------------------------------------------------
# Outputs:
#   data_intermediate/monex_diario.csv
#   output/monex_excedente_anual.csv
#   output/fig_monex_excedente.png
# -------------------------------------------------------------------------
# Códigos SDDE candidatos del MONEX (verificar contra el catálogo público
# en gee.bccr.fi.cr antes de la primera corrida productiva):
#   3322 — TC promedio MONEX (validado, ya en el catálogo principal)
#   3323 — TC venta MONEX
#   3324 — Monto negociado MONEX (millones USD, diario)
#   3325 — Demanda diaria MONEX (millones USD)
#   3326 — Oferta diaria MONEX (millones USD)
#
# Si los códigos exactos difieren, ajustar la tabla `monex_codes` y
# rerun. El script descarga lo que encuentra y reporta lo que no.
# =========================================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(tidyr)
  library(lubridate); library(ggplot2); library(readr)
})

source("scripts/utils_bccr.R")

monex_codes <- tribble(
  ~var_name,         ~code,    ~descr,
  "monex_monto",     "3324",   "Monto negociado MONEX (millones USD, diario)",
  "monex_demanda",   "3325",   "Demanda diaria MONEX (millones USD)",
  "monex_oferta",    "3326",   "Oferta diaria MONEX (millones USD)"
)

message("[32] Descargando series MONEX desde SDDE…")
creds <- get_bccr_credentials()

raw_list <- list()
for (i in seq_len(nrow(monex_codes))) {
  r <- monex_codes[i,]
  message(sprintf("  - %s (%s)", r$var_name, r$code))
  res <- tryCatch(
    download_bccr_series(r$code,
                         start_date = "2015/01/01",
                         end_date   = format(Sys.Date(), "%Y/%m/%d"),
                         credentials = creds, verbose = FALSE,
                         save_raw = TRUE),
    error = function(e) { message("    error: ", e$message); NULL }
  )
  if (!is.null(res$data) && nrow(res$data) > 0) {
    d <- res$data
    d$var_name <- r$var_name
    raw_list[[r$var_name]] <- d
  } else {
    message(sprintf("    [%s] devolvió vacío o falló — código sin datos disponibles", r$code))
  }
  Sys.sleep(0.7)
}

if (length(raw_list) == 0) {
  message("[32] No se descargaron series MONEX. Ajustar códigos en monex_codes y reintentar.")
  message("    Consultar el catálogo BCCR en https://gee.bccr.fi.cr para verificar")
  message("    los códigos vigentes de oferta, demanda y monto negociado MONEX.")
  quit(save = "no", status = 1)
}

panel <- bind_rows(raw_list) %>%
  mutate(date = as.Date(date)) %>%
  select(var_name, date, value) %>%
  pivot_wider(names_from = var_name, values_from = value)

write_csv(panel, "data_intermediate/monex_diario.csv")

# Excedente diario
if (all(c("monex_oferta","monex_demanda") %in% names(panel))) {
  panel <- panel %>%
    mutate(monex_excedente = monex_oferta - monex_demanda)
} else {
  message("[32] Falta oferta o demanda — no se puede computar excedente.")
  quit(save = "no", status = 1)
}

# Compilación anual
anual <- panel %>%
  filter(!is.na(monex_excedente)) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    oferta_diaria_prom   = mean(monex_oferta,   na.rm = TRUE),
    demanda_diaria_prom  = mean(monex_demanda,  na.rm = TRUE),
    excedente_diario_prom= mean(monex_excedente,na.rm = TRUE),
    excedente_anual_acum = sum(monex_excedente, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )
write_csv(anual, "output/monex_excedente_anual.csv")

# Reportes
cat("\n=== MONEX excedente anual ===\n")
print(anual)

# Comparación 2015 vs 2025 (paper claim: x >5)
y2015 <- anual %>% filter(year == 2015) %>% pull(excedente_anual_acum)
y2025 <- anual %>% filter(year == 2025) %>% pull(excedente_anual_acum)
if (length(y2015) && length(y2025) && y2015 > 0) {
  ratio <- y2025 / y2015
  cat(sprintf("\nRatio excedente 2025 / 2015 = %.2fx\n", ratio))
}

# Gráfico
p <- anual %>%
  ggplot(aes(x = year, y = excedente_anual_acum/1000)) +
  geom_col(fill = "steelblue") +
  labs(
    title    = "MONEX: excedente cambiario anual (oferta − demanda)",
    subtitle = "Suma de excedentes diarios por año",
    x = NULL, y = "Excedente (miles de millones USD)",
    caption  = "Fuente: BCCR, Mercado de Monedas Extranjeras (MONEX)."
  ) +
  theme_minimal(base_size = 11)
ggsave("output/fig_monex_excedente.png", p, width = 9, height = 4.5, dpi = 200)

message("[32] OK — output/monex_excedente_anual.csv y figura.")
