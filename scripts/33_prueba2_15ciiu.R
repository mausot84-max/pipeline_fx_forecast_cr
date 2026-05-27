# =========================================================================
# 33_prueba2_15ciiu.R
# -------------------------------------------------------------------------
# Prueba 2 reescrita: tests de Chow / sup-Wald de cambio estructural sobre
# las 15 secciones CIIU rev 4 individuales (base 2022 tendencia ciclo),
# sin agregación a buckets sectoriales.
#
# Fuente: BCCR, Excel "Índice mensual de actividad económica por industrias"
# en data_raw/imaes_web/. Tasa interanual computada como (nivel_t/nivel_{t-12}-1)*100.
#
# Fechas candidatas: 2018-01-01, 2020-03-01, 2022-01-01.
# -------------------------------------------------------------------------
# Outputs:
#   output/prueba2_15ciiu.csv       — F, p-value por sector y fecha
#   output/fig_prueba2_15ciiu.png   — heatmap p-values
# =========================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(readr)
})

# 1. Cargar IMAE base 2022 por sección CIIU
imae_path <- "data_raw/imaes_web/Índice mensual de actividad económica por industrias.xlsx"
if (!file.exists(imae_path)) stop("Falta ", imae_path)
df <- read_excel(imae_path, sheet = "Series", skip = 4)
names(df)[1] <- "date"
df$date <- as.Date(df$date)
df <- df %>% filter(!is.na(date)) %>% arrange(date)

# 2. Mapping de columnas (nivel) a etiqueta CIIU
sector_map <- list(
  "A"   = list(col = "Agricultura, silvicultura y pesca. Nivel",            name = "Agropecuario"),
  "B"   = list(col = "Extracción de minas y canteras. Nivel",                name = "Minas"),
  "C"   = list(col = "Industria manufacturera. Nivel",                       name = "Manufactura"),
  "D-E" = list(col = "Electricidad, agua y serv. saneamiento. Nivel",        name = "Electricidad/agua"),
  "F"   = list(col = "Construcción. Nivel",                                  name = "Construcción"),
  "G"   = list(col = "Comercio. Nivel",                                       name = "Comercio"),
  "H"   = list(col = "Transporte, almacenamiento. Nivel",                    name = "Transporte"),
  "I"   = list(col = "Activ alojamiento y serv de comida. Nivel",            name = "Alojamiento"),
  "J"   = list(col = "Información y comunicaciones. Nivel",                  name = "Información/Comunicaciones"),
  "K"   = list(col = "Actividades financieras y de seguros. Nivel",          name = "Financieras"),
  "L"   = list(col = "Actividades inmobiliarias. Nivel",                     name = "Inmobiliarias"),
  "M-N" = list(col = "Profes, cient, téc, admin y serv apoyo. Nivel",        name = "Profesional"),
  "O"   = list(col = "Adm. pública y planes seguridad social. Nivel",        name = "Adm. pública"),
  "P-Q" = list(col = "Enseñanza y activ. de la salud humana. Nivel",         name = "Enseñanza/salud"),
  "R-S" = list(col = "Otras actividades. Nivel",                              name = "Otras")
)

# 3. Función Chow test (cambio en media)
chow_test <- function(y, idx) {
  n <- length(y); k <- 1
  rss_p <- sum((y - mean(y))^2)
  y1 <- y[1:idx]; y2 <- y[(idx+1):n]
  rss_u <- sum((y1 - mean(y1))^2) + sum((y2 - mean(y2))^2)
  F  <- ((rss_p - rss_u)/k) / (rss_u/(n - 2*k))
  p  <- 1 - pf(F, k, n - 2*k)
  list(F = F, p = p)
}

candidate_dates <- as.Date(c("2018-01-01", "2020-03-01", "2022-01-01"))

# 4. Iterar 15 sectores
results <- list()
for (ciiu in names(sector_map)) {
  col  <- sector_map[[ciiu]]$col
  name <- sector_map[[ciiu]]$name
  if (!(col %in% names(df))) next
  s <- df %>%
    transmute(date, nivel = .data[[col]]) %>%
    filter(!is.na(nivel)) %>%
    arrange(date) %>%
    mutate(yoy = 100 * (nivel / lag(nivel, 12) - 1)) %>%
    filter(!is.na(yoy), date >= as.Date("2010-01-01"))
  if (nrow(s) < 50) next
  y <- s$yoy
  row <- list(ciiu = ciiu, name = name)
  for (d in candidate_dates) {
    idx <- which(s$date >= d)[1]
    if (is.na(idx) || idx < 12 || idx > length(y) - 12) {
      row[[paste0("F_", as.character(d))]] <- NA
      row[[paste0("p_", as.character(d))]] <- NA
      next
    }
    res <- chow_test(y, idx)
    row[[paste0("F_", as.character(d))]] <- res$F
    row[[paste0("p_", as.character(d))]] <- res$p
  }
  results[[ciiu]] <- as.data.frame(row, stringsAsFactors = FALSE)
}

out <- bind_rows(results)
write_csv(out, "output/prueba2_15ciiu.csv")

cat("\n=== Prueba 2 — Chow sobre 15 secciones CIIU (IMAE base 2022 tendencia ciclo) ===\n")
print(out, row.names = FALSE)

# Resumen
for (d in candidate_dates) {
  pcol <- paste0("p_", as.character(d))
  n_break <- sum(out[[pcol]] < 0.05, na.rm = TRUE)
  cat(sprintf("\n  @ %s: %d/%d sectores rechazo al 5%%\n", d, n_break, nrow(out)))
}

# 5. Gráfico (opcional)
plot_df <- out %>%
  pivot_longer(-c(ciiu, name), names_to = "key", values_to = "value") %>%
  mutate(
    stat = if_else(grepl("^F_", key), "F", "p"),
    date = sub("^[Fp]_", "", key)
  ) %>%
  filter(stat == "p") %>%
  mutate(
    sig  = if_else(value < 0.05, "rechazo", "estable"),
    name = factor(name, levels = unique(out$name))
  )

p <- ggplot(plot_df, aes(x = date, y = name, fill = sig)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("rechazo" = "firebrick", "estable" = "lightblue")) +
  labs(
    title    = "Prueba 2: cambio estructural por sección CIIU rev 4",
    subtitle = "Chow test sobre la tasa interanual del IMAE (base 2022 tendencia ciclo)",
    x = NULL, y = NULL, fill = "5% sig.",
    caption  = "Fuente: BCCR, Excel \"Índice mensual de actividad económica por industrias\"."
  ) +
  theme_minimal(base_size = 11)

ggsave("output/fig_prueba2_15ciiu.png", p, width = 8, height = 5, dpi = 200)

message("[33] OK — output/prueba2_15ciiu.csv y figura.")
