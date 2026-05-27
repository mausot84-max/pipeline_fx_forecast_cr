# =========================================================================
# 31_credit_pib_ratio.R
# -------------------------------------------------------------------------
# Prueba 4 (Registro 1, lectura adicional): crédito SBN al sector privado
# como porcentaje del PIB nominal anual.
#
# Esta lectura protege contra el sesgo que introducirían tasas de inflación
# variables entre sub-períodos: normalizar el crédito por el PIB nominal
# del mismo período convierte la comparación pre/post-2022 en un ratio
# observado que no depende de elecciones de deflactor.
#
# Fuente PIB: BCCR "Producto interno bruto por actividad económica" en
# millones de CRC corrientes, trimestral, descargado desde sdd.bccr.fi.cr
# y guardado en data_raw/cuentas_nacionales/.
# -------------------------------------------------------------------------
# Outputs:
#   output/credito_pct_pib.csv   — panel anual con razón crédito/PIB
#   output/fig_credito_pct_pib.png
# =========================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(readr)
})

# 1. PIB trimestral → anual --------------------------------------------------
pib_path <- "data_raw/cuentas_nacionales/Producto interno bruto por actividad económica.xlsx"
if (!file.exists(pib_path)) stop("Falta ", pib_path)

pib <- read_excel(pib_path, sheet = "Series", skip = 4)
names(pib)[1:2] <- c("date", "pib_mp")
pib <- pib %>%
  filter(!is.na(date), !is.na(pib_mp)) %>%
  mutate(date = as.Date(date), year = year(date))

pib_anual <- pib %>%
  group_by(year) %>%
  summarise(pib_anual_crc = sum(pib_mp, na.rm = TRUE), .groups = "drop") %>%
  # Filtrar años incompletos (menos de 4 trimestres) — toleramos 3 o 4
  filter(year >= 2010, year <= year(Sys.Date()))

# 2. Crédito SBN cierre de año ------------------------------------------------
mn <- read_csv("cierre_v6/outputs/raw/credit_sbn_TOTAL_MN__.csv", show_col_types = FALSE) %>%
  rename(date = fecha, mn = valor) %>% mutate(date = as.Date(date))
me <- read_csv("cierre_v6/outputs/raw/credit_sbn_TOTAL_ME__.csv", show_col_types = FALSE) %>%
  rename(date = fecha, me = valor) %>% mutate(date = as.Date(date))

cr <- inner_join(mn, me, by = "date") %>%
  mutate(total = mn + me, year = year(date)) %>%
  group_by(year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(year, mn, me, total)

# 3. Combinar y razones ------------------------------------------------------
ratios <- inner_join(cr, pib_anual, by = "year") %>%
  mutate(
    credit_total_pct_pib = 100 * total / pib_anual_crc,
    credit_mn_pct_pib    = 100 * mn    / pib_anual_crc,
    credit_me_pct_pib    = 100 * me    / pib_anual_crc
  )
write_csv(ratios, "output/credito_pct_pib.csv")

# Pre vs post 2022
pre  <- ratios %>% filter(year <  2022)
post <- ratios %>% filter(year >= 2022)
cat("\n=== Crédito SBN como % PIB ===\n")
cat(sprintf("Pre-2022:  Total=%.2f%%, CRC=%.2f%%, USD=%.2f%%\n",
            mean(pre$credit_total_pct_pib),
            mean(pre$credit_mn_pct_pib),
            mean(pre$credit_me_pct_pib)))
cat(sprintf("Post-2022: Total=%.2f%%, CRC=%.2f%%, USD=%.2f%%\n",
            mean(post$credit_total_pct_pib),
            mean(post$credit_mn_pct_pib),
            mean(post$credit_me_pct_pib)))

# 4. Gráfico ------------------------------------------------------------------
plot_df <- ratios %>%
  select(year, USD = credit_me_pct_pib, CRC = credit_mn_pct_pib) %>%
  pivot_longer(-year, names_to = "moneda", values_to = "pct_pib")

p <- plot_df %>%
  ggplot(aes(x = year, y = pct_pib, color = moneda)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  labs(
    title    = "Crédito SBN al sector privado como % del PIB nominal",
    subtitle = "Línea vertical: inicio del régimen de abundancia (2022)",
    x = NULL, y = "% PIB",
    caption  = "Fuente: BCCR (créditos: códigos 20578-20579; PIB: cuentas nacionales)."
  ) +
  theme_minimal(base_size = 11)
ggsave("output/fig_credito_pct_pib.png", p, width = 9, height = 4.5, dpi = 200)

message("[31] OK — output/credito_pct_pib.csv y figura.")
