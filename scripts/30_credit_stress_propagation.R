# =========================================================================
# 30_credit_stress_propagation.R
# -------------------------------------------------------------------------
# Extensión de la sección 8 del paper (escenario de estrés Ormuz):
# proyecta qué le pasa a las tasas de crecimiento del crédito por sector
# y por moneda bajo el escenario adverso del BCCR, aplicando el
# diferencial de respuesta sectorial observado en 2024–2025.
#
# Tesis: bajo un shock de petróleo y compresión de márgenes en el bloque
# Transable, la propagación al crédito amplifica la desaceleración en los
# sectores ya identificados como más expuestos en el Registro 2.
# -------------------------------------------------------------------------
# Outputs:
#   output/credit_stress_propagation_table.csv  — tasas baseline vs estrés
#   output/fig_credit_stress_propagation.png    — gráfico contrafactual
# =========================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(readr)
})

# -------------------------------------------------------------------------
# 1. Carga del baseline sectorial (Registro 2) y del escenario Ormuz (sec 8)
# -------------------------------------------------------------------------
baseline <- read_csv("output/credit_sectoral_deceleration_ranking.csv",
                     show_col_types = FALSE)

# El escenario Ormuz se construye en 23_islmbp_simulate.R. Asumimos que
# produce, entre otros, un shock sobre el ITCER (apreciación adicional)
# y un shock sobre el costo de combustibles transables. Los outputs
# canónicos están en output/islmbp_ormuz_summary.csv:

stress <- tryCatch(
  read_csv("output/islmbp_ormuz_summary.csv", show_col_types = FALSE),
  error = function(e) NULL
)

if (is.null(stress)) {
  message("[30] No existe output/islmbp_ormuz_summary.csv todavía.")
  message("    Correr antes 22_islmbp_calibrate_shock.R y 23_islmbp_simulate.R.")
  message("    Continuando con valores ilustrativos del escenario.")
  # Valores ilustrativos (a sustituir por los reales del Ormuz simulado)
  stress <- tibble(
    ciiu = c("A","F","G","H","I","J","MN"),
    elast_imae_itcer = c(-0.30, -0.45, -0.20, -0.10, -0.55, 0.05, 0.10),
    shock_itcer_pct  = rep(-3.0, 7)  # apreciación adicional 3% en estrés
  )
}

# -------------------------------------------------------------------------
# 2. Proyección de la tasa de crédito bajo estrés
# -------------------------------------------------------------------------
# Hipótesis: el coeficiente de transmisión sectorial del IMAE al crédito
# se estima como la elasticidad del g_yoy del crédito al g_yoy del IMAE
# por sector durante 2024-2025. Por simplicidad lo asumimos = 1.0 inicial
# y se corrige una vez se estimen las regresiones sectoriales.

elast_credit_imae <- 1.0

projection <- baseline %>%
  left_join(stress, by = "ciiu") %>%
  mutate(
    delta_imae_stress   = elast_imae_itcer * shock_itcer_pct,
    delta_credit_stress = elast_credit_imae * delta_imae_stress,
    g_2025_stressed     = g_2025 + delta_credit_stress
  ) %>%
  select(ciiu, sector_label, g_2024, g_2025, g_2025_stressed, delta_credit_stress)

write_csv(projection, "output/credit_stress_propagation_table.csv")

# -------------------------------------------------------------------------
# 3. Gráfico
# -------------------------------------------------------------------------
plot_df <- projection %>%
  pivot_longer(c(g_2024, g_2025, g_2025_stressed),
               names_to = "escenario", values_to = "g_yoy") %>%
  mutate(escenario = recode(escenario,
    "g_2024"          = "2024 (observado)",
    "g_2025"          = "2025 (observado)",
    "g_2025_stressed" = "2025 con shock Ormuz"
  ))

p <- plot_df %>%
  ggplot(aes(x = reorder(sector_label, g_yoy), y = g_yoy, fill = escenario)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "black") +
  coord_flip() +
  labs(
    title    = "Costa Rica: tasa interanual del crédito sectorial bajo escenario Ormuz",
    subtitle = "Observado 2024–2025 vs proyección bajo shock simulado del paper §8",
    x = NULL, y = "Tasa interanual del crédito (%)",
    caption  = "Fuente: BCCR cuadro 144; escenario Ormuz construido en scripts 22-23."
  ) +
  scale_fill_manual(values = c("steelblue", "darkorange", "firebrick")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("output/fig_credit_stress_propagation.png", p, width = 9, height = 5, dpi = 200)

message("[30] OK — Output en output/credit_stress_propagation_table.csv y figura.")
