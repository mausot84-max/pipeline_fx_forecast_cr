# ============================================================
# 25_islmbp_figures.R — Figuras finales para el paper Lizano
# (motor de cuatro cuadrantes)
# ============================================================
# Figuras:
#   Fig 1. Hechos estilizados macro (ITCER, TPM, inflación)
#   Fig 2. Trayectorias por cuadrante, 2015-2025 (4 paneles)
#   Fig 3. Métricas históricas del impuesto silencioso
#          (squeeze, subsidy, amplitude)
#   Fig 4. Mapa de cuadrantes 2x2 con sectores posicionados
#   Fig 5. Trayectorias contrafactuales bajo Ormuz (paneles
#          por cuadrante + macro)
#   Fig 6. El impuesto silencioso revelado (Δ amplitude por
#          escenario y horizonte)
# ============================================================

log_msg("=== 25: ISLMBP figures (4 cuadrantes) ===")

panel <- readRDS("data_intermediate/islmbp/panel_islmbp_monthly.rds")
sims  <- readRDS("data_intermediate/islmbp/simulations.rds")

theme_paper <- function() {
  theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey90", linewidth = 0.3),
      plot.title         = element_text(face = "bold", size = 11),
      plot.subtitle      = element_text(colour = "grey40", size = 9),
      strip.text         = element_text(face = "bold"),
      legend.position    = "bottom",
      legend.title       = element_blank()
    )
}

quadrant_colors <- c(
  "TV — Muriendo (ingresos USD, costos CRC)"     = "firebrick3",
  "TH — Hedged USD (ingresos+costos USD)"        = "steelblue4",
  "NH — Hedged CRC (ingresos+costos CRC)"        = "grey40",
  "NB — Beneficiado (ingresos CRC, costos USD)"  = "darkgreen"
)

# ------ FIG 1 — Hechos estilizados ------------------------------
if (all(c("itcer","tpm","inflation_yoy") %in% names(panel))) {
  fig1_df <- panel %>%
    select(date, ITCER = itcer, TPM = tpm, `Inflación i.a.` = inflation_yoy) %>%
    pivot_longer(-date, names_to = "serie", values_to = "valor")

  fig1 <- ggplot(fig1_df, aes(x = date, y = valor)) +
    geom_line(colour = "steelblue4", linewidth = 0.6) +
    facet_wrap(~ serie, scales = "free_y", ncol = 1) +
    labs(title = "Figura 1. Hechos estilizados macroeconómicos",
         subtitle = "Costa Rica, 2010-2025",
         x = NULL, y = NULL) +
    theme_paper()
  ggsave("output/figures/paper_fig1_hechos.png", fig1,
         width = 7, height = 6, dpi = 200, bg = "white")
}

# ------ FIG 2 — Trayectorias por cuadrante ----------------------
quadrant_cols_present <- intersect(c("y_TV","y_TH","y_NH","y_NB"), names(panel))
if (length(quadrant_cols_present) >= 2) {
  base_date <- as.Date("2015-01-01")
  base_row  <- panel %>% filter(date >= base_date) %>% slice(1)

  fig2_df <- panel %>%
    filter(date >= base_date) %>%
    mutate(across(all_of(quadrant_cols_present),
                  ~ exp(.x - base_row[[cur_column()]]) * 100)) %>%
    select(date, all_of(quadrant_cols_present)) %>%
    pivot_longer(-date, names_to = "cuadrante", values_to = "indice") %>%
    mutate(cuadrante = case_when(
      cuadrante == "y_TV" ~ "TV — Muriendo (ingresos USD, costos CRC)",
      cuadrante == "y_TH" ~ "TH — Hedged USD (ingresos+costos USD)",
      cuadrante == "y_NH" ~ "NH — Hedged CRC (ingresos+costos CRC)",
      cuadrante == "y_NB" ~ "NB — Beneficiado (ingresos CRC, costos USD)"
    )) %>%
    filter(!is.na(indice))

  fig2 <- ggplot(fig2_df, aes(x = date, y = indice, colour = cuadrante)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
    scale_colour_manual(values = quadrant_colors) +
    labs(title = "Figura 2. Trayectorias por cuadrante, 2015-2025",
         subtitle = "Índices de actividad, base 100 = enero 2015",
         x = NULL, y = "Índice") +
    theme_paper()
  ggsave("output/figures/paper_fig2_cuadrantes_trayectoria.png", fig2,
         width = 7, height = 5, dpi = 200, bg = "white")
}

# ------ FIG 3 — Métricas históricas del impuesto silencioso -----
metric_cols <- intersect(c("tax_squeeze_TV","tax_subsidy_NB","silent_tax_amplitude"),
                          names(panel))
if (length(metric_cols) >= 1) {
  fig3_df <- panel %>%
    select(date, all_of(metric_cols)) %>%
    pivot_longer(-date, names_to = "metrica", values_to = "valor") %>%
    mutate(metrica = case_when(
      metrica == "tax_squeeze_TV"       ~ "Squeeze TV (TH menos TV)",
      metrica == "tax_subsidy_NB"       ~ "Subsidio NB (NB menos NH)",
      metrica == "silent_tax_amplitude" ~ "Amplitud total"
    )) %>%
    filter(!is.na(valor))

  fig3 <- ggplot(fig3_df, aes(x = date, y = valor * 100, colour = metrica)) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
    scale_colour_manual(values = c(
      "Squeeze TV (TH menos TV)" = "firebrick3",
      "Subsidio NB (NB menos NH)" = "darkgreen",
      "Amplitud total" = "steelblue4"
    )) +
    labs(title = "Figura 3. El impuesto silencioso histórico, 2016-2025",
         subtitle = "Brechas asimétricas de crecimiento (puntos porcentuales)",
         x = NULL, y = "pp i.a.") +
    theme_paper()
  ggsave("output/figures/paper_fig3_metricas_historicas.png", fig3,
         width = 7, height = 4, dpi = 200, bg = "white")
}

# ------ FIG 4 — Mapa del cuadrante 2x2 con posiciones -----------
mapping_file <- "data_intermediate/islmbp/quadrant_mapping.csv"
if (file.exists(mapping_file)) {
  mp <- readr::read_csv(mapping_file, show_col_types = FALSE)

  fig4 <- ggplot(mp, aes(x = cost_share_usd_used, y = income_share_usd_used,
                          colour = quadrant_assigned)) +
    annotate("rect", xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1,
             fill = "firebrick3", alpha = 0.05) +
    annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
             fill = "steelblue4", alpha = 0.05) +
    annotate("rect", xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5,
             fill = "grey40", alpha = 0.05) +
    annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5,
             fill = "darkgreen", alpha = 0.05) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
    geom_point(size = 4) +
    ggrepel::geom_text_repel(aes(label = ciiu), size = 3.5,
                              box.padding = 0.5,
                              max.overlaps = 20) +
    scale_colour_manual(values = c("TV" = "firebrick3",
                                     "TH" = "steelblue4",
                                     "NH" = "grey40",
                                     "NB" = "darkgreen")) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
    annotate("text", x = 0.25, y = 0.95, label = "TV — Muriendo",
              fontface = "bold", colour = "firebrick3", size = 4) +
    annotate("text", x = 0.75, y = 0.95, label = "TH — Hedged USD",
              fontface = "bold", colour = "steelblue4", size = 4) +
    annotate("text", x = 0.25, y = 0.05, label = "NH — Hedged CRC",
              fontface = "bold", colour = "grey40", size = 4) +
    annotate("text", x = 0.75, y = 0.05, label = "NB — Beneficiado",
              fontface = "bold", colour = "darkgreen", size = 4) +
    labs(title = "Figura 4. Mapa de cuadrantes sectoriales",
         subtitle = "Posicionamiento de los sectores CIIU según composición monetaria de ingresos y costos",
         x = "Proporción de costos en USD",
         y = "Proporción de ingresos en USD",
         colour = "Cuadrante") +
    theme_paper() +
    theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggsave("output/figures/paper_fig4_mapa_cuadrantes.png", fig4,
           width = 7, height = 6.5, dpi = 200, bg = "white")
  } else {
    log_msg("ggrepel no instalado — omitiendo Fig 4.", "WARN")
  }
}

# ------ FIG 5 — Trayectorias contrafactuales --------------------
sims_long <- sims %>%
  mutate(scenario_label = case_when(
    scenario == "baseline" ~ "Baseline (sin choque)",
    scenario == "shock_A"  ~ "A — Moderado",
    scenario == "shock_B"  ~ "B — Medio",
    scenario == "shock_C"  ~ "C — Severo",
    TRUE                   ~ scenario
  )) %>%
  select(h, scenario_label,
         `TV (muriendo)` = y_TV_yoy_log,
         `TH (hedged USD)` = y_TH_yoy_log,
         `NH (hedged CRC)` = y_NH_yoy_log,
         `NB (beneficiado)` = y_NB_yoy_log,
         `TPM` = tpm,
         `Inflación` = inflation_yoy) %>%
  pivot_longer(-c(h, scenario_label), names_to = "variable", values_to = "valor")

fig5 <- ggplot(sims_long, aes(x = h, y = valor, colour = scenario_label,
                                linetype = scenario_label)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("Baseline (sin choque)" = "grey40",
                                   "A — Moderado"           = "goldenrod3",
                                   "B — Medio"              = "darkorange3",
                                   "C — Severo"             = "firebrick3")) +
  scale_linetype_manual(values = c("Baseline (sin choque)" = "dashed",
                                     "A — Moderado"           = "solid",
                                     "B — Medio"              = "solid",
                                     "C — Severo"             = "solid")) +
  labs(title = "Figura 5. Trayectorias contrafactuales por cuadrante",
       subtitle = "Simulación mensual del shock de Ormuz, horizonte 18 meses",
       x = "Meses desde el choque", y = NULL) +
  theme_paper()
ggsave("output/figures/paper_fig5_contrafactual_cuadrantes.png", fig5,
       width = 8, height = 8, dpi = 200, bg = "white")

# ------ FIG 6 — Impuesto silencioso revelado --------------------
revealed <- readr::read_csv("output/tables/islmbp_silent_tax_revealed.csv",
                              show_col_types = FALSE)

fig6_df <- revealed %>%
  select(scenario, h, d_squeeze, d_subsidy, d_amplitude) %>%
  pivot_longer(c(d_squeeze, d_subsidy, d_amplitude),
                names_to = "metrica", values_to = "valor") %>%
  mutate(
    scenario_label = case_when(
      scenario == "shock_A" ~ "A — Moderado",
      scenario == "shock_B" ~ "B — Medio",
      scenario == "shock_C" ~ "C — Severo",
      TRUE                  ~ scenario
    ),
    metrica_label = case_when(
      metrica == "d_squeeze"   ~ "Squeeze TV adicional",
      metrica == "d_subsidy"   ~ "Subsidio NB adicional",
      metrica == "d_amplitude" ~ "Amplitud total adicional"
    )
  )

fig6 <- ggplot(fig6_df, aes(x = h, y = valor * 100,
                              colour = scenario_label,
                              fill = scenario_label)) +
  geom_area(alpha = 0.25, position = "identity") +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  facet_wrap(~ metrica_label, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("A — Moderado" = "goldenrod3",
                                   "B — Medio"    = "darkorange3",
                                   "C — Severo"   = "firebrick3")) +
  scale_fill_manual(values = c("A — Moderado" = "goldenrod3",
                                 "B — Medio"    = "darkorange3",
                                 "C — Severo"   = "firebrick3")) +
  labs(title = "Figura 6. El impuesto silencioso revelado bajo Ormuz",
       subtitle = "Apertura adicional de las brechas asimétricas vs. baseline (puntos porcentuales)",
       x = "Meses desde el choque",
       y = "pp adicional") +
  theme_paper()
ggsave("output/figures/paper_fig6_silent_tax_revealed.png", fig6,
       width = 7, height = 7, dpi = 200, bg = "white")

log_msg("Figuras guardadas en output/figures/paper_fig*.png")
log_msg("=== 25 done ===")
