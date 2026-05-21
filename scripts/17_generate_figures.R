# ============================================================
# 17_generate_figures.R — Publication figures (refined)
# ============================================================
# REFINAMIENTOS DE ESTA VERSIÓN:
#   - Fan chart: jerarquía visual mejorada + anotación a 24w
#   - Risk comparison: dos paneles (downside | upside), leyenda simple
#   - Horse race: etiqueta de % mejora vs RW en barra ganadora
#   - Rolling conditional downside: título/subtítulo más precisos
#   - Regime chronology y probabilidades: títulos honestos
# ============================================================

log_msg("=== 17: Figures ===")

wk <- readRDS("data_intermediate/features/features_weekly.rds")

# ==============================================================
# FIG: REGIME CHRONOLOGY
# ==============================================================

if (file.exists("data_intermediate/diagnostics/regime_chronology.csv")) {
  ch <- readr::read_csv("data_intermediate/diagnostics/regime_chronology.csv",
                          show_col_types = FALSE) %>%
    mutate(start_date = as.Date(start_date),
           end_date   = as.Date(end_date))

  rcol <- c("Abundancia" = "#2ca02c", "Compresion" = "#ff7f0e", "Estres" = "#d62728")

  p <- ggplot(ch) +
    geom_rect(aes(xmin = start_date, xmax = end_date,
                   ymin = 0, ymax = 1, fill = regime), alpha = 0.85) +
    scale_fill_manual(values = rcol) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                  expand = c(0.01, 0)) +
    labs(title = "Cronología de regímenes cambiarios",
         subtitle = "Clasificación discreta suavizada con persistencia mínima de 4 semanas",
         x = NULL, y = NULL, fill = "Régimen") +
    theme_fx() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  save_plot(p, "regime_chronology_timeline.png", width = 14, height = 3.2)
  log_msg("Chronology saved.")
}

# ==============================================================
# FIG: REGIME PROBABILITIES (honestly titled)
# ==============================================================

if (all(c("prob_abundance","prob_compression","prob_stress") %in% names(wk))) {
  wk_r <- wk %>% dplyr::filter(!is.na(prob_abundance))
  if (nrow(wk_r) > 0) {
    wk_long <- wk_r %>%
      dplyr::select(date, prob_abundance, prob_compression, prob_stress) %>%
      tidyr::pivot_longer(cols = -date, names_to = "regime", values_to = "share") %>%
      mutate(regime = factor(regime,
        levels = c("prob_abundance","prob_compression","prob_stress"),
        labels = c("Abundancia","Compresion","Estres")))

    p <- ggplot(wk_long, aes(date, share, fill = regime)) +
      geom_area(position = "stack", alpha = 0.78) +
      scale_fill_manual(values = c("Abundancia" = "#2ca02c",
                                    "Compresion" = "#ff7f0e",
                                    "Estres"     = "#d62728")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(title = "Frecuencia rolling de régimen",
           subtitle = "Participación de cada estado en ventana móvil de 20 semanas (clasificación suavizada, no probabilidades posteriores)",
           y = "Participación", x = NULL, fill = "Régimen") +
      theme_fx()
    save_plot(p, "regime_probabilities_over_time.png", width = 12, height = 5)
  }
}

# ==============================================================
# FIG: FAN CHART — jerarquía visual + anotación a 24w
# ==============================================================

if (file.exists("output/tables/fan_chart_data.csv")) {
  fan <- readr::read_csv("output/tables/fan_chart_data.csv",
                           show_col_types = FALSE) %>%
    mutate(date = as.Date(target_date)) %>%
    dplyr::filter(!is.na(point))

  if (nrow(fan) > 0) {
    last_fx   <- tail(wk$fx_sell[!is.na(wk$fx_sell)], 1)
    last_date <- max(wk$date[!is.na(wk$fx_sell)])

    df_hist <- wk %>% dplyr::filter(!is.na(fx_sell)) %>%
      tail(104) %>% dplyr::select(date, fx_level = fx_sell)

    origin_row <- tibble(date = last_date, point = last_fx,
                          lower_80 = last_fx, upper_80 = last_fx,
                          lower_95 = last_fx, upper_95 = last_fx)
    fan_plot <- bind_rows(origin_row, fan %>% dplyr::select(date, point,
                            lower_80, upper_80, lower_95, upper_95))

    # Row for 24w (for annotation)
    ann_row <- fan %>% dplyr::filter(horizon == "24w") %>% dplyr::slice(1)

    p <- ggplot() +
      # Outer band (95%) — lighter
      geom_ribbon(data = fan_plot,
                   aes(date, ymin = lower_95, ymax = upper_95),
                   fill = "#3182bd", alpha = 0.15) +
      # Inner band (80%) — darker
      geom_ribbon(data = fan_plot,
                   aes(date, ymin = lower_80, ymax = upper_80),
                   fill = "#3182bd", alpha = 0.32) +
      # Historical line
      geom_line(data = df_hist, aes(date, fx_level),
                colour = "black", linewidth = 0.7) +
      # Median — thicker, dashed, orange
      geom_line(data = fan_plot, aes(date, point),
                colour = "#e6550d", linewidth = 0.9, linetype = "dashed") +
      # Origin point
      geom_point(data = origin_row, aes(date, point),
                  colour = "black", size = 2) +
      # Horizon markers (vertical dotted lines at 4, 8, 12, 24w)
      geom_vline(data = fan, aes(xintercept = as.numeric(date)),
                  colour = "grey70", linewidth = 0.25, linetype = "dotted") +
      # Annotation at 24w
      { if (nrow(ann_row) > 0)
        list(
          annotate("point", x = ann_row$date, y = ann_row$upper_95,
                   colour = "#3182bd", size = 1.8),
          annotate("point", x = ann_row$date, y = ann_row$lower_95,
                   colour = "#3182bd", size = 1.8),
          annotate("point", x = ann_row$date, y = ann_row$point,
                   colour = "#e6550d", size = 2),
          annotate("text", x = ann_row$date, y = ann_row$upper_95,
                   label = sprintf("  P95: %.0f", ann_row$upper_95),
                   hjust = 0, vjust = 0.5, size = 3.2, colour = "#1f4e79"),
          annotate("text", x = ann_row$date, y = ann_row$point,
                   label = sprintf("  Mediana: %.0f", ann_row$point),
                   hjust = 0, vjust = 0.5, size = 3.2, fontface = "bold",
                   colour = "#a63603"),
          annotate("text", x = ann_row$date, y = ann_row$lower_95,
                   label = sprintf("  P5: %.0f", ann_row$lower_95),
                   hjust = 0, vjust = 0.5, size = 3.2, colour = "#1f4e79")
        ) } +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                    expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "CRC/USD — Fan chart condicional desde el estado actual",
           subtitle = "Banda 95% en tono claro · Banda 80% en tono fuerte · Mediana en naranja punteado\nCuantiles condicionales al estado actual vía quantile regression (horizontes: 1, 4, 8, 12, 24 semanas)",
           y = "CRC/USD", x = NULL) +
      theme_fx()
    save_plot(p, "fan_chart_fx.png", width = 13, height = 6.5)
    log_msg("Fan chart saved.")
  }
}

# ==============================================================
# FIG: RISK COMPARISON — two panels (downside | upside)
# ==============================================================

if (file.exists("output/tables/risk_comparison.csv")) {
  comp <- readr::read_csv("output/tables/risk_comparison.csv",
                            show_col_types = FALSE)

  comp_long <- comp %>%
    tidyr::pivot_longer(cols = -horizon, names_to = "metric", values_to = "val") %>%
    mutate(
      tipo = ifelse(grepl("^uncond", metric), "Histórico incondicional", "Condicional actual"),
      lado = case_when(
        grepl("down", metric) ~ "Downside (95%)",
        grepl("up", metric)   ~ "Upside (95%)",
        TRUE                  ~ "Mediana"
      )
    ) %>%
    dplyr::filter(lado != "Mediana", !is.na(val)) %>%
    mutate(val_pct = val * 100)

  h_order <- c("1w","4w","8w","12w","24w")
  comp_long$horizon <- factor(comp_long$horizon, levels = h_order)
  comp_long$tipo    <- factor(comp_long$tipo,
                               levels = c("Histórico incondicional",
                                          "Condicional actual"))

  p <- ggplot(comp_long,
              aes(horizon, val_pct, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.75),
             width = 0.65, alpha = 0.88) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%+.1f%%", val_pct),
                   y = val_pct + ifelse(val_pct >= 0, 0.4, -0.4)),
               position = position_dodge(width = 0.75),
               size = 2.8, colour = "grey20") +
    scale_fill_manual(values = c("Histórico incondicional" = "#bdbdbd",
                                  "Condicional actual"      = "#2166ac")) +
    facet_wrap(~ lado, ncol = 2, scales = "free_y") +
    labs(title = "Riesgo por horizonte — histórico vs condicional al estado actual",
         subtitle = "Downside: máxima apreciación plausible del colón · Upside: máxima depreciación plausible · Intervalos al 95%",
         x = "Horizonte", y = "Retorno del CRC/USD (%)", fill = NULL) +
    theme_fx() +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold", size = 11),
          panel.spacing.x = unit(1.2, "lines"))
  save_plot(p, "risk_comparison_by_horizon.png", width = 12, height = 6)
  log_msg("Risk comparison chart saved.")
}

# ==============================================================
# FIG: UNCONDITIONAL RISK (kept as reference)
# ==============================================================

if (file.exists("output/tables/risk_unconditional.csv")) {
  ru <- readr::read_csv("output/tables/risk_unconditional.csv",
                          show_col_types = FALSE)
  ru$horizon <- factor(ru$horizon, levels = c("1w","4w","8w","12w","24w"))
  p <- plot_risk_by_horizon(ru, "horizon", "downside_95", "upside_95",
                             title = "Riesgo incondicional por horizonte (histórico)")
  save_plot(p, "risk_unconditional_by_horizon.png")
}

# ==============================================================
# FIG: ROLLING CONDITIONAL DOWNSIDE 4W — título más preciso
# ==============================================================

if (file.exists("output/tables/rolling_conditional_downside_4w.csv")) {
  cd <- readr::read_csv("output/tables/rolling_conditional_downside_4w.csv",
                          show_col_types = FALSE)
  if (nrow(cd) > 10) {
    p <- ggplot(cd, aes(x = as.Date(origin_date))) +
      geom_line(aes(y = actual, colour = "Retorno realizado (4w)"),
                linewidth = 0.4) +
      geom_line(aes(y = cond_downside_05,
                     colour = "P5 condicional del retorno forward a 4w"),
                linewidth = 0.55) +
      scale_colour_manual(values = c("Retorno realizado (4w)" = "grey55",
                                      "P5 condicional del retorno forward a 4w" = "#d62728")) +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "Estimación rolling del percentil 5 condicional — horizonte 4 semanas",
           subtitle = "Quantile regression out-of-sample, retornos forward a 4 semanas del CRC/USD",
           y = "Retorno forward 4w", x = NULL, colour = "") +
      theme_fx() +
      theme(legend.position = "top")
    save_plot(p, "rolling_conditional_downside_4w.png", width = 12, height = 5)
  }
}

# ==============================================================
# FIG: HORSE RACE — label with % improvement vs RW
# ==============================================================

if (file.exists("output/tables/model_horse_race.csv")) {
  hr <- readr::read_csv("output/tables/model_horse_race.csv",
                          show_col_types = FALSE)
  for (h in intersect(HORIZONS_W, unique(hr$horizon))) {
    hr_h <- hr %>% dplyr::filter(horizon == h) %>%
      mutate(label_best = ifelse(rank_rmse == 1 & rmse_vs_rw < -3,
                                   sprintf("%+.1f%% vs RW", rmse_vs_rw),
                                   ""))
    if (nrow(hr_h) < 2) next

    # Title reflects magnitude
    magn_best <- hr_h$improvement_magnitude[hr_h$rank_rmse == 1][1]
    magn_txt <- switch(magn_best %||% "",
      "strong"   = " — mejora material",
      "moderate" = " — mejora moderada",
      "marginal" = " — mejora marginal",
      "worse"    = " — sin mejora",
      "")

    p <- hr_h %>%
      mutate(model = reorder(model, -rmse)) %>%
      ggplot(aes(rmse, model, fill = improvement_magnitude)) +
      geom_col(alpha = 0.88) +
      geom_text(aes(label = label_best), hjust = -0.1,
                size = 3.3, fontface = "bold", colour = "grey15") +
      scale_fill_manual(values = c("strong"   = "#2ca02c",
                                    "moderate" = "#98df8a",
                                    "marginal" = "#aec7e8",
                                    "worse"    = "#ff9896"),
                         na.value = "#3182bd",
                         name = "Magnitud vs RW",
                         labels = c("strong" = "material",
                                     "moderate" = "moderada",
                                     "marginal" = "marginal",
                                     "worse" = "peor")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.22))) +
      labs(title = paste0("Horse race — horizonte ", h, " semanas", magn_txt),
           subtitle = "RMSE sobre retornos forward; barra verde = mejor modelo con mejora ≥3% vs Random Walk",
           x = "RMSE", y = NULL) +
      theme_fx() +
      theme(legend.position = "bottom")
    save_plot(p, paste0("horse_race_h", h, "w.png"), width = 9, height = 5)
  }
}

# ==============================================================
# FIG: BACKTEST RW vs ARIMA (kept)
# ==============================================================

if (file.exists("models/bt_baseline_weekly.rds")) {
  bt <- readRDS("models/bt_baseline_weekly.rds")
  bt_4w <- bt %>% dplyr::filter(horizon == 4, model %in% c("rw","arima"))
  if (nrow(bt_4w) > 10) {
    p <- ggplot(bt_4w, aes(target_date)) +
      geom_line(aes(y = actual_level, colour = "Observado"), linewidth = 0.5) +
      geom_point(aes(y = predicted_level, colour = model),
                  size = 0.8, alpha = 0.6) +
      scale_colour_manual(values = c("Observado" = "black",
                                      "rw" = "#e6550d", "arima" = "#3182bd")) +
      labs(title = "Pronóstico a 4 semanas — Random Walk vs ARIMA",
           y = "CRC/USD", x = NULL, colour = "") + theme_fx()
    save_plot(p, "backtest_rw_arima_4w.png", width = 12, height = 5)
  }
}

# FX level
if ("fx_sell" %in% names(wk))
  save_plot(plot_series(wk, "date","fx_sell",
            title = "CRC/USD (tipo de cambio venta)", y_label = "CRC/USD"),
            "ts_fx_sell.png")

log_msg(paste("Figures:", length(list.files("output/figures", "\\.png$"))))
log_msg("=== 17 done ===")
