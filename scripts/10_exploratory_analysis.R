# ============================================================
# 10_exploratory_analysis.R — Descriptives, correlations, EDA
# ============================================================

log_msg("=== 10: Exploratory analysis ===")
wk <- readRDS("data_intermediate/features/features_weekly.rds")

# Missingness
miss_wk <- missingness_report(wk)
readr::write_csv(miss_wk, "output/tables/missingness_weekly.csv")

# Descriptives
num_vars <- names(wk)[sapply(wk, is.numeric)]
desc <- tibble(
  variable = num_vars,
  n     = sapply(num_vars, function(v) sum(!is.na(wk[[v]]))),
  mean  = sapply(num_vars, function(v) mean(wk[[v]], na.rm=TRUE)),
  sd    = sapply(num_vars, function(v) sd(wk[[v]], na.rm=TRUE)),
  min   = sapply(num_vars, function(v) min(wk[[v]], na.rm=TRUE)),
  median= sapply(num_vars, function(v) median(wk[[v]], na.rm=TRUE)),
  max   = sapply(num_vars, function(v) max(wk[[v]], na.rm=TRUE))
)
readr::write_csv(desc, "output/tables/descriptives_weekly.csv")

# Correlation heatmap of core variables
core <- intersect(c("fx_sell","tpm","vix","dxy","sp500","wti","ust10y",
                      "ust2y","fedfunds","nfci","rin","inflation_yoy",
                      "imae_tc","pressure_index"), names(wk))
if (length(core) >= 3) {
  cm <- cor(wk[, core], use="pairwise.complete.obs")
  readr::write_csv(as.data.frame(cm) %>% tibble::rownames_to_column("var"),
                    "output/tables/correlations_weekly.csv")
  save_plot(plot_correlation_heatmap(cm), "correlation_heatmap_weekly.png",
            width=10, height=9)
}

# Key plots
if ("fx_sell" %in% names(wk))
  save_plot(plot_series(wk, "date","fx_sell", title="CRC/USD Venta", y_label="CRC/USD"),
            "ts_fx_sell.png")
if ("pressure_index" %in% names(wk)) {
  pi_df <- wk %>% dplyr::filter(!is.na(pressure_index))
  last_val  <- tail(pi_df$pressure_index, 1)
  last_date <- tail(pi_df$date, 1)
  min_val   <- min(pi_df$pressure_index, na.rm = TRUE)
  min_date  <- pi_df$date[which.min(pi_df$pressure_index)]

  y_rng <- range(pi_df$pressure_index, na.rm = TRUE)
  y_rng[1] <- y_rng[1] - 0.1
  y_rng[2] <- y_rng[2] + 0.1

  p <- ggplot(pi_df, aes(date, pressure_index)) +
    # Subtle shading of regime zones
    annotate("rect", xmin = min(pi_df$date), xmax = max(pi_df$date) + 60,
             ymin = y_rng[1], ymax = -0.5,
             fill = "#2ca02c", alpha = 0.06) +
    annotate("rect", xmin = min(pi_df$date), xmax = max(pi_df$date) + 60,
             ymin = 0.5, ymax = y_rng[2],
             fill = "#d62728", alpha = 0.06) +
    geom_line(colour = "#1f77b4", linewidth = 0.55) +
    geom_hline(yintercept = c(-0.5, 0, 0.5),
               linetype = c("dashed", "solid", "dashed"),
               colour = "grey45", linewidth = 0.4) +
    # Annotations for thresholds
    annotate("text", x = min(pi_df$date), y = -0.55,
             label = "Umbral abundancia (-0.5)",
             hjust = 0, vjust = 1, size = 3, colour = "grey30") +
    annotate("text", x = min(pi_df$date), y = 0.55,
             label = "Umbral estrés (+0.5)",
             hjust = 0, vjust = 0, size = 3, colour = "grey30") +
    # Mark historical minimum
    annotate("point", x = min_date, y = min_val,
             colour = "#d62728", size = 2.2) +
    annotate("text", x = min_date, y = min_val - 0.08,
             label = sprintf("Mínimo histórico\n%s (%.2f)",
                              format(min_date, "%b %Y"), min_val),
             hjust = 0.5, vjust = 1, size = 2.9, colour = "grey25") +
    # Mark current value
    annotate("point", x = last_date, y = last_val,
             colour = "#1f77b4", size = 2.8) +
    annotate("text", x = last_date, y = last_val + 0.12,
             label = sprintf("Actual: %.3f", last_val),
             hjust = 1, vjust = 0, size = 3.2, fontface = "bold",
             colour = "#1f4e79") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                  expand = expansion(mult = c(0.02, 0.08))) +
    coord_cartesian(ylim = y_rng) +
    labs(title = "Índice compuesto de presión cambiaria",
         subtitle = "Negativo = abundancia, Positivo = estrés. EMA span=8 semanas, 5 componentes.",
         y = "Índice", x = NULL) +
    theme_fx()
  save_plot(p, "ts_pressure_index.png", width = 11, height = 5.5)
}
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
      geom_area(position = "stack", alpha = 0.75) +
      scale_fill_manual(values = c("Abundancia" = "#2ca02c",
                                    "Compresion" = "#ff7f0e",
                                    "Estres"     = "#d62728")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "Frecuencia rolling de régimen",
           subtitle = "Participación de cada estado en ventana móvil de 20 semanas (clasificación discreta suavizada, no probabilidades posteriores)",
           y = "Participación", x = NULL, fill = "Régimen") +
      theme_fx()
    save_plot(p, "regime_probabilities_over_time.png", width = 12, height = 5)
  }
}

log_msg("=== 10 done ===")
