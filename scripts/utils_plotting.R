# ============================================================
# utils_plotting.R — Standardised ggplot functions
# ============================================================

library(ggplot2)
library(scales)
library(dplyr)

theme_fx <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face="bold", size=base_size+2),
          plot.subtitle = element_text(colour="grey40"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(face="bold"))
}

plot_series <- function(df, date_col="date", value_col="value",
                         title="", y_label="", colour="#1f77b4") {
  ggplot(df, aes(.data[[date_col]], .data[[value_col]])) +
    geom_line(colour=colour, linewidth=0.6) +
    labs(title=title, y=y_label, x=NULL) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    theme_fx()
}

plot_fan_chart <- function(df_hist, df_fc, date_col="date",
                            actual_col="fx_level", point_col="point",
                            n_tail=52, title="FX Fan Chart") {
  ht <- tail(df_hist, n_tail)
  ggplot() +
    geom_ribbon(data=df_fc, aes(.data[[date_col]], ymin=lower_95, ymax=upper_95),
                fill="#3182bd", alpha=0.15) +
    geom_ribbon(data=df_fc, aes(.data[[date_col]], ymin=lower_80, ymax=upper_80),
                fill="#3182bd", alpha=0.25) +
    geom_line(data=ht, aes(.data[[date_col]], .data[[actual_col]]),
              colour="black", linewidth=0.7) +
    geom_line(data=df_fc, aes(.data[[date_col]], .data[[point_col]]),
              colour="#e6550d", linewidth=0.7, linetype="dashed") +
    labs(title=title, y="CRC/USD", x=NULL) + theme_fx()
}

plot_regime_probs <- function(df, date_col="date",
                               prob_cols=c("prob_abundance","prob_compression","prob_stress"),
                               labels=c("Abundancia","Compresion","Estres"),
                               colours=c("#2ca02c","#ff7f0e","#d62728"),
                               title="Regime Probabilities") {
  df %>%
    select(all_of(c(date_col, prob_cols))) %>%
    tidyr::pivot_longer(all_of(prob_cols), names_to="regime", values_to="prob") %>%
    mutate(regime = factor(regime, levels=prob_cols, labels=labels)) %>%
    ggplot(aes(.data[[date_col]], prob, fill=regime)) +
    geom_area(alpha=0.7) +
    scale_fill_manual(values=setNames(colours, labels)) +
    scale_y_continuous(labels=percent_format()) +
    labs(title=title, y="Probability", x=NULL, fill="Regime") +
    theme_fx()
}

plot_risk_by_horizon <- function(df, horizon_col="horizon",
                                  down_col="downside_95", up_col="upside_95",
                                  title="Risk by Horizon") {
  df %>%
    tidyr::pivot_longer(all_of(c(down_col, up_col)),
                         names_to="dir", values_to="val") %>%
    mutate(dir = ifelse(dir==down_col, "Downside 95%", "Upside 95%")) %>%
    ggplot(aes(factor(.data[[horizon_col]]), val, fill=dir)) +
    geom_col(position="dodge", alpha=0.8) +
    scale_fill_manual(values=c("Downside 95%"="#d62728","Upside 95%"="#2ca02c")) +
    geom_hline(yintercept=0, colour="grey30") +
    labs(title=title, x="Horizon", y="% Change", fill="") + theme_fx()
}

plot_correlation_heatmap <- function(cor_mat, title = "Correlations",
                                      subtitle = "Correlaciones contemporáneas semanales; no implican causalidad") {
  # Order by hierarchical clustering (1 - |r| as distance)
  if (nrow(cor_mat) > 2) {
    d  <- as.dist(1 - abs(cor_mat))
    hc <- hclust(d, method = "average")
    ord <- hc$order
    cor_mat <- cor_mat[ord, ord]
  }

  # Upper triangle only (set lower to NA so it doesn't render)
  cor_plot <- cor_mat
  cor_plot[lower.tri(cor_plot, diag = FALSE)] <- NA_real_

  as.data.frame(as.table(cor_plot)) %>%
    rename(var1 = Var1, var2 = Var2, r = Freq) %>%
    dplyr::filter(!is.na(r)) %>%
    ggplot(aes(var1, var2, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.2f", r)), size = 2.8, colour = "grey20") +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850",
                          midpoint = 0, limits = c(-1, 1),
                          name = "r") +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_fx() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          panel.grid = element_blank())
}

plot_missingness <- function(df, date_col="date", title="Data Availability") {
  vars <- setdiff(names(df), date_col)
  df %>%
    mutate(across(all_of(vars), ~as.integer(!is.na(.x)))) %>%
    tidyr::pivot_longer(all_of(vars), names_to="variable", values_to="avail") %>%
    ggplot(aes(.data[[date_col]], variable, fill=factor(avail))) +
    geom_tile() +
    scale_fill_manual(values=c("0"="#d62728","1"="#2ca02c"),
                       labels=c("Missing","Available")) +
    labs(title=title, x=NULL, y=NULL, fill="") + theme_fx() +
    theme(axis.text.y=element_text(size=7))
}

save_plot <- function(p, filename, dir="output/figures",
                       width=10, height=6, dpi=300) {
  dir.create(dir, recursive=TRUE, showWarnings=FALSE)
  fp <- file.path(dir, filename)
  ggsave(fp, p, width=width, height=height, dpi=dpi, bg="white")
  message("[PLOT] Saved: ", fp)
}
