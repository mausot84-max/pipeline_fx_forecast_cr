# ============================================================
# 26_sterilization_test.R — Prueba 3b: canal de esterilización
# ============================================================
# Objetivo: documentar empíricamente si la acumulación de Reservas
# Internacionales Netas (RIN) fue acompañada por una expansión
# proporcional de la base monetaria. Si la base creció menos que las
# reservas convertidas a colones, la diferencia tuvo que ser absorbida
# por algún pasivo del banco central. Esto es la prueba IMPLÍCITA.
#
# Series usadas (todas en features_monthly.rds, ya integradas):
#   rin            — Reservas Internacionales Netas (BCCR cuadro 3044)
#   monetary_base  — Base Monetaria (BCCR cuadro 1363)
#   fx_sell        — Tipo de cambio venta (BCCR cuadro 318) — para
#                    convertir RIN (USD) a CRC y hacerla comparable
#                    con base monetaria (CRC)
#   inflation_yoy  — IPC interanual (BCCR cuadro 25485) — control
#   tpm            — Tasa de política monetaria (BCCR cuadro 3541) — control
#
# Notas sobre alcance:
# - La serie del saldo agregado de Bonos de Estabilización Monetaria
#   (BEM) está disponible localmente en data_raw/bccr/bccr_bem.csv
#   pero NO se incluye en el análisis principal de esta versión.
#   Razón: el calendario operativo de las emisiones de BEM no es
#   conocido con precisión, por lo que la correlación contemporánea
#   mensual entre flow_crc y ΔBEM es ruidosa y no informativa.
#   El stock de BEM puede retomarse en una iteración futura como
#   métrica de cointegración en niveles o con rezagos institucionales
#   explícitos.
# - La métrica reportada es de "esterilización implícita": mide cuánto
#   del flujo de compras netas de divisas (en CRC, purgado de valuación
#   cambiaria) NO se trasladó al crecimiento de la base monetaria.
#
# Outputs:
#   output/tables/paper_tabla8_esterilizacion.csv
#   output/figures/paper_fig7_esterilizacion.png
# ============================================================

log_msg("=== 26: Esterilización implícita (Prueba 3b) ===")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sandwich)
  library(lmtest)
  library(zoo)
})

# ------ 1. Cargar panel mensual ---------------------------------
mn <- readRDS("data_intermediate/features/features_monthly.rds")

required <- c("date", "rin", "monetary_base", "fx_sell", "inflation_yoy", "tpm")
missing <- setdiff(required, names(mn))
if (length(missing) > 0) {
  stop("Series faltantes en features_monthly.rds: ",
       paste(missing, collapse = ", "))
}

log_msg(sprintf("Panel cargado: %d obs, %s a %s",
                nrow(mn), format(min(mn$date)), format(max(mn$date))))

# ------ 2. Construir variables derivadas ------------------------
# Descomposición del cambio mensual en RIN_CRC:
#   Δrin_crc = Δrin_USD * fx_prom  +  rin_USD_{t-1} * Δfx
#              \-- flujo real --/      \-- valuación del stock --/
# La regresión principal usa el FLUJO PURGADO, robusto a movimientos
# del tipo de cambio.
df <- mn %>%
  arrange(date) %>%
  mutate(
    rin_crc      = rin * fx_sell,
    d_rin_crc    = rin_crc - lag(rin_crc),
    fx_prom      = (fx_sell + lag(fx_sell)) / 2,
    d_rin_usd    = rin - lag(rin),
    flow_crc     = d_rin_usd * fx_prom,
    valuation_crc = lag(rin) * (fx_sell - lag(fx_sell)),
    d_base       = monetary_base - lag(monetary_base),
    dlog_rin_crc = log(pmax(rin_crc, 1e-6)) - log(pmax(lag(rin_crc), 1e-6)),
    dlog_base    = log(pmax(monetary_base, 1e-6)) - log(pmax(lag(monetary_base), 1e-6)),
    dlog_fx      = log(pmax(fx_sell, 1e-6)) - log(pmax(lag(fx_sell), 1e-6)),
    dlog_rin_flow = log(pmax(1 + flow_crc / lag(rin_crc), 1e-6)),
    sterilization_ratio_flow = ifelse(
      flow_crc > 0,
      1 - (d_base / flow_crc),
      NA_real_
    )
  ) %>%
  filter(!is.na(d_rin_crc), !is.na(d_base), !is.na(flow_crc))

n_total <- nrow(df)
log_msg(sprintf("Observaciones tras construir cambios mensuales: %d", n_total))

# ------ 3. Estadísticas descriptivas del ratio ------------------
strz_flow <- (function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  list(
    n_episodios          = length(x),
    ratio_mean           = mean(x, na.rm = TRUE),
    ratio_median         = median(x, na.rm = TRUE),
    ratio_q25            = unname(quantile(x, 0.25, na.rm = TRUE)),
    ratio_q75            = unname(quantile(x, 0.75, na.rm = TRUE)),
    pct_esteril_positiva = mean(x > 0, na.rm = TRUE) * 100,
    pct_esteril_alta     = mean(x > 0.5, na.rm = TRUE) * 100
  )
})(df$sterilization_ratio_flow)

log_msg(sprintf("Ratio esterilización implícita (flow_crc>0, n=%d):",
                strz_flow$n_episodios))
log_msg(sprintf("  media=%.3f  mediana=%.3f  IQR=[%.3f, %.3f]",
                strz_flow$ratio_mean, strz_flow$ratio_median,
                strz_flow$ratio_q25, strz_flow$ratio_q75))
log_msg("  Caveat: en magnitud, flow_crc suele ser >> d_base en meses con compras netas,")
log_msg("          por lo que el ratio tiende a 1 mecánicamente. La regresión es más informativa.")

# ------ 4. Correlación contemporánea ----------------------------
cor_levels <- cor(df$flow_crc, df$d_base, use = "complete.obs")
cor_logs   <- cor(df$dlog_rin_flow, df$dlog_base, use = "complete.obs")
log_msg(sprintf("Correlación Δ(Base) vs flow_crc (niveles): %.3f", cor_levels))
log_msg(sprintf("Correlación Δlog(Base) vs Δlog(RIN_flow): %.3f", cor_logs))

# ------ 5. Regresión principal con HAC Newey-West ---------------
# Δlog(Base) = α + β·Δlog(RIN_flow) + γ_1·inflación + γ_2·TPM + ε
# Sin dlog_fx como control: ya está purgado en RIN_flow.
df_reg <- df %>%
  select(dlog_base, dlog_rin_flow, inflation_yoy, tpm) %>%
  filter(complete.cases(.))

fit <- lm(dlog_base ~ dlog_rin_flow + inflation_yoy + tpm, data = df_reg)
ct <- coeftest(fit, vcov = NeweyWest(fit, lag = 4, prewhite = FALSE))

beta_rin <- ct["dlog_rin_flow", "Estimate"]
se_rin   <- ct["dlog_rin_flow", "Std. Error"]
pval_rin <- ct["dlog_rin_flow", "Pr(>|t|)"]
r2_reg   <- summary(fit)$r.squared
n_reg    <- nrow(df_reg)

log_msg(sprintf("Regresión Δlog(Base) ~ Δlog(RIN_flow) + ctrl (n=%d, R2=%.3f):",
                n_reg, r2_reg))
log_msg(sprintf("  β sobre Δlog(RIN_flow) = %.4f (EE=%.4f, p=%.4f)",
                beta_rin, se_rin, pval_rin))
log_msg("Interpretación de β:")
log_msg("  β cercano a 1: base acompaña pleno a reservas (poca esterilización)")
log_msg("  β cercano a 0: base no responde a reservas (fuerte esterilización)")
log_msg("  β intermedio: esterilización parcial")

# ------ 6. Tabla de resultados ----------------------------------
tabla8 <- tibble::tibble(
  metric = c(
    "n_observaciones_totales",
    "n_episodios_flow_crc_positivo",
    "correlacion_dlevels_flow",
    "correlacion_dlogs_flow",
    "ratio_esteril_flow_media",
    "ratio_esteril_flow_mediana",
    "ratio_esteril_flow_q25",
    "ratio_esteril_flow_q75",
    "pct_esteril_flow_positiva",
    "pct_esteril_flow_mayor_50",
    "regresion_n",
    "regresion_R2",
    "regresion_beta",
    "regresion_se",
    "regresion_pvalue"
  ),
  value = c(
    n_total,
    strz_flow$n_episodios,
    round(cor_levels, 4),
    round(cor_logs, 4),
    round(strz_flow$ratio_mean, 4),
    round(strz_flow$ratio_median, 4),
    round(strz_flow$ratio_q25, 4),
    round(strz_flow$ratio_q75, 4),
    round(strz_flow$pct_esteril_positiva, 2),
    round(strz_flow$pct_esteril_alta, 2),
    n_reg,
    round(r2_reg, 4),
    round(beta_rin, 4),
    round(se_rin, 4),
    round(pval_rin, 4)
  ),
  interpretation = c(
    "obs mensuales con cambios disponibles",
    "meses con flujo real de compras > 0",
    "Δ(Base) vs flow_crc niveles, purgado de valuación",
    "Δlog(Base) vs Δlog(RIN flow), purgado",
    "1−(Δbase/flow_crc) promedio — caveat de escala",
    "mediana del ratio (informativo direccional, no nivel)",
    "p25 del ratio",
    "p75 del ratio",
    "% meses con base creciendo MENOS que el flujo real",
    "% con esterilización implícita > 50%",
    "obs en regresión principal",
    "R² del modelo",
    "β: elasticidad base a flujo real — métrica principal",
    "EE HAC Newey-West (bandwidth=4)",
    "p-valor del test t para β"
  )
)

readr::write_csv(tabla8, "output/tables/paper_tabla8_esterilizacion.csv")
log_msg("Tabla guardada: output/tables/paper_tabla8_esterilizacion.csv")

# ------ 7. Figura: dos paneles ----------------------------------
# Panel A: ACUMULADO de flujo real de compras netas vs ACUMULADO de Δbase
# (ambos desde 0 en 2010; la brecha vertical = esterilización acumulada implícita)
df_cum <- df %>%
  arrange(date) %>%
  mutate(
    cum_flow_crc = cumsum(replace_na(flow_crc, 0)),
    cum_d_base   = cumsum(replace_na(d_base, 0))
  ) %>%
  select(date, cum_flow_crc, cum_d_base) %>%
  pivot_longer(-date, names_to = "serie", values_to = "valor") %>%
  mutate(serie = recode(serie,
                        cum_flow_crc = "Compras netas de divisas (acum., CRC, flujo puro)",
                        cum_d_base   = "Aumento acumulado de base monetaria (CRC)"),
         valor_billones = valor / 1e6)

p1 <- ggplot(df_cum, aes(x = date, y = valor_billones, colour = serie)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(
    "Compras netas de divisas (acum., CRC, flujo puro)" = "steelblue4",
    "Aumento acumulado de base monetaria (CRC)"          = "firebrick3"
  )) +
  labs(title = "(a) Flujo acumulado de compras de divisas vs aumento de base monetaria",
       subtitle = "Ambas series desde 0 en enero 2010. La brecha entre la línea azul y la roja\nes esterilización acumulada implícita (compras no monetizadas).",
       x = NULL, y = "Billones de colones",
       colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

# Panel B: correlación móvil 24m con flujo purgado de valuación
roll_cor <- df %>%
  arrange(date) %>%
  mutate(rolling_cor = zoo::rollapplyr(
    cbind(dlog_rin_flow, dlog_base),
    width = 24,
    FUN = function(z) cor(z[, 1], z[, 2], use = "complete.obs"),
    by.column = FALSE, fill = NA
  )) %>%
  select(date, rolling_cor) %>%
  filter(!is.na(rolling_cor))

p2 <- ggplot(roll_cor, aes(x = date, y = rolling_cor)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey70") +
  geom_line(colour = "darkgreen", linewidth = 0.7) +
  scale_y_continuous(limits = c(-1, 1.05), breaks = seq(-1, 1, 0.5)) +
  labs(title = "(b) Correlación móvil 24m entre Δlog(RIN flujo puro) y Δlog(Base)",
       subtitle = "Cerca de 1 = acompañamiento pleno; cerca de 0 = esterilización. Valuación cambiaria excluida.",
       x = NULL, y = "Correlación") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

fig <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))

ggsave("output/figures/paper_fig7_esterilizacion.png", fig,
       width = 8, height = 7.5, dpi = 200, bg = "white")
log_msg("Figura guardada: output/figures/paper_fig7_esterilizacion.png")

log_msg("=== 26 done ===")
