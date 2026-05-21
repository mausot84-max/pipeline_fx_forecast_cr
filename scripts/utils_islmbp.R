# ============================================================
# utils_islmbp.R — IS-LM-BP de cuatro cuadrantes: estimación
# y simulación contrafactual del impuesto silencioso
# ============================================================
# Add-on mensual al pipeline_fx_forecast_cr.
# Lee features_monthly.rds y produce un motor estructural
# calibrado para simulación contrafactual del choque externo.
#
# ----------------------------------------------------------------
# ARQUITECTURA: CUATRO CUADRANTES SECTORIALES
# ----------------------------------------------------------------
# La economía costarricense se descompone según la composición
# monetaria de ingresos y costos sectoriales:
#
#                       Ingresos colones    Ingresos dólares
#                     ┌─────────────────┬─────────────────┐
#   Costos colones    │  N-hedged (NH)  │ T-vulnerable(TV)│
#                     │  hedge natural  │  "muriendo"     │
#                     ├─────────────────┼─────────────────┤
#   Costos dólares    │ N-beneficiado   │ T-hedged (TH)   │
#                     │ (NB) subsidiado │ hedge natural   │
#                     └─────────────────┴─────────────────┘
#
# El impuesto silencioso opera transfiriendo márgenes del
# cuadrante TV (que carga el costo de la apreciación) hacia
# el cuadrante NB (que es subsidiado por la apreciación), con
# los cuadrantes hedged (TH, NH) en posición neutral.
#
# ----------------------------------------------------------------
# MAPEO IS-LM-BP CANÓNICO ↔ MOTOR DE CUATRO CUADRANTES
# ----------------------------------------------------------------
#
#   Curva IS  →  desagregada en cuatro: IS_TV + IS_TH + IS_NH + IS_NB
#                Cada cuadrante tiene regresores propios consistentes
#                con su geometría monetaria (qué afecta a qué).
#   Curva LM  →  implícita en la regla de Taylor (TPM como instrumento)
#   Curva BP  →  identidad de FX + ITCER (módulo BP explícito pendiente)
#   Phillips  →  cierre nominal con pass-through cambiario
#
# ----------------------------------------------------------------
# ECUACIONES (5 reducidas + identidades)
# ----------------------------------------------------------------
#
#   Eq1.  IS sector TV:  y_TV_yoy = f(us_ip_yoy, itcer_yoy, tot_yoy, lag)
#          [β_itcer NEGATIVO fuerte: la trampa terminal]
#   Eq2.  IS sector TH:  y_TH_yoy = f(us_ip_yoy, tot_yoy, lag)
#          [β_itcer omitido: hedge natural neutraliza]
#   Eq3.  IS sector NH:  y_NH_yoy = f(r_real, cr_col_yoy, lag)
#          [β_itcer omitido: poca transabilidad]
#   Eq4.  IS sector NB:  y_NB_yoy = f(r_real, itcer_yoy, lag)
#          [β_itcer POSITIVO: insumos importados abaratados]
#   Eq5.  Taylor:        tpm = f(tpm_lag1, inflation_yoy, output_gap [, q_gap])
#   Eq6.  Phillips:      dinf = f(dinf_lag, output_gap, dfx, doil)
#   Eq7.  FX nominal:    dfx = f(rate_diff_cr_us, tot_yoy, drisk)
#
# ----------------------------------------------------------------
# MÉTRICAS DEL IMPUESTO SILENCIOSO
# ----------------------------------------------------------------
#
#   tax_squeeze_TV  = y_TH_yoy - y_TV_yoy
#       Brecha entre los dos cuadrantes transables: cuánto peor le
#       va al TV vs su par hedged.  Captura el costo de no tener
#       hedge natural en el bloque exportador.
#
#   tax_subsidy_NB  = y_NB_yoy - y_NH_yoy
#       Brecha entre los dos cuadrantes domésticos: cuánto mejor
#       le va al NB vs su par hedged.  Captura el subsidio implícito
#       que recibe el cuadrante con costos USD.
#
#   silent_tax_amplitude = tax_squeeze_TV + tax_subsidy_NB
#       Amplitud total de la divergencia que el régimen monetario
#       abre entre los cuadrantes asimétricos, controlando por las
#       posiciones hedged.  Es la métrica integrada del paper.
#
# Estimación: OLS con errores HAC Newey-West (bw=4).
# Sin GMM, sin IV.  Hostile-economist standard.
# ============================================================

library(dplyr)
library(sandwich)
library(lmtest)
library(zoo)

# ==============================================================
# 1.  INFERENCIA HAC
# ==============================================================

estimate_ols_hac <- function(formula, data, bw = 4L) {
  fit <- lm(formula, data = data, na.action = na.omit)
  vc  <- sandwich::NeweyWest(fit, lag = bw, prewhite = FALSE)
  ct  <- lmtest::coeftest(fit, vcov. = vc)

  coefs <- tibble::tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std_error = ct[, "Std. Error"],
    statistic = ct[, "t value"],
    p_value   = ct[, "Pr(>|t|)"]
  )

  list(
    fit       = fit,
    vcov      = vc,
    coefs     = coefs,
    n         = stats::nobs(fit),
    r_squared = summary(fit)$r.squared,
    formula   = format(formula),
    bw        = bw
  )
}

# ==============================================================
# 2.  FILTRO HP
# ==============================================================

hp_filter <- function(y, lambda = 14400) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 8) return(list(trend = rep(NA_real_, n), cycle = rep(NA_real_, n)))

  ok <- !is.na(y)
  if (sum(ok) < 8) return(list(trend = rep(NA_real_, n), cycle = rep(NA_real_, n)))

  y_clean <- y[ok]
  m  <- length(y_clean)
  Im <- diag(m)
  D  <- diff(Im, lag = 1, differences = 2)
  trend_clean <- solve(Im + lambda * crossprod(D), y_clean)

  trend <- rep(NA_real_, n)
  trend[ok] <- as.numeric(trend_clean)
  list(trend = trend, cycle = y - trend)
}

# ==============================================================
# 3.  MATRIZ SECTORIAL DEL CUADRANTE
# ==============================================================
#
# Cada sector CIIU se caracteriza por dos shares:
#   income_share_usd_prior: proporción a priori de ingresos en USD
#   cost_share_usd_prior:   proporción a priori de costos en USD
#
# Los valores a priori son una calibración basada en juicio
# económico documentado.  Cuando hay data del cuadro 144 BCCR
# (crédito por actividad × moneda) y del cuadro 82 (exportaciones
# por actividad), los shares observados sustituyen a los a priori.
#
# El cuadrante se asigna según:
#   TV: income > 0.5 & cost <= 0.5   (la trampa terminal)
#   TH: income > 0.5 & cost > 0.5    (hedge USD-USD)
#   NH: income <= 0.5 & cost <= 0.5  (hedge CRC-CRC)
#   NB: income <= 0.5 & cost > 0.5   (beneficiado)
#
# Manufactura (C) y financiero (K) son intra-heterogéneos en CR
# (ZF vs régimen definitivo; intermediación dual moneda) — se
# tratan con bandera `mixed = TRUE` y se excluyen del agregado
# de cuadrante por default, con documentación explícita.

SECTOR_MATRIX <- tibble::tribble(
  ~var_name,      ~ciiu, ~income_share_usd_prior, ~cost_share_usd_prior, ~mixed, ~justificacion_prior,
  "imae_agro",    "A",   0.85,                    0.30,                   FALSE,  "Banano/cafe/pina: exporta USD, costos planilla+quimicos en CRC. Trampa terminal clasica.",
  "imae_manuf",   "C",   0.50,                    0.50,                   TRUE,   "Mitad ZF (TH puro) mitad regimen definitivo (NH/NB). Excluida del agregado por heterogeneidad.",
  "imae_const",   "F",   0.55,                    0.25,                   FALSE,  "Preventa USD en vertical residencial; costos planilla y materiales locales en CRC. TV con dinamica de colonizacion observable.",
  "imae_comer",   "G",   0.10,                    0.60,                   FALSE,  "Comercio importador: vende en CRC localmente, costos USD (mercancia importada). Cuadrante subsidiado por apreciacion.",
  "imae_transp",  "H",   0.30,                    0.40,                   FALSE,  "Logistica mixta: parcial cabotaje internacional (USD), parcial domestico (CRC). NH leve.",
  "imae_aloj",    "I",   0.75,                    0.25,                   FALSE,  "Turismo cobra USD por habitacion pero opera en CRC (planilla, alimentos). TV con dinamica de premiumizacion (tarifas USD al alza).",
  "imae_infocom", "J",   0.85,                    0.70,                   FALSE,  "TIC exportador: clientes externos USD, insumos importados (SaaS, hardware). TH.",
  "imae_fin",     "K",   0.50,                    0.50,                   TRUE,   "Intermediacion dual moneda. Tratamiento mixed; excluida del agregado de cuadrante.",
  "imae_prof",    "M-N", 0.80,                    0.65,                   FALSE,  "Servicios profesionales exportables: clientes externos, parte operacion local. TH."
)

assign_quadrant <- function(income_share, cost_share, threshold = 0.5) {
  dplyr::case_when(
    income_share  > threshold & cost_share <= threshold ~ "TV",
    income_share  > threshold & cost_share  > threshold ~ "TH",
    income_share <= threshold & cost_share <= threshold ~ "NH",
    income_share <= threshold & cost_share  > threshold ~ "NB",
    TRUE                                                  ~ NA_character_
  )
}

# ==============================================================
# 4.  CONSTRUCCIÓN EMPÍRICA DE SHARES (cuando hay data BCCR)
# ==============================================================

build_cost_share_usd_from_credit <- function(df, sector_var) {
  # Para un sector dado, computa cost_share_usd_obs como ratio
  # del saldo de credito en USD sobre el credito total del sector.
  # Asume nombres de columnas: `cartera_{sector}_usd` y `cartera_{sector}_crc`.
  # Si alguna no existe, retorna NA_real_.
  usd_col <- paste0("cartera_", sector_var, "_usd")
  crc_col <- paste0("cartera_", sector_var, "_crc")
  if (!all(c(usd_col, crc_col) %in% names(df))) return(rep(NA_real_, nrow(df)))
  usd <- df[[usd_col]]; crc <- df[[crc_col]]
  total <- usd + crc
  ifelse(total > 0, usd / total, NA_real_)
}

build_income_share_usd_from_exports <- function(df, sector_var,
                                                  imae_var = NULL) {
  # Proxy: exportaciones del sector / produccion del sector.
  # Si no hay produccion sectorial, usa exportaciones / IMAE como
  # aproximacion (ambos en indices comparables tras normalizar).
  exp_col <- paste0("exports_fob_", sector_var)
  if (!(exp_col %in% names(df))) return(rep(NA_real_, nrow(df)))
  prod_col <- if (!is.null(imae_var)) imae_var else paste0("imae_", sector_var)
  if (!(prod_col %in% names(df))) return(rep(NA_real_, nrow(df)))
  # Normalizar ambos a base 100 en la primera obs valida
  e <- df[[exp_col]]; p <- df[[prod_col]]
  ok <- !is.na(e) & !is.na(p)
  if (sum(ok) < 12) return(rep(NA_real_, nrow(df)))
  ratio <- e / p
  # Suavizar con MA 12m para evitar volatilidad mensual de exportaciones
  zoo::rollapply(ratio, width = 12, FUN = mean, na.rm = TRUE,
                  fill = NA, align = "right")
}

resolve_sector_shares <- function(df, sector_matrix = SECTOR_MATRIX) {
  # Para cada sector en la matriz, decide income_share_usd y
  # cost_share_usd: empirico si esta disponible, prior si no.
  # Retorna sector_matrix enriquecida con columnas `_obs` y `_used`.
  m <- sector_matrix
  m$income_share_usd_obs <- NA_real_
  m$cost_share_usd_obs   <- NA_real_

  for (i in seq_len(nrow(m))) {
    sec_short <- sub("^imae_", "", m$var_name[i])
    cs_obs <- build_cost_share_usd_from_credit(df, sec_short)
    is_obs <- build_income_share_usd_from_exports(df, sec_short,
                                                    imae_var = m$var_name[i])
    if (any(!is.na(cs_obs))) {
      m$cost_share_usd_obs[i] <- mean(tail(cs_obs[!is.na(cs_obs)], 12),
                                       na.rm = TRUE)
    }
    if (any(!is.na(is_obs))) {
      m$income_share_usd_obs[i] <- mean(tail(is_obs[!is.na(is_obs)], 12),
                                         na.rm = TRUE)
    }
  }

  m$income_share_usd_used <- dplyr::coalesce(m$income_share_usd_obs,
                                              m$income_share_usd_prior)
  m$cost_share_usd_used   <- dplyr::coalesce(m$cost_share_usd_obs,
                                              m$cost_share_usd_prior)
  m$quadrant_assigned <- assign_quadrant(m$income_share_usd_used,
                                          m$cost_share_usd_used)
  m$source_used <- ifelse(!is.na(m$cost_share_usd_obs) &
                            !is.na(m$income_share_usd_obs),
                           "empirical", "prior")
  m
}

# ==============================================================
# 5.  CONSTRUCCIÓN DE ÍNDICES POR CUADRANTE
# ==============================================================

build_quadrant_indices <- function(df, sector_matrix = SECTOR_MATRIX,
                                    exclude_mixed = TRUE) {
  # Resuelve shares (empirico vs prior) y agrega los IMAE por
  # cuadrante con promedio geometrico simple (log-aditivo).
  # Retorna df con cuatro nuevas columnas: y_TV, y_TH, y_NH, y_NB.

  m <- resolve_sector_shares(df, sector_matrix)

  if (exclude_mixed) {
    m_active <- m %>% dplyr::filter(mixed == FALSE,
                                     !is.na(quadrant_assigned),
                                     var_name %in% names(df))
  } else {
    m_active <- m %>% dplyr::filter(!is.na(quadrant_assigned),
                                     var_name %in% names(df))
  }

  available <- m_active$var_name
  missing_  <- setdiff(SECTOR_MATRIX$var_name, c(available,
                                                   m$var_name[m$mixed]))
  if (length(missing_) > 0)
    message("[ISLMBP] Componentes IMAE no disponibles en el panel: ",
            paste(missing_, collapse = ", "))

  for (q in c("TV","TH","NH","NB")) {
    comps <- m_active$var_name[m_active$quadrant_assigned == q]
    if (length(comps) == 0) {
      df[[paste0("y_", q)]] <- NA_real_
      next
    }
    # Promedio geometrico simple dentro del cuadrante
    w <- rep(1 / length(comps), length(comps))
    log_q <- rowSums(sapply(comps, function(v)
      w[which(comps == v)] * log(pmax(df[[v]], 1e-6))))
    df[[paste0("y_", q)]] <- log_q
  }

  attr(df, "quadrant_mapping") <- m_active %>%
    dplyr::select(var_name, ciiu, income_share_usd_used,
                  cost_share_usd_used, quadrant_assigned, source_used,
                  justificacion_prior)
  attr(df, "full_sector_matrix") <- m
  df
}

# ==============================================================
# 6.  TRANSFORMACIONES MENSUALES
# ==============================================================

add_log_yoy_m <- function(df, var, periods = 12) {
  col <- paste0(var, "_yoy_log")
  vals <- df[[var]]
  n <- length(vals)
  if (n <= periods) { df[[col]] <- NA_real_; return(df) }
  df[[col]] <- c(rep(NA_real_, periods),
                 vals[(periods + 1):n] - vals[1:(n - periods)])
  df
}

add_diff_m <- function(df, var, n = 1) {
  col <- paste0(var, "_d", n)
  df[[col]] <- c(rep(NA_real_, n), diff(df[[var]], differences = n))
  df
}

add_lag_m <- function(df, var, n = 1) {
  col <- paste0(var, "_lag", n)
  df[[col]] <- dplyr::lag(df[[var]], n = n)
  df
}

# ==============================================================
# 7.  MÉTRICAS DEL IMPUESTO SILENCIOSO
# ==============================================================

compute_silent_tax_metrics <- function(df) {
  # Requiere las cuatro series y_{TV,TH,NH,NB} construidas.
  # Produce las tres metricas del paper.
  for (q in c("TV","TH","NH","NB")) {
    col <- paste0("y_", q)
    if (!(col %in% names(df))) {
      message("[ISLMBP] Faltante: ", col, ". Imposible calcular metricas.")
      df[[paste0(col, "_yoy_log")]] <- NA_real_
      next
    }
    df <- add_log_yoy_m(df, col, 12)
  }

  df$tax_squeeze_TV       <- df$y_TH_yoy_log - df$y_TV_yoy_log
  df$tax_subsidy_NB       <- df$y_NB_yoy_log - df$y_NH_yoy_log
  df$silent_tax_amplitude <- df$tax_squeeze_TV + df$tax_subsidy_NB
  df
}

# ==============================================================
# 8.  ESTIMADORES DE ECUACIÓN — CUATRO IS SECTORIALES
# ==============================================================

estimate_IS_TV <- function(panel) {
  # IS sector TV (la trampa terminal):
  # y_TV_yoy_log = b0 + b1 us_ip_yoy_log + b2 itcer_yoy_log
  #               + b3 tot_yoy_log + b4 y_TV_yoy_log_lag1
  # Signo esperado: b1>0 (demanda externa), b2<0 fuerte (apreciacion
  # asfixia margenes), b3 ambiguo (depende del producto), b4 in (0,1).
  fml <- y_TV_yoy_log ~ us_ip_yoy_log + itcer_yoy_log + tot_yoy_log +
                        y_TV_yoy_log_lag1
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_IS_TH <- function(panel) {
  # IS sector TH (hedged USD-USD):
  # y_TH_yoy_log = b0 + b1 us_ip_yoy_log + b2 tot_yoy_log + b3 y_TH_yoy_log_lag1
  # ITCER omitido por hipotesis: el hedge natural neutraliza el efecto
  # de la apreciacion (insumos y ventas se mueven juntos en USD).
  fml <- y_TH_yoy_log ~ us_ip_yoy_log + tot_yoy_log + y_TH_yoy_log_lag1
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_IS_NH <- function(panel) {
  # IS sector NH (hedged CRC-CRC):
  # y_NH_yoy_log = b0 + b1 r_real + b2 cr_col_yoy_log + b3 y_NH_yoy_log_lag1
  # ITCER omitido: poca transabilidad neutraliza el canal.
  # b1<0 (TPM contrae), b2>0 (credito alimenta), b3 in (0,1).
  fml <- y_NH_yoy_log ~ r_real + cr_col_yoy_log + y_NH_yoy_log_lag1
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_IS_NB <- function(panel) {
  # IS sector NB (beneficiado):
  # y_NB_yoy_log = b0 + b1 r_real + b2 itcer_yoy_log + b3 y_NB_yoy_log_lag1
  # b1<0 (TPM contrae demanda interna), b2>0 (apreciacion abarata
  # insumos importados), b3 in (0,1).
  fml <- y_NB_yoy_log ~ r_real + itcer_yoy_log + y_NB_yoy_log_lag1
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_taylor <- function(panel, include_q_gap = FALSE) {
  if (include_q_gap) {
    fml <- tpm ~ tpm_lag1 + inflation_yoy + output_gap + q_gap
  } else {
    fml <- tpm ~ tpm_lag1 + inflation_yoy + output_gap
  }
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_phillips <- function(panel) {
  fml <- inflation_yoy_d1 ~ inflation_yoy_d1_lag1 + output_gap +
                             fx_sell_yoy_log + wti_yoy_log
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_fx <- function(panel) {
  fml <- fx_sell_yoy_log ~ rate_diff_cr_us + tot_yoy_log + vix_d1
  estimate_ols_hac(fml, panel, bw = 4L)
}

estimate_all <- function(panel, include_q_gap = FALSE) {
  list(
    IS_TV    = estimate_IS_TV(panel),
    IS_TH    = estimate_IS_TH(panel),
    IS_NH    = estimate_IS_NH(panel),
    IS_NB    = estimate_IS_NB(panel),
    taylor   = estimate_taylor(panel, include_q_gap = include_q_gap),
    phillips = estimate_phillips(panel),
    fx       = estimate_fx(panel)
  )
}

# ==============================================================
# 9.  TABLA DE COEFICIENTES PARA REPORTE
# ==============================================================

format_coefs_table <- function(eq_list) {
  dplyr::bind_rows(lapply(names(eq_list), function(eq) {
    co <- eq_list[[eq]]$coefs
    co$equation <- eq
    co$n        <- eq_list[[eq]]$n
    co$r2       <- round(eq_list[[eq]]$r_squared, 3)
    co
  })) %>%
    dplyr::mutate(
      stars = dplyr::case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE           ~ ""
      ),
      estimate  = round(estimate, 4),
      std_error = round(std_error, 4),
      statistic = round(statistic, 2),
      p_value   = round(p_value, 4)
    ) %>%
    dplyr::select(equation, term, estimate, std_error, statistic,
                  p_value, stars, n, r2)
}

# ==============================================================
# 10. HELPER: EXTRAER COEFICIENTE PUNTUAL
# ==============================================================

cf_of <- function(eq_obj, term, default = 0) {
  if (is.null(eq_obj)) return(default)
  v <- eq_obj$coefs$estimate[eq_obj$coefs$term == term]
  if (length(v) == 0) default else v
}

# ==============================================================
# 11. SIMULACIÓN CONTRAFACTUAL — CUATRO CUADRANTES
# ==============================================================
#
# Propaga las 7 ecuaciones (4 IS + Taylor + Phillips + FX) hacia
# adelante.  Variables endogenas: y_TV, y_TH, y_NH, y_NB, tpm,
# inflation_yoy, fx_sell_yoy_log, itcer_yoy_log, r_real,
# output_gap (sintetico), cr_col_yoy_log, q_gap.

simulate_counterfactual <- function(eq_list, initial_state,
                                     exog_paths, horizon = 18L,
                                     pib_weights = NULL) {

  need_init <- c("y_TV_yoy_log","y_TH_yoy_log","y_NH_yoy_log","y_NB_yoy_log",
                 "tpm","inflation_yoy","inflation_yoy_d1",
                 "fx_sell_yoy_log","itcer_yoy_log",
                 "r_real","output_gap","cr_col_yoy_log","q_gap")
  miss <- setdiff(need_init, names(initial_state))
  if (length(miss) > 0)
    stop("[ISLMBP] initial_state incompleto.  Faltan: ",
         paste(miss, collapse = ", "))

  need_exog <- c("h","us_ip_yoy_log","tot_yoy_log","wti_yoy_log",
                 "vix_d1","fedfunds","ipc_ext_yoy_log")
  miss <- setdiff(need_exog, names(exog_paths))
  if (length(miss) > 0)
    stop("[ISLMBP] exog_paths incompleto.  Faltan: ",
         paste(miss, collapse = ", "))
  if (nrow(exog_paths) < horizon)
    stop("[ISLMBP] exog_paths tiene menos filas que horizon.")

  # Pesos PIB por cuadrante (placeholder; reemplazar con cuentas
  # nacionales reales si se obtienen).  Suma = 1.
  if (is.null(pib_weights)) {
    pib_weights <- c(TV = 0.20, TH = 0.30, NH = 0.30, NB = 0.20)
  }
  stopifnot(abs(sum(pib_weights) - 1) < 0.01)

  H <- horizon
  trj <- tibble::tibble(
    h                    = 0:H,
    y_TV_yoy_log         = NA_real_,
    y_TH_yoy_log         = NA_real_,
    y_NH_yoy_log         = NA_real_,
    y_NB_yoy_log         = NA_real_,
    tax_squeeze_TV       = NA_real_,
    tax_subsidy_NB       = NA_real_,
    silent_tax_amplitude = NA_real_,
    tpm                  = NA_real_,
    inflation_yoy        = NA_real_,
    inflation_yoy_d1     = NA_real_,
    fx_sell_yoy_log      = NA_real_,
    itcer_yoy_log        = NA_real_,
    r_real               = NA_real_,
    output_gap           = NA_real_,
    cr_col_yoy_log       = NA_real_,
    q_gap                = NA_real_,
    us_ip_yoy_log        = NA_real_,
    tot_yoy_log          = NA_real_,
    wti_yoy_log          = NA_real_,
    vix_d1               = NA_real_,
    fedfunds             = NA_real_,
    ipc_ext_yoy_log      = NA_real_,
    rate_diff_cr_us      = NA_real_
  )

  # Período 0: copiar estado inicial
  for (v in need_init) trj[[v]][1] <- initial_state[[v]]
  trj$tax_squeeze_TV[1]       <- trj$y_TH_yoy_log[1] - trj$y_TV_yoy_log[1]
  trj$tax_subsidy_NB[1]       <- trj$y_NB_yoy_log[1] - trj$y_NH_yoy_log[1]
  trj$silent_tax_amplitude[1] <- trj$tax_squeeze_TV[1] + trj$tax_subsidy_NB[1]
  trj$us_ip_yoy_log[1]        <- initial_state[["us_ip_yoy_log"]] %||% 0
  trj$fedfunds[1]             <- initial_state[["fedfunds"]] %||% 0
  trj$rate_diff_cr_us[1]      <- trj$tpm[1] - trj$fedfunds[1]

  for (t in 1:H) {
    idx <- t + 1
    prv <- t

    # 11.1  Exógenas del path
    trj$us_ip_yoy_log[idx]   <- exog_paths$us_ip_yoy_log[t]
    trj$tot_yoy_log[idx]     <- exog_paths$tot_yoy_log[t]
    trj$wti_yoy_log[idx]     <- exog_paths$wti_yoy_log[t]
    trj$vix_d1[idx]          <- exog_paths$vix_d1[t]
    trj$fedfunds[idx]        <- exog_paths$fedfunds[t]
    trj$ipc_ext_yoy_log[idx] <- exog_paths$ipc_ext_yoy_log[t]

    # 11.2  FX nominal (i.a. log)
    fx_b0 <- cf_of(eq_list$fx, "(Intercept)")
    fx_b1 <- cf_of(eq_list$fx, "rate_diff_cr_us")
    fx_b2 <- cf_of(eq_list$fx, "tot_yoy_log")
    fx_b3 <- cf_of(eq_list$fx, "vix_d1")
    trj$rate_diff_cr_us[idx] <- trj$tpm[prv] - trj$fedfunds[idx]
    trj$fx_sell_yoy_log[idx] <- fx_b0 +
      fx_b1 * trj$rate_diff_cr_us[idx] +
      fx_b2 * trj$tot_yoy_log[idx] +
      fx_b3 * trj$vix_d1[idx]

    # 11.3  ITCER por identidad: dq = de + dp* - dp
    trj$itcer_yoy_log[idx] <- trj$fx_sell_yoy_log[idx] +
                              trj$ipc_ext_yoy_log[idx] -
                              trj$inflation_yoy[prv]

    # 11.4  Phillips
    ph_b0 <- cf_of(eq_list$phillips, "(Intercept)")
    ph_b1 <- cf_of(eq_list$phillips, "inflation_yoy_d1_lag1")
    ph_b2 <- cf_of(eq_list$phillips, "output_gap")
    ph_b3 <- cf_of(eq_list$phillips, "fx_sell_yoy_log")
    ph_b4 <- cf_of(eq_list$phillips, "wti_yoy_log")
    trj$inflation_yoy_d1[idx] <- ph_b0 +
      ph_b1 * trj$inflation_yoy_d1[prv] +
      ph_b2 * trj$output_gap[prv] +
      ph_b3 * trj$fx_sell_yoy_log[idx] +
      ph_b4 * trj$wti_yoy_log[idx]
    trj$inflation_yoy[idx] <- trj$inflation_yoy[prv] + trj$inflation_yoy_d1[idx]

    # 11.5  Taylor
    ta_b0 <- cf_of(eq_list$taylor, "(Intercept)")
    ta_b1 <- cf_of(eq_list$taylor, "tpm_lag1")
    ta_b2 <- cf_of(eq_list$taylor, "inflation_yoy")
    ta_b3 <- cf_of(eq_list$taylor, "output_gap")
    ta_b4 <- cf_of(eq_list$taylor, "q_gap")
    trj$tpm[idx] <- ta_b0 +
      ta_b1 * trj$tpm[prv] +
      ta_b2 * trj$inflation_yoy[idx] +
      ta_b3 * trj$output_gap[prv] +
      ta_b4 * trj$q_gap[prv]

    trj$r_real[idx] <- trj$tpm[idx] - trj$inflation_yoy[prv]

    # 11.6  IS_TV (la trampa)
    tv_b0 <- cf_of(eq_list$IS_TV, "(Intercept)")
    tv_b1 <- cf_of(eq_list$IS_TV, "us_ip_yoy_log")
    tv_b2 <- cf_of(eq_list$IS_TV, "itcer_yoy_log")
    tv_b3 <- cf_of(eq_list$IS_TV, "tot_yoy_log")
    tv_b4 <- cf_of(eq_list$IS_TV, "y_TV_yoy_log_lag1")
    trj$y_TV_yoy_log[idx] <- tv_b0 +
      tv_b1 * trj$us_ip_yoy_log[idx] +
      tv_b2 * trj$itcer_yoy_log[idx] +
      tv_b3 * trj$tot_yoy_log[idx] +
      tv_b4 * trj$y_TV_yoy_log[prv]

    # 11.7  IS_TH (hedge USD-USD)
    th_b0 <- cf_of(eq_list$IS_TH, "(Intercept)")
    th_b1 <- cf_of(eq_list$IS_TH, "us_ip_yoy_log")
    th_b2 <- cf_of(eq_list$IS_TH, "tot_yoy_log")
    th_b3 <- cf_of(eq_list$IS_TH, "y_TH_yoy_log_lag1")
    trj$y_TH_yoy_log[idx] <- th_b0 +
      th_b1 * trj$us_ip_yoy_log[idx] +
      th_b2 * trj$tot_yoy_log[idx] +
      th_b3 * trj$y_TH_yoy_log[prv]

    # 11.8  IS_NH (hedge CRC-CRC)
    nh_b0 <- cf_of(eq_list$IS_NH, "(Intercept)")
    nh_b1 <- cf_of(eq_list$IS_NH, "r_real")
    nh_b2 <- cf_of(eq_list$IS_NH, "cr_col_yoy_log")
    nh_b3 <- cf_of(eq_list$IS_NH, "y_NH_yoy_log_lag1")
    trj$y_NH_yoy_log[idx] <- nh_b0 +
      nh_b1 * trj$r_real[idx] +
      nh_b2 * trj$cr_col_yoy_log[prv] +
      nh_b3 * trj$y_NH_yoy_log[prv]

    # 11.9  IS_NB (beneficiado)
    nb_b0 <- cf_of(eq_list$IS_NB, "(Intercept)")
    nb_b1 <- cf_of(eq_list$IS_NB, "r_real")
    nb_b2 <- cf_of(eq_list$IS_NB, "itcer_yoy_log")
    nb_b3 <- cf_of(eq_list$IS_NB, "y_NB_yoy_log_lag1")
    trj$y_NB_yoy_log[idx] <- nb_b0 +
      nb_b1 * trj$r_real[idx] +
      nb_b2 * trj$itcer_yoy_log[idx] +
      nb_b3 * trj$y_NB_yoy_log[prv]

    # 11.10 Métricas del impuesto silencioso
    trj$tax_squeeze_TV[idx] <- trj$y_TH_yoy_log[idx] - trj$y_TV_yoy_log[idx]
    trj$tax_subsidy_NB[idx] <- trj$y_NB_yoy_log[idx] - trj$y_NH_yoy_log[idx]
    trj$silent_tax_amplitude[idx] <- trj$tax_squeeze_TV[idx] +
                                     trj$tax_subsidy_NB[idx]

    # 11.11 Output gap sintetico (promedio ponderado por PIB)
    y_synth <- pib_weights["TV"] * trj$y_TV_yoy_log[idx] +
               pib_weights["TH"] * trj$y_TH_yoy_log[idx] +
               pib_weights["NH"] * trj$y_NH_yoy_log[idx] +
               pib_weights["NB"] * trj$y_NB_yoy_log[idx]
    y_synth_prev <- pib_weights["TV"] * trj$y_TV_yoy_log[prv] +
                    pib_weights["TH"] * trj$y_TH_yoy_log[prv] +
                    pib_weights["NH"] * trj$y_NH_yoy_log[prv] +
                    pib_weights["NB"] * trj$y_NB_yoy_log[prv]
    trj$output_gap[idx] <- trj$output_gap[prv] + 0.5 * (y_synth - y_synth_prev)

    # 11.12 q_gap (proxy mecanico)
    trj$q_gap[idx] <- trj$q_gap[prv] + 0.7 * (trj$itcer_yoy_log[idx] -
                                               trj$itcer_yoy_log[prv])

    # 11.13 Credito en colones (semi-exogeno)
    trj$cr_col_yoy_log[idx] <- 0.85 * trj$cr_col_yoy_log[prv] -
                                0.02 * (trj$r_real[idx] - trj$r_real[prv])
  }

  trj
}

# ==============================================================
# 12. DIAGNÓSTICOS DE ESTABILIDAD (CHOW)
# ==============================================================

chow_split_test <- function(formula, data, break_dates = c("2018-01-01",
                                                             "2020-03-01")) {
  res <- list()
  for (bd in break_dates) {
    bd_date <- as.Date(bd)
    if (!"date" %in% names(data)) {
      res[[bd]] <- tibble::tibble(break_date = bd, F = NA, p = NA,
                                   note = "no date column")
      next
    }
    d1 <- data[data$date <  bd_date, ]
    d2 <- data[data$date >= bd_date, ]
    if (nrow(d1) < 20 || nrow(d2) < 20) {
      res[[bd]] <- tibble::tibble(break_date = bd, F = NA, p = NA,
                                   note = "n insuficiente")
      next
    }
    f_full <- lm(formula, data = data)
    f1     <- lm(formula, data = d1)
    f2     <- lm(formula, data = d2)
    rss_full <- sum(residuals(f_full)^2)
    rss_split <- sum(residuals(f1)^2) + sum(residuals(f2)^2)
    k <- length(coef(f_full))
    n <- nobs(f_full)
    F_stat <- ((rss_full - rss_split) / k) / (rss_split / (n - 2 * k))
    p_val  <- pf(F_stat, k, n - 2 * k, lower.tail = FALSE)
    res[[bd]] <- tibble::tibble(break_date = bd,
                                 F = round(F_stat, 3),
                                 p = round(p_val, 4),
                                 note = "")
  }
  dplyr::bind_rows(res)
}
