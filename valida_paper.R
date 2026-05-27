# =========================================================================
# valida_paper.R — Validación independiente de reproducibilidad
# -------------------------------------------------------------------------
# Verifica los seis resultados centrales de la Prueba 5 del paper
# "Abundancia cambiaria y la brecha creciente de dos economías"
# (Soto Rodríguez, 2026) contra tolerancias documentadas.
#
# Diseñado para ejecutarse desde una sesión externa que clona el repo
# público, configura sus propias credenciales SDDE en .Renviron local
# y ejecuta este script tras correr los scripts 27 a 30.
#
# Salida: bloque de texto con el resultado de cada check (PASS/FAIL/SKIP)
# y un resumen final. Las cifras y tolerancias responden directamente a
# los hallazgos reportados en el paper.
# =========================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lubridate)
})

cat("======================================================================\n")
cat("VALIDACIÓN DE REPRODUCIBILIDAD — pipeline_fx_forecast_cr\n")
cat("Paper: 'Abundancia cambiaria y la brecha creciente de dos economías'\n")
cat(sprintf("Fecha de ejecución: %s\n", format(Sys.Date())))
cat("======================================================================\n")

results <- list()

check <- function(id, desc, ok, msg) {
  status <- if (is.na(ok)) "SKIP" else if (ok) "PASS" else "FAIL"
  cat(sprintf("[%s] %s — %s\n      %s\n", id, status, desc, msg))
  results[[id]] <<- status
}

# -------------------------------------------------------------------------
# c1: Manifest de descargas — series del crédito disponibles en raw
# -------------------------------------------------------------------------
manifest_path <- "cierre_v6/outputs/raw_pinza_manifest.csv"
ok1 <- FALSE; msg1 <- "Manifest no encontrado"
if (file.exists(manifest_path)) {
  mf <- read_csv(manifest_path, show_col_types = FALSE)
  credit_rows <- mf %>% filter(bloque == "credit_sbn")
  ok_count   <- sum(credit_rows$status == "ok",   na.rm = TRUE)
  fail_count <- sum(credit_rows$status == "fail", na.rm = TRUE)
  ok1 <- (ok_count >= 18) && (fail_count <= 8)
  msg1 <- sprintf("Manifest crédito: %d OK + %d fail (esperado: >=18 OK, fail <=8 — fails son agregadores cuyos hijos sí descargan)",
                  ok_count, fail_count)
}
check("c1", "Manifest descargas crédito", ok1, msg1)

# -------------------------------------------------------------------------
# c2: Crédito USD agregado — desaceleración post-2022 esperada
# -------------------------------------------------------------------------
me_path <- "cierre_v6/outputs/raw/credit_sbn_TOTAL_ME__.csv"
ok2 <- NA; msg2 <- "Archivo de crédito USD no encontrado"
if (file.exists(me_path)) {
  me <- read_csv(me_path, show_col_types = FALSE)
  names(me) <- c("date","value")
  me$date <- as.Date(me$date)
  me <- me %>% arrange(date) %>%
        mutate(g_yoy = 100 * (value / lag(value, 12) - 1))
  pre  <- me %>% filter(date >= "2010-01-01", date <  "2022-01-01")
  post <- me %>% filter(date >= "2022-01-01")
  mean_pre  <- mean(pre$g_yoy,  na.rm = TRUE)
  mean_post <- mean(post$g_yoy, na.rm = TRUE)
  delta <- mean_post - mean_pre
  # Esperado: pre ~ 8.7%, post ~ 1.1%, delta < -6 pp
  ok2 <- (delta < -6) && (mean_post < 3)
  msg2 <- sprintf("Crédito USD YoY: pre=%.2f%%, post=%.2f%%, Δ=%.2f pp (esperado Δ<-6 y post<3)",
                  mean_pre, mean_post, delta)
}
check("c2", "Crédito USD post-2022", ok2, msg2)

# -------------------------------------------------------------------------
# c3: Crédito CRC agregado — NO se desacelera significativamente
# -------------------------------------------------------------------------
mn_path <- "cierre_v6/outputs/raw/credit_sbn_TOTAL_MN__.csv"
ok3 <- NA; msg3 <- "Archivo de crédito CRC no encontrado"
if (file.exists(mn_path)) {
  mn <- read_csv(mn_path, show_col_types = FALSE)
  names(mn) <- c("date","value")
  mn$date <- as.Date(mn$date)
  mn <- mn %>% arrange(date) %>%
        mutate(g_yoy = 100 * (value / lag(value, 12) - 1))
  pre  <- mn %>% filter(date >= "2010-01-01", date <  "2022-01-01")
  post <- mn %>% filter(date >= "2022-01-01")
  mean_pre  <- mean(pre$g_yoy,  na.rm = TRUE)
  mean_post <- mean(post$g_yoy, na.rm = TRUE)
  delta <- mean_post - mean_pre
  # Esperado: pre ~ 8.7%, post ~ 7.9%, |delta| < 2 pp
  ok3 <- abs(delta) < 2
  msg3 <- sprintf("Crédito CRC YoY: pre=%.2f%%, post=%.2f%%, Δ=%.2f pp (esperado |Δ|<2)",
                  mean_pre, mean_post, delta)
}
check("c3", "Crédito CRC post-2022 (asimetría)", ok3, msg3)

# -------------------------------------------------------------------------
# c4: Chow test (sup-Wald) @ 2022-01-01 sobre crédito USD
# -------------------------------------------------------------------------
ok4 <- NA; msg4 <- "No se pudo calcular Chow test"
if (file.exists(me_path)) {
  me <- read_csv(me_path, show_col_types = FALSE)
  names(me) <- c("date","value")
  me$date <- as.Date(me$date)
  me <- me %>% arrange(date) %>%
        mutate(g_yoy = 100 * (value / lag(value, 12) - 1)) %>%
        filter(!is.na(g_yoy), date >= "2010-01-01")
  y <- me$g_yoy
  idx <- which(me$date >= as.Date("2022-01-01"))[1]
  n <- length(y); k <- 1
  rss_p <- sum((y - mean(y))^2)
  rss_1 <- sum((y[1:idx]      - mean(y[1:idx]))^2)
  rss_2 <- sum((y[(idx+1):n]  - mean(y[(idx+1):n]))^2)
  rss_u <- rss_1 + rss_2
  Fstat <- ((rss_p - rss_u)/k) / (rss_u/(n - 2*k))
  pval  <- 1 - pf(Fstat, k, n - 2*k)
  ok4 <- Fstat > 15 && pval < 0.001
  msg4 <- sprintf("Chow USD @ 2022-01: F=%.2f, p=%.4f (esperado F>15, p<0.001)",
                  Fstat, pval)
}
check("c4", "Chow test crédito USD @ 2022", ok4, msg4)

# -------------------------------------------------------------------------
# c5: Ranking sectorial 2025 — Vivienda + Industria USD deben caer
# -------------------------------------------------------------------------
ok5 <- NA; msg5 <- "Archivos sectoriales USD no encontrados"
sectores_corto <- c("AGRO","CONSTRUCCION","COMERCIO_REST_HOT","OTROS_SERVICIOS",
                    "VIVIENDA","INDUSTRIA_OTRAS","CONSUMO_OTROS","OTROS_PRESTAMOS")
g_2025 <- tibble(sector = character(), g_yoy = numeric())
for (s in sectores_corto) {
  f <- sprintf("cierre_v6/outputs/raw/credit_sbn_%s_ME__.csv", s)
  if (file.exists(f)) {
    d <- read_csv(f, show_col_types = FALSE)
    names(d) <- c("date","value")
    d$date <- as.Date(d$date)
    d <- d %>% arrange(date) %>%
         mutate(g_yoy = 100 * (value / lag(value, 12) - 1)) %>%
         filter(year(date) == 2025, !is.na(g_yoy))
    if (nrow(d) > 0) g_2025 <- bind_rows(g_2025, tibble(sector = s, g_yoy = mean(d$g_yoy)))
  }
}
if (nrow(g_2025) >= 6) {
  g_2025 <- g_2025 %>% arrange(g_yoy)
  viv <- g_2025 %>% filter(sector == "VIVIENDA") %>% pull(g_yoy)
  ind <- g_2025 %>% filter(sector == "INDUSTRIA_OTRAS") %>% pull(g_yoy)
  # Esperado: Vivienda USD < -10 %, Industria USD < -5 %
  ok5 <- length(viv) > 0 && length(ind) > 0 && viv[1] < -10 && ind[1] < -5
  msg5 <- sprintf("Sectorial 2025 USD: Vivienda=%.2f%%, Industria=%.2f%% (esperado Viv<-10, Ind<-5)",
                  ifelse(length(viv)>0, viv[1], NA_real_),
                  ifelse(length(ind)>0, ind[1], NA_real_))
}
check("c5", "Ranking sectorial 2025 USD", ok5, msg5)

# -------------------------------------------------------------------------
# c6: SUGEF — razón CEC/total ME debe estar entre 55% y 70%
# -------------------------------------------------------------------------
# El reporte SUGEF Acuerdo 2-10 más reciente reporta razón CEC ~ 61.8 %
# a cierre 2022. La razón se ha movido en un rango estable. Si el archivo
# local no está, este check se marca SKIP (la cifra del paper viene del
# reporte público de SUGEF, no de una descarga automatizada).
ok6 <- NA
msg6 <- "Cifra SUGEF se toma del reporte público (sugef.fi.cr, Acuerdo 2-10): CEC/total ME ≈ 61.8% al cierre 2022."
sugef_path <- "output/sugef_cec_ratios.csv"
if (file.exists(sugef_path)) {
  sg <- read_csv(sugef_path, show_col_types = FALSE)
  if ("cec_share_me" %in% names(sg) && nrow(sg) > 0) {
    last_ratio <- 100 * tail(sg$cec_share_me, 1)
    ok6 <- last_ratio >= 55 && last_ratio <= 70
    msg6 <- sprintf("SUGEF CEC/total ME = %.2f%% (esperado entre 55%% y 70%%)", last_ratio)
  }
}
check("c6", "SUGEF: razón CEC/total ME", ok6, msg6)

# -------------------------------------------------------------------------
# c7: Crédito USD como % PIB cae > 3 pp post-2022 (lectura normalizada)
# -------------------------------------------------------------------------
ok7 <- NA; msg7 <- "Archivo crédito/PIB no encontrado"
ratios_path <- "output/credito_pct_pib.csv"
if (file.exists(ratios_path)) {
  r7 <- read_csv(ratios_path, show_col_types = FALSE)
  pre_me  <- r7 %>% filter(year <  2022) %>% pull(credit_me_pct_pib) %>% mean()
  post_me <- r7 %>% filter(year >= 2022) %>% pull(credit_me_pct_pib) %>% mean()
  delta   <- post_me - pre_me
  pre_mn  <- r7 %>% filter(year <  2022) %>% pull(credit_mn_pct_pib) %>% mean()
  post_mn <- r7 %>% filter(year >= 2022) %>% pull(credit_mn_pct_pib) %>% mean()
  delta_mn <- post_mn - pre_mn
  # Esperado: USD/PIB cae al menos 3 pp; CRC/PIB se mantiene o sube
  ok7 <- (delta < -3) && (delta_mn > -1)
  msg7 <- sprintf("USD/PIB: pre=%.2f%%, post=%.2f%% (Δ=%.2f pp); CRC/PIB: pre=%.2f%%, post=%.2f%% (Δ=%.2f pp). Esperado USD Δ<-3, CRC Δ>-1.",
                  pre_me, post_me, delta, pre_mn, post_mn, delta_mn)
} else {
  msg7 <- "Correr scripts/31_credit_pib_ratio.R antes para generar credito_pct_pib.csv"
}
check("c7", "Crédito USD/PIB cae bajo régimen abundancia", ok7, msg7)

# -------------------------------------------------------------------------
# c8: Prueba 2 sobre 15 sectores CIIU — rechazo en >= 4 sectores @ 2022-01
# -------------------------------------------------------------------------
# La hipótesis no es rechazo uniforme; es rechazo localizado en sectores
# con composición monetaria asimétrica. Esperado: al menos 4 secciones
# rechazan estabilidad al 5% en 2022-01-01, incluyendo Manufactura y
# Financieras (los sectores estructuralmente más cíclicos).
ok8 <- NA; msg8 <- "Archivo prueba2_15ciiu.csv no encontrado — correr scripts/33_prueba2_15ciiu.R"
p8_path <- "output/prueba2_15ciiu.csv"
if (file.exists(p8_path)) {
  r8 <- read_csv(p8_path, show_col_types = FALSE)
  col_p_2022 <- grep("p_.*2022", names(r8), value = TRUE)[1]
  if (!is.na(col_p_2022)) {
    sig <- r8 %>% filter(.data[[col_p_2022]] < 0.05) %>% pull(name)
    n_sig <- length(sig)
    has_manu <- any(grepl("Manufactura", sig))
    has_fin  <- any(grepl("Financieras", sig))
    ok8 <- (n_sig >= 4) && has_manu && has_fin
    msg8 <- sprintf("Prueba 2 @ 2022-01: %d/%d sectores rechazan estabilidad; incluyen Manufactura=%s, Financieras=%s",
                    n_sig, nrow(r8), has_manu, has_fin)
  }
}
check("c8", "Prueba 2 sobre 15 CIIU @ 2022", ok8, msg8)

# -------------------------------------------------------------------------
# c9: MONEX — excedente diario 2025 entre USD 20 y USD 35 millones
# -------------------------------------------------------------------------
ok9 <- NA; msg9 <- "Archivo MONEX no encontrado — correr scripts/32_monex_excedente.R"
m9_path <- "output/monex_excedente_anual.csv"
if (file.exists(m9_path)) {
  m9 <- read_csv(m9_path, show_col_types = FALSE)
  if ("excedente_diario_prom" %in% names(m9)) {
    last_year <- max(m9$year, na.rm = TRUE)
    last_obs <- m9 %>% filter(year == last_year)
    if (nrow(last_obs) > 0) {
      exc <- last_obs$excedente_diario_prom[1]
      ok9 <- (exc >= 20) && (exc <= 35)
      msg9 <- sprintf("MONEX excedente diario %d: %.1f M USD (esperado entre 20 y 35)",
                      last_year, exc)
    }
  }
} else {
  msg9 <- paste(msg9, "Los códigos MONEX (3324, 3325, 3326) en series_bccr_template.csv",
                "están marcados POR_VERIFICAR; ajustar si no responden.")
}
check("c9", "MONEX excedente diario coincide con cifra del paper", ok9, msg9)

# -------------------------------------------------------------------------
# c10: Pinza cambiaria sectorial (reporte 1900)
#   - Excedente diario promedio 2025 entre USD 20 y 30 millones
#   - Comercio aparece como demandante neto (saldo Compra-Venta < 0)
#   - Profesional/M-N aparece como oferente neto (Compra-Venta > 0)
# -------------------------------------------------------------------------
ok10 <- NA; msg10 <- "Correr scripts/34_pinza_cambiaria_sectorial.R primero"
anual_path <- "output/pinza_cambiaria_anual.csv"
sect_path  <- "output/pinza_cambiaria_sectorial.csv"
if (file.exists(anual_path) && file.exists(sect_path)) {
  an <- read_csv(anual_path, show_col_types = FALSE)
  se <- read_csv(sect_path,  show_col_types = FALSE)
  y2025 <- an %>% filter(year == 2025)
  if (nrow(y2025) > 0) {
    neto_d <- y2025$Neto_diaria[1]
    comercio_neto <- se %>% filter(grepl("Comercio", desc)) %>% pull(saldo_neto) %>% first()
    prof_neto     <- se %>% filter(grepl("profesionales", desc)) %>% pull(saldo_neto) %>% first()
    cond_neto     <- !is.na(neto_d) && (neto_d >= 20) && (neto_d <= 30)
    cond_comercio <- !is.na(comercio_neto) && comercio_neto < 0
    cond_prof     <- !is.na(prof_neto)     && prof_neto     > 0
    ok10 <- cond_neto && cond_comercio && cond_prof
    msg10 <- sprintf("2025 neto diario=%.2f M (esperado [20,30]); Comercio neto=%.0f (esperado <0); Profesional neto=%.0f (esperado >0)",
                     neto_d, comercio_neto, prof_neto)
  }
}
check("c10", "Pinza cambiaria 2025 + saldos sectoriales", ok10, msg10)

# -------------------------------------------------------------------------
# Resumen
# -------------------------------------------------------------------------
pass <- sum(unlist(results) == "PASS")
fail <- sum(unlist(results) == "FAIL")
skip <- sum(unlist(results) == "SKIP")
total <- length(results)
cat("======================================================================\n")
cat(sprintf("RESUMEN: %d PASS, %d FAIL, %d SKIP de %d checks\n", pass, fail, skip, total))
cat("======================================================================\n")
