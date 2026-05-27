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
# Resumen
# -------------------------------------------------------------------------
pass <- sum(unlist(results) == "PASS")
fail <- sum(unlist(results) == "FAIL")
skip <- sum(unlist(results) == "SKIP")
total <- length(results)
cat("======================================================================\n")
cat(sprintf("RESUMEN: %d PASS, %d FAIL, %d SKIP de %d checks\n", pass, fail, skip, total))
cat("======================================================================\n")
