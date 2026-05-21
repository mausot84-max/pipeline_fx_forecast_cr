# ============================================================
# cierre_v6/02d_descargar_rama_sectorial_agregada.R
# ============================================================
# Descarga la rama 1416-1429 (Sistema Bancario Nacional —
# Crédito por actividad económica) y 1497-1510 (Sistema
# Financiero Nacional). Son series MENSUALES SECTORIALES
# AGREGADAS POR MONEDA (todo expresado en colones).
#
# No nos dan σ_C sectorial directamente (porque no separan
# MN/ME por sector), pero sirven para tres cosas:
#
#   1. Confirmar que tienen serie larga 2000+ (la sospecha es
#      que esta rama precede al desglose por moneda).
#   2. Reproducir la cifra 32.7% del BCCR si esa cifra viene
#      de otro código distinto al 20578/20579.
#   3. Si tienen historia larga sectorial agregada y combinamos
#      con TOTAL MN/ME largo (20578/20579), permitirían inferir
#      σ_C sectorial pre-2024 bajo el supuesto de proporción
#      sectorial estable.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate)
})

if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
dir.create(RAW, recursive = TRUE, showWarnings = FALSE)

# Códigos del Sistema Bancario Nacional (rama E07.07.07.01.XX, todos M.CRC)
rama_sbn_agregado <- tribble(
  ~codigo,  ~etiqueta,                              ~clave,
  "1416",   "SBN-Agricultura (CRC agregado)",       "credit_sbn_agg_AGRO",
  "1419",   "SBN-Industria (CRC agregado)",         "credit_sbn_agg_INDUSTRIA",
  "1420",   "SBN-Vivienda (CRC agregado)",          "credit_sbn_agg_VIVIENDA",
  "1421",   "SBN-Construcción (CRC agregado)",      "credit_sbn_agg_CONSTRUCCION",
  "1422",   "SBN-Turismo (CRC agregado)",           "credit_sbn_agg_TURISMO",
  "1424",   "SBN-Comercio (CRC agregado)",          "credit_sbn_agg_COMERCIO",
  "1425",   "SBN-Servicios (CRC agregado)",         "credit_sbn_agg_SERVICIOS",
  "1426",   "SBN-Consumo (CRC agregado)",           "credit_sbn_agg_CONSUMO",
  "1427",   "SBN-Transporte (CRC agregado)",        "credit_sbn_agg_TRANSPORTE",
  "1429",   "SBN-Otras Actividades (CRC agregado)", "credit_sbn_agg_OTRAS",
  "1415",   "SBN-TOTAL (CRC agregado)",             "credit_sbn_agg_TOTAL"
)

message(sprintf("[02d] %d series a descargar (rama SBN agregada por moneda).",
                nrow(rama_sbn_agregado)))
message("[02d] Pausa inicial de 20 s...")
Sys.sleep(20)

descargar <- function(codigo, verbose = TRUE) {
  tryCatch(
    download_bccr_series(codigo, "2000/01/01",
                         format(Sys.Date(), "%Y/%m/%d"),
                         credentials = creds, verbose = verbose, save_raw = TRUE),
    error = function(e) { message("  ! ", conditionMessage(e)); NULL }
  )
}

resumen <- list()

for (i in seq_len(nrow(rama_sbn_agregado))) {
  r <- rama_sbn_agregado[i, ]
  message(sprintf("[%d/%d] %s code=%s", i, nrow(rama_sbn_agregado), r$clave, r$codigo))
  df <- descargar(r$codigo)
  if (!is.null(df) && nrow(df) > 0) {
    out <- df %>% transmute(fecha = date, valor = value)
    fname <- file.path(RAW, paste0(r$clave, ".csv"))
    write_csv(out, fname)
    resumen[[r$codigo]] <- list(
      clave = r$clave, status = "ok",
      n = nrow(out),
      min_f = as.character(min(out$fecha)),
      max_f = as.character(max(out$fecha))
    )
    n_nz <- sum(out$valor != 0, na.rm = TRUE)
    message(sprintf("  OK (%d obs, %s a %s, %d no-cero)",
                    nrow(out), min(out$fecha), max(out$fecha), n_nz))
  } else {
    resumen[[r$codigo]] <- list(clave = r$clave, status = "fail", n = 0)
    message("  FAIL")
  }
  Sys.sleep(3)
}

# ---- Test de discrepancia BCCR (32.7% vs 40.05%) -----------------
# Probar el código 4814 (Crédito sector privado en colones — "validación cruzada")
# que aparece en codes_seleccionados.csv pero falló en el 02 original
message("\n[02d] Test: código 4814 (Crédito sector privado en colones)")
df_4814 <- descargar("4814")
if (!is.null(df_4814) && nrow(df_4814) > 0) {
  out <- df_4814 %>% transmute(fecha = date, valor = value)
  n_nz <- sum(out$valor != 0, na.rm = TRUE)
  message(sprintf("  4814: %d obs (%s a %s, %d no-cero)",
                  nrow(out), min(out$fecha), max(out$fecha), n_nz))
  write_csv(out, file.path(RAW, "credit_total_4814.csv"))
} else {
  message("  4814 fallo")
}
Sys.sleep(3)

# Códigos del SFN paralelo (E07.07.07.02 — puede que tenga más cobertura)
message("\n[02d] Test: rama Sistema Financiero Nacional (SFN, 1496)")
df_sfn <- descargar("1496")
if (!is.null(df_sfn) && nrow(df_sfn) > 0) {
  out <- df_sfn %>% transmute(fecha = date, valor = value)
  message(sprintf("  SFN-TOTAL: %d obs (%s a %s)",
                  nrow(out), min(out$fecha), max(out$fecha)))
  write_csv(out, file.path(RAW, "credit_sfn_agg_TOTAL.csv"))
} else {
  message("  SFN fallo")
}

# ---- Resumen final -----------------------------------------------
message("\n[02d] === Resumen ===")
ok_n <- sum(sapply(resumen, function(x) x$status == "ok"))
message(sprintf("SBN agregado: %d OK / %d total", ok_n, length(resumen)))
for (cod in names(resumen)) {
  r <- resumen[[cod]]
  if (r$status == "ok") {
    message(sprintf("  %s (%s): %d obs (%s a %s)",
                    cod, r$clave, r$n, r$min_f, r$max_f))
  } else {
    message(sprintf("  %s (%s): FAIL", cod, r$clave))
  }
}
