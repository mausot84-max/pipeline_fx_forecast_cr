# ============================================================
# cierre_v6/02c_descargar_leaf_faltantes.R
# ============================================================
# Diagnóstico: en el árbol del BCCR los códigos 20289 (Industria),
# 20081 (Consumo) y 20621 (Servicios) son AGREGADORES con
# `medida=1` y `Medida=''`, no series propias. Sus hijos sí
# tienen datos (`medida=4, Millones`).
#
# Este script descarga los 6 leaf que reemplazan a los 3
# agregadores fallidos en cada moneda:
#
#   Industria (MN) = Construcción(20079) + Otras Industrias(20333)
#   Industria (ME) = Construcción(20080) + Otras Industrias(20334)
#   Consumo   (MN) = Otros Consumos(20437) + Tarjetas crédito(20653)
#   Consumo   (ME) = Otros Consumos(20438) + Tarjetas crédito(20654)
#   Servicios (MN) = Otros Servicios(20501) + Transp/Comunic(20667)
#   Servicios (ME) = Otros Servicios(20502) + Transp/Comunic(20668)
#
# Construcción, Transp/Comunic y Otros Servicios ya están en el
# raw/, así que sólo bajamos 6 leaf nuevas: 20333, 20334, 20437,
# 20438, 20653, 20654.
#
# Adicionalmente prueba el IPX DEFINITIVO (82543) con ventana
# 2018-2026 para diagnosticar por qué falló con la ventana original.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate)
})

if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
MANIFEST_FILE <- file.path(OUT, "raw_pinza_manifest.csv")
dir.create(RAW, recursive = TRUE, showWarnings = FALSE)

# ------ Nuevas leaf de crédito SBN -----------------------------
leaf_extra <- tribble(
  ~bloque,      ~seccion,                ~moneda, ~regimen, ~codigo_sdde, ~etiqueta,
  "credit_sbn", "INDUSTRIA_OTRAS",        "MN",   "",       "20333",      "Otras Industrias — hipótesis MN (leaf)",
  "credit_sbn", "INDUSTRIA_OTRAS",        "ME",   "",       "20334",      "Otras Industrias — hipótesis ME (leaf)",
  "credit_sbn", "CONSUMO_OTROS",          "MN",   "",       "20437",      "Otros Consumos — hipótesis MN (leaf)",
  "credit_sbn", "CONSUMO_OTROS",          "ME",   "",       "20438",      "Otros Consumos — hipótesis ME (leaf)",
  "credit_sbn", "CONSUMO_TARJETAS",       "MN",   "",       "20653",      "Tarjetas de crédito — hipótesis MN (leaf)",
  "credit_sbn", "CONSUMO_TARJETAS",       "ME",   "",       "20654",      "Tarjetas de crédito — hipótesis ME (leaf)"
)

message("[02c] Pausa inicial de 30 s...")
Sys.sleep(30)

descargar_una <- function(codigo, start = "2010/01/01", verbose = TRUE) {
  tryCatch(
    download_bccr_series(codigo, start,
                         format(Sys.Date(), "%Y/%m/%d"),
                         credentials = creds, verbose = verbose, save_raw = TRUE),
    error = function(e) { message("  ! ", conditionMessage(e)); NULL }
  )
}

build_key <- function(r) {
  paste(r$bloque,
        ifelse(r$seccion == "" | is.na(r$seccion), "_", r$seccion),
        ifelse(r$moneda  == "" | is.na(r$moneda),  "_", r$moneda),
        ifelse(r$regimen == "" | is.na(r$regimen), "_", r$regimen),
        sep = "_")
}

# Lee manifest existente para no duplicar filas
manifest <- read_csv(MANIFEST_FILE, show_col_types = FALSE,
                     col_types = cols(.default = col_character(),
                                      n_obs = col_integer()))

resultados <- list()

for (i in seq_len(nrow(leaf_extra))) {
  r   <- leaf_extra[i, ]
  key <- build_key(r)
  message(sprintf("[%d/%d] %s code=%s", i, nrow(leaf_extra), key, r$codigo_sdde))

  df <- descargar_una(r$codigo_sdde, "2010/01/01")
  if (!is.null(df) && nrow(df) > 0) {
    out   <- df %>% transmute(fecha = date, valor = value)
    fname <- file.path(RAW, paste0(key, ".csv"))
    write_csv(out, fname)
    resultados[[r$codigo_sdde]] <- list(
      status = "ok",
      n      = nrow(out),
      min_f  = as.character(min(out$fecha)),
      max_f  = as.character(max(out$fecha))
    )
    # Agregar al manifest
    nueva_fila <- tibble(
      bloque = r$bloque, seccion = r$seccion, moneda = r$moneda, regimen = r$regimen,
      codigo_sdde = r$codigo_sdde, etiqueta = r$etiqueta,
      status = "ok", n_obs = nrow(out), archivo = fname,
      min_fecha = as.character(min(out$fecha)),
      max_fecha = as.character(max(out$fecha))
    )
    # Si la fila ya existía la quitamos para reemplazar
    manifest <- manifest %>%
      filter(codigo_sdde != r$codigo_sdde) %>%
      bind_rows(nueva_fila)
    message(sprintf("  OK (%d obs, %s a %s)",
                    nrow(out), min(out$fecha), max(out$fecha)))
  } else {
    resultados[[r$codigo_sdde]] <- list(status = "fail", n = 0)
    message("  FAIL")
  }
  Sys.sleep(4)
}

# ------ Diagnóstico IPX DEFINITIVO ------------------------------
message("\n[02c] Diagnóstico IPX DEFINITIVO (82543) con ventana 2018-2026...")
Sys.sleep(10)
df_ipx <- descargar_una("82543", "2018/01/01")
if (!is.null(df_ipx) && nrow(df_ipx) > 0) {
  message(sprintf("  IPX DEF (82543) - 2018+: OK (%d obs, %s a %s)",
                  nrow(df_ipx), min(df_ipx$date), max(df_ipx$date)))
  out <- df_ipx %>% transmute(fecha = date, valor = value)
  fname <- file.path(RAW, "ipx_____DEFINITIVO.csv")
  write_csv(out, fname)
  # Actualizar manifest
  manifest <- manifest %>%
    filter(codigo_sdde != "82543") %>%
    bind_rows(tibble(
      bloque = "ipx", seccion = "", moneda = "", regimen = "DEFINITIVO",
      codigo_sdde = "82543",
      etiqueta = "Índice precios exportaciones Régimen Definitivo",
      status = "ok", n_obs = nrow(out), archivo = fname,
      min_fecha = as.character(min(out$fecha)),
      max_fecha = as.character(max(out$fecha))
    ))
} else {
  message("  IPX DEF (82543) sigue fallando con ventana 2018+. Probablemente código retirado.")
}

# ------ Escribir manifest actualizado ---------------------------
write_csv(manifest, MANIFEST_FILE)

# ------ Resumen ------------------------------------------------
message("\n[02c] === Resumen ===")
ok_n   <- sum(sapply(resultados, function(x) x$status == "ok"))
fail_n <- length(resultados) - ok_n
message(sprintf("Leaf de crédito SBN: %d OK / %d total", ok_n, length(resultados)))
for (cod in names(resultados)) {
  r <- resultados[[cod]]
  if (r$status == "ok") {
    message(sprintf("  %s: %d obs (%s a %s)", cod, r$n, r$min_f, r$max_f))
  } else {
    message(sprintf("  %s: FAIL", cod))
  }
}
message("\n[02c] Manifest actualizado en ", MANIFEST_FILE)
message("[02c] Próximo paso: actualizar script 10 para reconstruir Industria/Servicios/Consumo como suma de leaf.")
