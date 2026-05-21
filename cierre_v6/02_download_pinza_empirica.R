# ============================================================
# cierre_v6/02_download_pinza_empirica.R
# ============================================================
# Descarga las series sectoriales seleccionadas en
# cierre_v6/codes_seleccionados.csv después de inspeccionar el
# discovery del script 01.
#
# Se ejecuta desde el R project pipeline_fx_forecast_cr. Reutiliza
# utils_bccr.R::download_bccr_series para mantener consistencia
# (cache, headers, parser) con el resto del pipeline.
#
# Estructura esperada de codes_seleccionados.csv:
#   bloque,seccion,moneda,regimen,codigo_sdde,etiqueta
#
# bloque        ∈ {imae_ciiu, imae_regimen, exports, credit}
# seccion       ∈ {TOTAL, A, C, F, G, H, I, J, K, L, MN}   (vacío si no aplica)
# moneda        ∈ {CRC, USD, TOTAL}                          (vacío si no aplica)
# regimen       ∈ {DEFINITIVO, ESPECIAL}                     (vacío si no aplica)
# codigo_sdde   código numérico del SDDE
# etiqueta      descripción libre
#
# Salida:
#   cierre_v6/outputs/raw/<key>.csv  (una por serie)
#   cierre_v6/outputs/raw_pinza_manifest.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate)
})

if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
dir.create(RAW, recursive = TRUE, showWarnings = FALSE)

CODES_FILE <- "cierre_v6/codes_seleccionados.csv"
if (!file.exists(CODES_FILE)) {
  stop("Falta cierre_v6/codes_seleccionados.csv. Editalo a partir del discovery del script 01.")
}
codes <- read_csv(CODES_FILE, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))

stopifnot(all(c("bloque","seccion","moneda","regimen","codigo_sdde","etiqueta") %in% names(codes)))

# Filtra filas sin código válido y normaliza columnas (NA → "")
codes <- codes %>%
  mutate(seccion = coalesce(as.character(seccion), ""),
         moneda  = coalesce(as.character(moneda), ""),
         regimen = coalesce(as.character(regimen), ""),
         etiqueta = coalesce(as.character(etiqueta), "")) %>%
  filter(!is.na(codigo_sdde),
         codigo_sdde != "",
         !grepl("^POR_", codigo_sdde, ignore.case = TRUE))

message("[02] ", nrow(codes), " series a descargar.")

# Pausa inicial: si el script 01 acabó hace poco, dar tiempo a la
# API a recuperarse de ~2000 llamadas de descubrimiento.
PAUSA_INICIAL <- 15  # segundos
if (PAUSA_INICIAL > 0) {
  message("[02] Pausa inicial de ", PAUSA_INICIAL, "s para evitar rate-limit del SDDE...")
  Sys.sleep(PAUSA_INICIAL)
}

descargar_una <- function(r, attempt_pause = 1.0, verbose = TRUE) {
  tryCatch(
    download_bccr_series(r$codigo_sdde, "2010/01/01",
                          format(Sys.Date(), "%Y/%m/%d"),
                          credentials = creds, verbose = verbose, save_raw = TRUE),
    error = function(e) { message("  ! ", conditionMessage(e)); NULL }
  )
}

build_manifest_row <- function(r, df, archivo = NA_character_, status = "ok") {
  if (status == "ok" && !is.null(df) && nrow(df) > 0) {
    return(tibble(
      bloque = r$bloque, seccion = r$seccion, moneda = r$moneda, regimen = r$regimen,
      codigo_sdde = r$codigo_sdde, etiqueta = r$etiqueta,
      status = "ok", n_obs = nrow(df),
      archivo = archivo,
      min_fecha = as.character(min(df$fecha)),
      max_fecha = as.character(max(df$fecha))
    ))
  }
  tibble(
    bloque = r$bloque, seccion = r$seccion, moneda = r$moneda, regimen = r$regimen,
    codigo_sdde = r$codigo_sdde, etiqueta = r$etiqueta,
    status = status, n_obs = 0L, archivo = NA_character_,
    min_fecha = NA_character_, max_fecha = NA_character_
  )
}

manifest <- tibble()
failed <- list()

# ---- Pasada principal --------------------------------------------
for (i in seq_len(nrow(codes))) {
  r <- codes[i, ]
  key <- paste(r$bloque,
               ifelse(r$seccion == "", "_", r$seccion),
               ifelse(r$moneda  == "", "_", r$moneda),
               ifelse(r$regimen == "", "_", r$regimen),
               sep = "_")
  message(sprintf("[%d/%d] %s code=%s", i, nrow(codes), key, r$codigo_sdde))
  df <- descargar_una(r, verbose = TRUE)
  if (is.null(df) || nrow(df) == 0) {
    failed[[length(failed)+1]] <- list(r = r, key = key)
    manifest <- bind_rows(manifest, build_manifest_row(r, NULL, status = "fail"))
  } else {
    out <- df %>% transmute(fecha = date, valor = value)
    fname <- file.path(RAW, paste0(key, ".csv"))
    write_csv(out, fname)
    manifest <- bind_rows(manifest, build_manifest_row(r, out, archivo = fname, status = "ok"))
  }
  Sys.sleep(1.0)  # más respetuoso con la API
}

# ---- Pasada de reintento (si hay fallidos) -----------------------
if (length(failed) > 0) {
  message(sprintf("\n[02] %d series fallaron en la primera pasada. Reintento tras pausa de 30s...",
                  length(failed)))
  Sys.sleep(30)
  for (i in seq_along(failed)) {
    r   <- failed[[i]]$r
    key <- failed[[i]]$key
    message(sprintf("  retry [%d/%d] %s code=%s", i, length(failed), key, r$codigo_sdde))
    df <- descargar_una(r, verbose = TRUE)
    if (!is.null(df) && nrow(df) > 0) {
      out <- df %>% transmute(fecha = date, valor = value)
      fname <- file.path(RAW, paste0(key, ".csv"))
      write_csv(out, fname)
      # Reemplazar fila correspondiente en manifest
      manifest <- manifest %>%
        filter(!(codigo_sdde == r$codigo_sdde &
                 bloque      == r$bloque &
                 seccion     == r$seccion &
                 moneda      == r$moneda &
                 regimen     == r$regimen)) %>%
        bind_rows(build_manifest_row(r, out, archivo = fname, status = "ok"))
    }
    Sys.sleep(2.0)
  }
}

write_csv(manifest, file.path(OUT, "raw_pinza_manifest.csv"))
ok_n <- sum(manifest$status == "ok")
message(sprintf("\n[02] Listo. %d series OK / %d totales. Manifest en %s",
                ok_n, nrow(manifest),
                file.path(OUT, "raw_pinza_manifest.csv")))
if (ok_n < nrow(manifest)) {
  fails <- manifest %>% filter(status != "ok")
  message("[02] Fallaron: ")
  print(fails %>% select(bloque, seccion, moneda, regimen, codigo_sdde, etiqueta), n = Inf)
}
