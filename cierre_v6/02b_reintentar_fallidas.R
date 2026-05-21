# ============================================================
# cierre_v6/02b_reintentar_fallidas.R
# ============================================================
# Reintenta únicamente las series marcadas como "fail" en
# raw_pinza_manifest.csv, con pausas más conservadoras para
# evitar el rate-limit del SDDE.
#
# Estrategia:
#   - Pausa inicial larga (60 s) para enfriar la API.
#   - Entre llamadas, 6 s.
#   - Hasta 3 pasadas; entre pasadas, 90 s.
#   - Cada serie que recupera se sobrescribe en el manifest
#     (status = "ok") y se escribe el CSV en outputs/raw/.
#   - Las que siguen fallando quedan con status = "fail" y
#     un campo `intentos_extra` en el manifest.
#
# Uso (con el .Rproj pipeline_fx_forecast_cr abierto):
#   source("cierre_v6/02b_reintentar_fallidas.R")
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(lubridate)
})

if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
RAW <- file.path(OUT, "raw")
MANIFEST_FILE <- file.path(OUT, "raw_pinza_manifest.csv")

if (!file.exists(MANIFEST_FILE)) {
  stop("[02b] No existe ", MANIFEST_FILE,
       ". Corré primero cierre_v6/02_download_pinza_empirica.R.")
}

manifest <- read_csv(MANIFEST_FILE, show_col_types = FALSE,
                     col_types = cols(.default = col_character(),
                                      n_obs = col_integer()))

fallidas <- manifest %>% filter(status != "ok")
n_fail <- nrow(fallidas)

if (n_fail == 0) {
  message("[02b] No hay series fallidas en el manifest. Nada que reintentar.")
  return(invisible(NULL))
}

message(sprintf("[02b] %d series fallidas detectadas:", n_fail))
print(fallidas %>% select(bloque, seccion, moneda, regimen, codigo_sdde, etiqueta), n = Inf)

# ------ Parámetros de cortesía con la API --------------------
PAUSA_INICIAL   <- 60   # s antes de la primera pasada
PAUSA_ENTRE     <-  6   # s entre llamadas
PAUSA_PASADA    <- 90   # s entre pasadas consecutivas
MAX_PASADAS     <-  3

descargar_una <- function(r, verbose = TRUE) {
  tryCatch(
    download_bccr_series(r$codigo_sdde, "2010/01/01",
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

intentos_extra <- setNames(integer(n_fail), fallidas$codigo_sdde)
recuperadas    <- character(0)

message(sprintf("\n[02b] Pausa inicial de %d s para enfriar la API...", PAUSA_INICIAL))
Sys.sleep(PAUSA_INICIAL)

pendientes <- fallidas

for (pasada in seq_len(MAX_PASADAS)) {
  if (nrow(pendientes) == 0) break
  message(sprintf("\n[02b] === Pasada %d/%d (%d pendientes) ===",
                  pasada, MAX_PASADAS, nrow(pendientes)))

  nuevas_pendientes <- pendientes[0, ]

  for (i in seq_len(nrow(pendientes))) {
    r   <- pendientes[i, ]
    key <- build_key(r)
    intentos_extra[r$codigo_sdde] <- intentos_extra[r$codigo_sdde] + 1L

    message(sprintf("  [pasada %d | %d/%d] %s code=%s (intento extra %d)",
                    pasada, i, nrow(pendientes), key, r$codigo_sdde,
                    intentos_extra[r$codigo_sdde]))

    df <- descargar_una(r, verbose = TRUE)

    if (!is.null(df) && nrow(df) > 0) {
      out   <- df %>% transmute(fecha = date, valor = value)
      fname <- file.path(RAW, paste0(key, ".csv"))
      write_csv(out, fname)

      manifest <- manifest %>%
        filter(!(codigo_sdde == r$codigo_sdde &
                 bloque      == r$bloque &
                 (seccion %in% c(r$seccion, NA) | is.na(seccion)) &
                 (moneda  %in% c(r$moneda,  NA) | is.na(moneda))  &
                 (regimen %in% c(r$regimen, NA) | is.na(regimen)))) %>%
        bind_rows(tibble(
          bloque      = r$bloque,
          seccion     = r$seccion,
          moneda      = r$moneda,
          regimen     = r$regimen,
          codigo_sdde = r$codigo_sdde,
          etiqueta    = r$etiqueta,
          status      = "ok",
          n_obs       = nrow(out),
          archivo     = fname,
          min_fecha   = as.character(min(out$fecha)),
          max_fecha   = as.character(max(out$fecha))
        ))

      recuperadas <- c(recuperadas, r$codigo_sdde)
      message(sprintf("    OK (%d obs)", nrow(out)))
    } else {
      nuevas_pendientes <- bind_rows(nuevas_pendientes, r)
      message("    seguía fallando")
    }

    Sys.sleep(PAUSA_ENTRE)
  }

  pendientes <- nuevas_pendientes
  if (nrow(pendientes) > 0 && pasada < MAX_PASADAS) {
    message(sprintf("\n[02b] Esperando %d s antes de la próxima pasada...",
                    PAUSA_PASADA))
    Sys.sleep(PAUSA_PASADA)
  }
}

# Adjuntar columna intentos_extra (sólo informativa)
manifest <- manifest %>%
  mutate(intentos_extra = as.integer(intentos_extra[codigo_sdde]),
         intentos_extra = ifelse(is.na(intentos_extra), 0L, intentos_extra))

write_csv(manifest, MANIFEST_FILE)

message(sprintf(
  "\n[02b] Listo. Recuperadas: %d / %d. Manifest actualizado en %s",
  length(recuperadas), n_fail, MANIFEST_FILE))

if (length(recuperadas) > 0) {
  message("[02b] Recuperadas: ", paste(recuperadas, collapse = ", "))
}

still_fail <- manifest %>% filter(status != "ok")
if (nrow(still_fail) > 0) {
  message("\n[02b] Siguen fallando tras ", MAX_PASADAS, " pasadas:")
  print(still_fail %>%
          select(bloque, seccion, moneda, regimen, codigo_sdde, etiqueta,
                 intentos_extra),
        n = Inf)
  message("\n  Sugerencia: si tras este reintento siguen fallando,",
          "\n  validar manualmente los códigos en sdd.bccr.fi.cr",
          "\n  (puede que estén descontinuados o renombrados).")
} else {
  message("\n[02b] Todas las series quedaron OK. Listo para correr el 10.")
}
