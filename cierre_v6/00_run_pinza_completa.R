# ============================================================
# cierre_v6/00_run_pinza_completa.R — corre la pinza en cadena
# ============================================================
# Ejecuta los tres scripts secuencialmente:
#   01_discover_bccr_codes.R       → sondea el SDDE
#                                  → genera codes_seleccionados_propuesto.csv
#                                  → si no existe codes_seleccionados.csv,
#                                    lo crea a partir de la propuesta
#   02_download_pinza_empirica.R   → descarga series seleccionadas
#   03_compute_pinza_empirica.R    → calcula σ_Y, σ_C, cuadrantes,
#                                    tabla sectorial, bloques, régimen
#
# Uso (con el .Rproj abierto):
#   source("cierre_v6/00_run_pinza_completa.R")
#
# Variables que podés ajustar antes de correr:
#   RUN_DISCOVERY  TRUE/FALSE   ¿correr el sondeo? (~6-10 min)
#   RUN_DOWNLOAD   TRUE/FALSE   ¿descargar series?
#   RUN_COMPUTE    TRUE/FALSE   ¿calcular σ y cuadrantes?
#   PAUSAR_REVISION TRUE/FALSE  ¿parar después del 01 para que
#                               revises codes_seleccionados.csv?
# ============================================================

RUN_DISCOVERY   <- FALSE  # IMAE-CIIU ya descubierto; codes_seleccionados.csv fijado a mano
RUN_DOWNLOAD    <- TRUE
RUN_COMPUTE     <- TRUE
PAUSAR_REVISION <- FALSE   # poné TRUE si querés ver/ajustar
                            # codes_seleccionados.csv antes de descargar

stopifnot(file.exists("pipeline_fx_forecast_cr.Rproj"))
message("[run_pinza] working dir: ", getwd())

t0 <- Sys.time()

if (RUN_DISCOVERY) {
  message("\n############################################################")
  message("# Paso 1/3 — discovery")
  message("############################################################")
  source("cierre_v6/01_discover_bccr_codes.R")
} else {
  message("\n[run_pinza] Salteando paso 1 (RUN_DISCOVERY=FALSE).")
}

if (PAUSAR_REVISION) {
  message("\n############################################################")
  message("# Pausa de revisión")
  message("############################################################")
  message("Revisá cierre_v6/codes_seleccionados.csv.")
  message("Si querés ajustarlo, hacelo ahora.")
  ans <- readline("Enter para continuar con la descarga, 'q' para abortar: ")
  if (tolower(trimws(ans)) %in% c("q","quit","abort","no","n")) {
    message("[run_pinza] Abortado por usuario.")
    return(invisible(NULL))
  }
}

if (RUN_DOWNLOAD) {
  message("\n############################################################")
  message("# Paso 2/3 — descarga")
  message("############################################################")
  if (!file.exists("cierre_v6/codes_seleccionados.csv")) {
    stop("Falta cierre_v6/codes_seleccionados.csv. Corré primero el paso 1.")
  }
  source("cierre_v6/02_download_pinza_empirica.R")
} else {
  message("\n[run_pinza] Salteando paso 2 (RUN_DOWNLOAD=FALSE).")
}

if (RUN_COMPUTE) {
  message("\n############################################################")
  message("# Paso 3/3 — cómputo de la pinza")
  message("############################################################")
  if (!file.exists("cierre_v6/outputs/raw_pinza_manifest.csv")) {
    stop("Falta cierre_v6/outputs/raw_pinza_manifest.csv. Corré primero el paso 2.")
  }
  source("cierre_v6/03_compute_pinza_empirica.R")
} else {
  message("\n[run_pinza] Salteando paso 3 (RUN_COMPUTE=FALSE).")
}

t1 <- Sys.time()
message("\n############################################################")
message("[run_pinza] Pinza empírica completa.")
message("           Tiempo total: ", round(as.numeric(difftime(t1, t0, units = "mins")), 1), " min")
message("############################################################")
message("\nMandame los archivos:")
for (f in c("cierre_v6/outputs/discovery_anotado.csv",
            "cierre_v6/outputs/raw_pinza_manifest.csv",
            "cierre_v6/outputs/sigma_Y_observado.csv",
            "cierre_v6/outputs/sigma_C_observado.csv",
            "cierre_v6/outputs/cuadrantes_observados.csv",
            "cierre_v6/outputs/tabla_sectorial_v6.csv",
            "cierre_v6/outputs/indices_bloques_v6.csv",
            "cierre_v6/outputs/bloque_growth_v6.csv",
            "cierre_v6/outputs/regimen_descomposicion.csv",
            "cierre_v6/codes_seleccionados.csv")) {
  message("  ", f)
}
