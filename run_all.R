# ============================================================
# run_all.R — Driver del pipeline completo
# ============================================================
# Corre TODO el pipeline de principio a fin:
#   - Bloque semanal de forecasting (scripts 00–18)
#   - Bloque ISLMBP cuatro cuadrantes (scripts 20–25)
#
# Uso desde RStudio:
#   - Abrir pipeline_fx_forecast_cr.Rproj
#   - source("run_all.R")
#
# Uso desde terminal:
#   cd /ruta/al/pipeline_fx_forecast_cr
#   Rscript run_all.R
#
# Prerequisitos:
#   - R >= 4.2
#   - Archivo .Renviron con BCCR_SDDE_USER, BCCR_SDDE_TOKEN, FRED_API_KEY
#   - Paquetes instalados (00_setup.R los chequea/instala)
# ============================================================

# --- 0. Sanity check: estar en la raíz del proyecto -----------
if (!file.exists("pipeline_fx_forecast_cr.Rproj")) {
  stop("run_all.R debe correrse desde la raiz del proyecto.\n",
       "  Working directory actual: ", getwd(), "\n",
       "  Hace setwd() a la carpeta que contiene pipeline_fx_forecast_cr.Rproj")
}

# --- 1. Descubrir scripts numerados en orden ------------------
all_scripts <- sort(list.files(
  path       = "scripts",
  pattern    = "^[0-9]+_.*\\.R$",
  full.names = TRUE
))

# Excluir utils_*.R (no se sourcean directo, los carga 00_setup.R)
all_scripts <- all_scripts[!grepl("/utils_", all_scripts)]

cat("=====================================================\n")
cat("  Pipeline completo: ", length(all_scripts), "scripts a correr\n")
cat("=====================================================\n")
for (s in all_scripts) cat("  -", s, "\n")
cat("\n")

# --- 2. Runner con timing y manejo de errores ------------------
run_one <- function(script_path) {
  cat("\n=====", script_path, "=====\n")
  t0 <- Sys.time()
  ok <- tryCatch({
    source(script_path, local = FALSE, echo = FALSE)
    TRUE
  }, error = function(e) {
    cat("  ERROR en", script_path, ":\n  ", conditionMessage(e), "\n")
    FALSE
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat("  -> ", ifelse(ok, "OK", "FALLO"), " (", dt, "s)\n", sep = "")
  list(script = script_path, ok = ok, secs = dt)
}

# --- 3. Ejecutar todo -----------------------------------------
start_t <- Sys.time()
results <- lapply(all_scripts, run_one)
elapsed <- difftime(Sys.time(), start_t, units = "mins")

# --- 4. Resumen final -----------------------------------------
cat("\n=====================================================\n")
cat("  PIPELINE COMPLETO terminado en ",
    round(as.numeric(elapsed), 2), " minutos\n", sep = "")
cat("=====================================================\n")

res_df <- do.call(rbind, lapply(results, as.data.frame))
print(res_df, row.names = FALSE)

n_fail <- sum(!sapply(results, `[[`, "ok"))
if (n_fail > 0) {
  cat("\n  ", n_fail, " script(s) fallaron. Revisa los logs arriba.\n", sep = "")
} else {
  cat("\n  Todos los scripts corrieron sin error.\n")
  cat("  Tablas:  output/tables/\n")
  cat("  Figuras: output/figures/\n")
  cat("  Logs:    output/logs/\n")
}
cat("=====================================================\n")
