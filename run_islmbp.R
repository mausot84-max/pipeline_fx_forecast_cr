# ============================================================
# run_islmbp.R — Driver del bloque IS-LM-BP únicamente
# ============================================================
# Asume que el pipeline semanal de forecasting (scripts 00-08)
# ya corrió y que features_monthly.rds está disponible.
#
# Ejecuta los siete scripts del bloque ISLMBP:
#   20_islmbp_data_prep.R
#   21_islmbp_estimate.R
#   22_islmbp_calibrate_shock.R
#   23_islmbp_simulate.R
#   24_islmbp_tables.R
#   25_islmbp_figures.R
#   26_sterilization_test.R  (Prueba 3b: canal de esterilización)
#
# Uso:
#   setwd("ruta/al/pipeline_fx_forecast_cr")
#   source("run_islmbp.R")
# ============================================================

# 1.  Cargar setup (paquetes, utilidades, configs)
source("scripts/00_setup.R")

# 2.  Verificar que el panel mensual del pipeline existe
if (!file.exists("data_intermediate/features/features_monthly.rds")) {
  stop("features_monthly.rds no existe.\n",
       "  Correr primero el pipeline semanal:\n",
       "    for (s in sort(list.files('scripts', '^0[0-9].*\\\\.R$', full.names=TRUE))) source(s)\n",
       "    source('scripts/08_feature_engineering.R')")
}

# 3.  Correr los siete pasos del bloque ISLMBP en orden
islmbp_scripts <- c(
  "scripts/20_islmbp_data_prep.R",
  "scripts/21_islmbp_estimate.R",
  "scripts/22_islmbp_calibrate_shock.R",
  "scripts/23_islmbp_simulate.R",
  "scripts/24_islmbp_tables.R",
  "scripts/25_islmbp_figures.R",
  "scripts/26_sterilization_test.R"
)

start_t <- Sys.time()
for (s in islmbp_scripts) {
  cat("\n=====", s, "=====\n")
  source(s)
}
elapsed <- difftime(Sys.time(), start_t, units = "secs")

cat("\n=====================================================\n")
cat("  ISLMBP block done in", round(as.numeric(elapsed), 1), "secs\n")
cat("=====================================================\n")
cat("  Tablas:   output/tables/paper_tabla*.csv\n")
cat("  Figuras:  output/figures/paper_fig*.png\n")
cat("=====================================================\n")
