# =========================================================================
# 35_cpi_us_deflactor_credito_me.R
# -------------------------------------------------------------------------
# Baja el CPI All Items de EE. UU. desde FRED (CPIAUCSL seasonally adjusted
# y CPIAUCNS not seasonally adjusted, para robustez) y computa la inflación
# acumulada bajo distintas convenciones para deflactar el crédito en
# moneda extranjera del sistema bancario costarricense.
#
# Motivación: en §7.1, segunda lectura, comparamos crédito ME en USD
# (nominal: +26,0 %) contra PIB real CR en colones (real: +17,4 %). Esa
# comparación mezcla nominal-USD con real-CRC. Para hacerla rigurosa hay
# que deflactar también el crédito ME por la inflación estadounidense.
#
# Convenciones reportadas:
#   - dic-2021 vs dic-2025 (cierre vs cierre)
#   - promedio anual 2021 vs promedio anual 2025
#   - últimos doce meses disponibles a 2025
#
# Outputs:
#   data_raw/external/ext_cpiaucsl.csv        — CPIAUCSL (SA) full series
#   data_raw/external/ext_cpiaucns.csv        — CPIAUCNS (NSA) full series
#   output/cpi_us_inflacion_acumulada.csv     — resumen por convención
#   output/credito_me_real_recompute.txt      — recomputo del crédito ME real
# =========================================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(readr)
  library(lubridate); library(tibble)
})

source("scripts/utils_external.R")

stopifnot(!is.null(get_fred_key()))

dir.create("data_raw/external", showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# --- 1. Descargar CPI EE. UU. ------------------------------------------------
message("[35] Descargando CPI EE. UU. desde FRED ...")

cpi_sa  <- download_fred_series("CPIAUCSL", start_date = "2018-01-01")
cpi_nsa <- download_fred_series("CPIAUCNS", start_date = "2018-01-01")

stopifnot(!is.null(cpi_sa), nrow(cpi_sa) > 0)
stopifnot(!is.null(cpi_nsa), nrow(cpi_nsa) > 0)

write_csv(cpi_sa,  "data_raw/external/ext_cpiaucsl.csv")
write_csv(cpi_nsa, "data_raw/external/ext_cpiaucns.csv")

# --- 2. Computar inflación acumulada bajo varias convenciones ---------------
inflation_summary <- function(d, label) {
  d <- d %>% mutate(year = year(date), month = month(date))

  # cierre dic 2021 vs cierre dic 2025
  v_dec21 <- d %>% filter(year == 2021, month == 12) %>% pull(value)
  v_dec25 <- d %>% filter(year == 2025, month == 12) %>% pull(value)

  # promedio anual 2021 vs promedio anual 2025
  avg_21  <- d %>% filter(year == 2021) %>% summarise(m = mean(value, na.rm=TRUE)) %>% pull(m)
  avg_25  <- d %>% filter(year == 2025) %>% summarise(m = mean(value, na.rm=TRUE)) %>% pull(m)

  # último dato disponible (para verificación)
  last_obs <- d %>% slice_tail(n = 1)

  tibble(
    serie          = label,
    last_date      = as.character(last_obs$date),
    cpi_dec21      = if (length(v_dec21)) v_dec21 else NA_real_,
    cpi_dec25      = if (length(v_dec25)) v_dec25 else NA_real_,
    cpi_avg21      = avg_21,
    cpi_avg25      = avg_25,
    infl_cierre_pct = if (length(v_dec21) && length(v_dec25))
                       100 * (v_dec25 / v_dec21 - 1) else NA_real_,
    infl_prom_pct   = 100 * (avg_25 / avg_21 - 1)
  )
}

resumen <- bind_rows(
  inflation_summary(cpi_sa,  "CPIAUCSL (Seasonally Adjusted)"),
  inflation_summary(cpi_nsa, "CPIAUCNS (Not Seasonally Adjusted)")
)

write_csv(resumen, "output/cpi_us_inflacion_acumulada.csv")

cat("\n=== Inflación EE. UU. acumulada 2021 → 2025 (FRED) ===\n")
print(resumen)

# --- 3. Recomputar crédito ME real ------------------------------------------
# Cifras de §7.1 segunda lectura (panel)
cred_me_nom_pct  <- 26.0   # nominal en USD
cred_mn_nom_pct  <- 29.5   # nominal en colones
pib_nom_pct      <- 27.3   # PIB nominal CRC
ipc_cr_acum_pct  <- 8.4    # IPC CR acumulado (paper)

# PIB real CRC (reportado en el paper)
pib_real_pct <- 100 * ((1 + pib_nom_pct/100) / (1 + ipc_cr_acum_pct/100) - 1)

# Cartera MN real CRC (paper)
mn_real_pct  <- 100 * ((1 + cred_mn_nom_pct/100) / (1 + ipc_cr_acum_pct/100) - 1)

# Cartera ME real USD bajo cada convención de CPI EE. UU.
me_real_under <- function(infl_pct) {
  100 * ((1 + cred_me_nom_pct/100) / (1 + infl_pct/100) - 1)
}

# Tres lecturas: SA cierre, SA promedio, NSA cierre (más cercana al IPC CR
# acumulado del paper que se midió con promedio anual del interanual)
me_real_sa_cierre  <- me_real_under(resumen$infl_cierre_pct[1])
me_real_sa_prom    <- me_real_under(resumen$infl_prom_pct[1])
me_real_nsa_cierre <- me_real_under(resumen$infl_cierre_pct[2])
me_real_nsa_prom   <- me_real_under(resumen$infl_prom_pct[2])

# --- 4. Reportar -------------------------------------------------------------
out <- c(
  "=== Recomputación del crédito ME real (deflactado por CPI EE. UU.) ===",
  "",
  sprintf("PIB CRC nominal       : +%.2f %%", pib_nom_pct),
  sprintf("IPC CR acumulado      : +%.2f %% (paper)", ipc_cr_acum_pct),
  sprintf("PIB CRC real          : +%.2f %%  (referencia)", pib_real_pct),
  "",
  sprintf("Cartera MN nominal    : +%.2f %%", cred_mn_nom_pct),
  sprintf("Cartera MN real (CRC) : +%.2f %%", mn_real_pct),
  "",
  sprintf("Cartera ME nominal USD: +%.2f %% (paper)", cred_me_nom_pct),
  "",
  "--- Deflactando por CPI EE. UU. (varias convenciones) ---",
  sprintf("CPIAUCSL  dic21 → dic25 : %.2f %%  → ME real USD = %.2f %%",
          resumen$infl_cierre_pct[1], me_real_sa_cierre),
  sprintf("CPIAUCSL  prom 21 → 25  : %.2f %%  → ME real USD = %.2f %%",
          resumen$infl_prom_pct[1],   me_real_sa_prom),
  sprintf("CPIAUCNS  dic21 → dic25 : %.2f %%  → ME real USD = %.2f %%",
          resumen$infl_cierre_pct[2], me_real_nsa_cierre),
  sprintf("CPIAUCNS  prom 21 → 25  : %.2f %%  → ME real USD = %.2f %%",
          resumen$infl_prom_pct[2],   me_real_nsa_prom),
  "",
  "--- Tabla comparativa final ---",
  sprintf("PIB real CRC          : %+5.2f %%", pib_real_pct),
  sprintf("Cartera MN real CRC   : %+5.2f %%", mn_real_pct),
  sprintf("Cartera ME real USD   : %+5.2f %% (mediana de las 4 convenciones)",
          median(c(me_real_sa_cierre, me_real_sa_prom,
                   me_real_nsa_cierre, me_real_nsa_prom))),
  ""
)

writeLines(out, "output/credito_me_real_recompute.txt")
cat("\n", paste(out, collapse = "\n"), "\n", sep = "")

message("[35] OK — outputs en output/cpi_us_inflacion_acumulada.csv y credito_me_real_recompute.txt")
