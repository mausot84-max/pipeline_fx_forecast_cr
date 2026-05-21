# ============================================================
# 01_validate_credentials.R — BCCR SDDE + FRED validation
# ============================================================

log_msg("=== 01: Validating credentials ===")

# ------ BCCR (SDDE new API) -----------------------------------
bccr_ok <- tryCatch({
  creds <- get_bccr_credentials()
  log_msg(paste("BCCR email:", creds$email))
  ok <- validate_bccr_credentials(creds)
  log_msg(paste("BCCR SDDE test:", if(ok) "OK" else "FAILED"))
  ok
}, error = function(e) {
  log_msg(paste("BCCR error:", e$message), "ERROR")
  FALSE
})

# ------ FRED --------------------------------------------------
fred_ok <- tryCatch({
  ok <- validate_fred_credentials()
  log_msg(paste("FRED test:", if(ok) "OK" else "FAILED"))
  ok
}, error = function(e) {
  log_msg(paste("FRED error:", e$message), "WARN")
  FALSE
})

# ------ SUMMARY -----------------------------------------------
CREDENTIALS_STATUS <- list(bccr = bccr_ok, fred = fred_ok)

if (!bccr_ok)
  warning("BCCR SDDE credentials invalid — local series will be skipped.\n",
          "Get your JWT token from: https://sdd.bccr.fi.cr")
if (!fred_ok)
  log_msg("FRED unavailable — FRED series will be skipped.", "WARN")

log_msg(paste("Credentials: BCCR=", bccr_ok, " FRED=", fred_ok))
log_msg("=== 01 done ===")
