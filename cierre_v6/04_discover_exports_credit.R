# ============================================================
# cierre_v6/04_discover_exports_credit.R
# ============================================================
# Sondeo dirigido para descubrir códigos SDDE de:
#   - Cuadro 82 BCCR: exportaciones FOB por actividad económica
#   - Cuadro 144 BCCR: crédito del sistema bancario por
#                       actividad económica y moneda
#
# Estrategia: barrer rangos plausibles del SDDE alrededor de
# códigos ya verificados y de cuadros temáticamente cercanos,
# con pausa generosa (1.5 s) para evitar rate-limit.
#
# Códigos verificados como ancla:
#   33    Exportaciones FOB total (USD millones)
#   159   Importaciones CIF total (USD millones)
#   4814  Crédito sector privado en colones (agregado)
#   28746 Términos de intercambio
#
# Uso (con el .Rproj abierto):
#   source("cierre_v6/04_discover_exports_credit.R")
#
# Salida:
#   cierre_v6/outputs/discovery_exports_credit.csv
#   cierre_v6/outputs/discovery_exports_credit_filtrado.csv
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(readr); library(tibble); library(stringr)
})

if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

TOKEN <- creds$token
BASE  <- BCCR_API_BASE

PAUSE_BASE <- 1.5         # s entre llamadas
BACKOFFS    <- c(30, 60, 120)

safe_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  v <- suppressWarnings(as.character(x)[1])
  if (is.na(v) || identical(v, "")) return(NA_character_)
  v
}
safe_int <- function(x, default = 0L) {
  if (is.null(x) || length(x) == 0) return(as.integer(default))
  v <- suppressWarnings(as.integer(x)[1])
  if (is.na(v)) return(as.integer(default))
  v
}
mk_row <- function(code, status, name = NA, periodicidad = NA, n = 0) {
  list(code = safe_int(code), status = safe_chr(status),
       name = safe_chr(name), periodicidad = safe_chr(periodicidad),
       n = safe_int(n))
}

probe <- function(code, start = "2024/01/01", end = "2024/03/31",
                  pause = PAUSE_BASE, verbose = FALSE) {
  url <- sprintf("%s/indicadoresEconomicos/%s/series?fechaInicio=%s&fechaFin=%s&idioma=es",
                 BASE, code, URLencode(start), URLencode(end))
  do_get <- function() tryCatch(
    httr::GET(url, httr::add_headers(Authorization = paste("Bearer", TOKEN),
                                      `Content-Type` = "application/json"),
              httr::timeout(30)),
    error = function(e) NULL
  )
  res <- do_get()
  if (!is.null(res) && httr::status_code(res) == 429) {
    for (b in BACKOFFS) {
      if (verbose) message(sprintf("  [rate-limit] code %s — esperando %ds...", code, b))
      Sys.sleep(b); res <- do_get()
      if (is.null(res) || httr::status_code(res) != 429) break
    }
  }
  Sys.sleep(pause)
  if (is.null(res)) return(mk_row(code, "network_error"))
  sc <- httr::status_code(res)
  if (sc != 200) return(mk_row(code, paste0("http_", sc)))
  parsed <- tryCatch(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                        simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || isFALSE(parsed$estado)) return(mk_row(code, "api_false"))
  datos <- parsed$datos
  if (is.null(datos) || length(datos) == 0) return(mk_row(code, "no_data"))
  pick <- function(field) {
    val <- if (is.data.frame(datos)) datos[[field]][1] else datos[[1]][[field]]
    safe_chr(val)
  }
  series <- if (is.data.frame(datos)) datos$series[[1]] else datos[[1]]$series
  n_obs <- if (is.null(series)) 0L else as.integer(NROW(series))
  mk_row(code, "ok", name = pick("nombreIndicador"),
         periodicidad = pick("periodicidad"), n = n_obs)
}

rows_to_tibble <- function(rows) {
  if (length(rows) == 0) return(tibble(code = integer(), status = character(),
                                        name = character(), periodicidad = character(),
                                        n = integer()))
  tibble(
    code         = vapply(rows, function(x) safe_int(x$code),       integer(1)),
    status       = vapply(rows, function(x) safe_chr(x$status),     character(1)),
    name         = vapply(rows, function(x) safe_chr(x$name),       character(1)),
    periodicidad = vapply(rows, function(x) safe_chr(x$periodicidad), character(1)),
    n            = vapply(rows, function(x) safe_int(x$n),          integer(1))
  )
}

# ------------ rangos a sondear ----------------------------------
# La estrategia: 4 bandas. Total ~1100 códigos a 1.5s ≈ 28 min.
#
# Banda 1: cerca de 33 (exports total) y 159 (imports), por si las
#          desagregaciones por actividad están en el rango bajo.
# Banda 2: rango 4400-5600, vecino de 4814 (credit_col) y 4815
#          (credit_usd). Probable hogar de crédito por actividad.
# Banda 3: 10000-12500, donde típicamente viven series del sistema
#          bancario nacional (SBN) en el SDDE.
# Banda 4: 28000-30000, cerca de 28746 (ToT), por si exports por
#          actividad económica conviven en bloques temáticos.

codes_to_probe <- unique(c(
  30:300,           # exports/imports range — 271 códigos
  4400:5600,        # credit range — 1201 códigos
  10000:11500,      # sistema bancario — 1501 códigos
  28000:28900       # ToT vecinos — 901 códigos
))
# Suprime los códigos ya verificados o ya conocidos para no gastar
codes_to_probe <- setdiff(codes_to_probe,
                          c(33, 159, 318, 317, 423, 1, 4814, 28746))

message(sprintf("[discover-82-144] %d códigos a sondear, pausa %.1fs",
                length(codes_to_probe), PAUSE_BASE))
message(sprintf("[discover-82-144] tiempo estimado: %.0f min",
                length(codes_to_probe) * PAUSE_BASE / 60))

out <- vector("list", length(codes_to_probe))
n_429 <- 0L
for (i in seq_along(codes_to_probe)) {
  out[[i]] <- probe(codes_to_probe[i], verbose = TRUE)
  if (out[[i]]$status == "http_429") n_429 <- n_429 + 1L
  if (i %% 50 == 0) {
    ok_n <- sum(vapply(out[seq_len(i)],
                        function(x) isTRUE(x$status == "ok"), logical(1)))
    message(sprintf("  progreso: %d/%d (ok=%d, 429=%d)",
                    i, length(codes_to_probe), ok_n, n_429))
  }
}
df <- rows_to_tibble(out)

# Pasada de reintento sobre 429
rate_limited <- df %>% filter(status == "http_429") %>% pull(code)
if (length(rate_limited) > 0) {
  message(sprintf("[discover-82-144] reintentando %d códigos en 429 tras 60s...",
                  length(rate_limited)))
  Sys.sleep(60)
  retries <- lapply(rate_limited, function(cc) probe(cc, pause = 2.0, verbose = TRUE))
  retries_df <- rows_to_tibble(retries)
  df <- df %>% filter(!code %in% rate_limited) %>% bind_rows(retries_df)
}

write_csv(df, file.path(OUT, "discovery_exports_credit.csv"))
message(sprintf("[discover-82-144] guardado: %s (%d filas ok)",
                file.path(OUT, "discovery_exports_credit.csv"),
                sum(df$status == "ok", na.rm = TRUE)))

# ------------ filtrado y propuesta ------------------------------
ciiu_hints <- tribble(
  ~seccion, ~patron,
  "A",  "agropecuari|agricultura|silvicultura|pesca|caf.|banano|pi.a|ca.a",
  "B",  "mina|cantera",
  "C",  "manufactur|industria|fabric",
  "F",  "construcc|edificac",
  "G",  "comercio",
  "H",  "transport|almacen",
  "I",  "alojam|comida|hoteler|turism|restaur",
  "J",  "informaci.n y comunicaci|telecom|tic",
  "K",  "financier|seguros|banca",
  "L",  "inmobiliar",
  "MN", "profesional|cient.f|t.cnic|administ|apoyo|consultor"
)

ann <- df %>%
  filter(status == "ok") %>%
  mutate(
    es_export = grepl("exporta|fob|venta(s)? al exterior", name, ignore.case = TRUE),
    es_credit = grepl("cr.dito|saldo|cartera|colocaci.n|pr.stamo", name, ignore.case = TRUE),
    es_moneda_usd = grepl("d.lar|usd|moneda extranjera|mon\\. ext", name, ignore.case = TRUE),
    es_moneda_crc = grepl("colones|crc|moneda nacional|mon\\. nac", name, ignore.case = TRUE),
    es_por_actividad = grepl("actividad|sector|rama|ciiu", name, ignore.case = TRUE),
    ciiu_sugerido = NA_character_
  )
for (i in seq_len(nrow(ciiu_hints))) {
  m <- grepl(ciiu_hints$patron[i], ann$name, ignore.case = TRUE)
  ann$ciiu_sugerido[m & is.na(ann$ciiu_sugerido)] <- ciiu_hints$seccion[i]
}

filtrado <- ann %>%
  filter(es_export | es_credit | es_por_actividad | !is.na(ciiu_sugerido)) %>%
  arrange(desc(es_export), desc(es_credit), code)

write_csv(filtrado, file.path(OUT, "discovery_exports_credit_filtrado.csv"))

# ------------ resumen en consola --------------------------------
message("\n========================================================")
message("CANDIDATOS EXPORTACIONES con etiqueta de actividad")
message("========================================================")
print(filtrado %>% filter(es_export) %>%
        select(code, name, periodicidad, n, ciiu_sugerido) %>% head(40),
      n = Inf)

message("\n========================================================")
message("CANDIDATOS CRÉDITO con etiqueta de actividad o moneda")
message("========================================================")
print(filtrado %>% filter(es_credit) %>%
        select(code, name, periodicidad, n, es_moneda_usd, es_moneda_crc, ciiu_sugerido) %>%
        head(60),
      n = Inf)

message("\n========================================================")
message("CANDIDATOS POR PATRÓN DE ACTIVIDAD (sin filtro tipo)")
message("========================================================")
print(filtrado %>% filter(!is.na(ciiu_sugerido), !es_export, !es_credit) %>%
        select(code, name, ciiu_sugerido, periodicidad, n) %>% head(30),
      n = Inf)

message("\n[04] Salidas:")
message("  cierre_v6/outputs/discovery_exports_credit.csv          (log completo)")
message("  cierre_v6/outputs/discovery_exports_credit_filtrado.csv (candidatos)")
message("")
message("Próximo paso: abrir el _filtrado.csv, identificar los códigos de")
message("exportaciones por actividad (cuadro 82) y crédito por actividad x")
message("moneda (cuadro 144), y agregarlos a cierre_v6/codes_seleccionados.csv")
message("con bloque=exports o bloque=credit. Después correr los scripts 02 y 03.")
