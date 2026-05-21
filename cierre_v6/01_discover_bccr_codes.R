# ============================================================
# cierre_v6/01_discover_bccr_codes.R
# ============================================================
# Sondea rangos del SDDE-BCCR para descubrir los códigos correctos
# del bloque sectorial (IMAE-CIIU, exportaciones por actividad,
# crédito por actividad x moneda, IMAE por régimen).
#
# Convención: este script se ejecuta desde el R project
# `pipeline_fx_forecast_cr`. Reutiliza utils_bccr.R (función
# `download_bccr_series`) para mantener cache y credenciales
# consistentes con el resto del pipeline.
#
# Uso (con el .Rproj abierto):
#   source("cierre_v6/01_discover_bccr_codes.R")
# o desde terminal:
#   Rscript cierre_v6/01_discover_bccr_codes.R
#
# Salida en cierre_v6/outputs/.
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(readr); library(tibble); library(stringr)
})

# Carga utils del pipeline (asume que el wd es la raíz del proyecto)
if (!exists("download_bccr_series")) source("scripts/utils_bccr.R")

# Carga credenciales — utils_bccr.R define get_bccr_credentials()
creds <- get_bccr_credentials()

OUT <- "cierre_v6/outputs"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

BASE  <- BCCR_API_BASE
TOKEN <- creds$token

# ------------ función de sondeo con manejo de rate-limit --------
# El SDDE devuelve HTTP 429 cuando saturamos. Estrategia:
#   - Pausa base de 1.0 segundo entre llamadas.
#   - Si aparece 429 → backoff exponencial (30s, 60s, 120s).
#   - Si la segunda y tercera espera también dan 429, marcamos
#     la fila y seguimos. Una pasada de reintento final cubre
#     los rezagados.

PAUSE_BASE <- 1.0   # segundos entre llamadas normales
BACKOFFS    <- c(30, 60, 120)

# Helper: garantizar que cada campo tenga longitud 1 (no character(0),
# no NULL, no NA suelto). bind_rows() descarta silenciosamente
# filas con campos de longitud cero o tipos inconsistentes.
safe_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  v <- suppressWarnings(as.character(x)[1])
  if (is.na(v)) return(NA_character_)
  if (identical(v, "")) return(NA_character_)
  v
}
safe_int <- function(x, default = 0L) {
  if (is.null(x)) return(as.integer(default))
  if (length(x) == 0) return(as.integer(default))
  v <- suppressWarnings(as.integer(x)[1])
  if (is.na(v)) return(as.integer(default))
  v
}
mk_row <- function(code, status, name = NA, periodicidad = NA, n = 0) {
  list(
    code         = safe_int(code),
    status       = safe_chr(status),
    name         = safe_chr(name),
    periodicidad = safe_chr(periodicidad),
    n            = safe_int(n)
  )
}

probe <- function(code, start = "2024/01/01", end = "2024/03/31",
                  pause = PAUSE_BASE, verbose = FALSE) {
  url <- sprintf("%s/indicadoresEconomicos/%s/series?fechaInicio=%s&fechaFin=%s&idioma=es",
                 BASE, code, URLencode(start), URLencode(end))
  do_get <- function() tryCatch(
    httr::GET(url,
              httr::add_headers(Authorization = paste("Bearer", TOKEN),
                                `Content-Type` = "application/json"),
              httr::timeout(30)),
    error = function(e) NULL
  )

  res <- do_get()

  # Manejar HTTP 429 con backoff
  if (!is.null(res) && httr::status_code(res) == 429) {
    for (b in BACKOFFS) {
      if (verbose) message(sprintf("  [rate-limit] code %s — esperando %ds...", code, b))
      Sys.sleep(b)
      res <- do_get()
      if (is.null(res) || httr::status_code(res) != 429) break
    }
  }

  Sys.sleep(pause)

  if (is.null(res)) return(mk_row(code, "network_error"))
  sc <- httr::status_code(res)
  if (sc != 200) return(mk_row(code, paste0("http_", sc)))

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE),
    error = function(e) NULL
  )
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

# Convierte una lista de "rows" (creadas con mk_row) a tibble. Usa
# vapply para forzar tipos y evitar el bug de bind_rows con campos
# de longitud cero o tipos mixtos.
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

probe_range <- function(codes, label, save_path) {
  message(sprintf("\n[discover] %s — sondeando %d códigos (pause=%.2fs)",
                  label, length(codes), PAUSE_BASE))
  out <- vector("list", length(codes))
  n_429 <- 0L
  for (i in seq_along(codes)) {
    out[[i]] <- probe(codes[i], verbose = TRUE)
    if (out[[i]]$status == "http_429") n_429 <- n_429 + 1L
    if (i %% 25 == 0) {
      ok_n <- sum(vapply(out[seq_len(i)], function(x) isTRUE(x$status == "ok"), logical(1)))
      message(sprintf("  %s progreso: %d/%d (ok=%d, 429=%d)", label, i, length(codes), ok_n, n_429))
    }
  }
  df <- rows_to_tibble(out) %>% mutate(bloque_sondeo = label)
  message(sprintf("  %s post-conversion: nrow(df)=%d, ok=%d, 429=%d",
                  label, nrow(df), sum(df$status == "ok", na.rm = TRUE),
                  sum(df$status == "http_429", na.rm = TRUE)))

  # Pasada de reintento sobre 429 si quedaron
  rate_limited <- df %>% filter(status == "http_429") %>% pull(code)
  if (length(rate_limited) > 0) {
    message(sprintf("[discover] %s — reintentando %d códigos con 429 tras pausa de 60s...",
                    label, length(rate_limited)))
    Sys.sleep(60)
    retries <- lapply(rate_limited, function(cc) probe(cc, pause = 2.0, verbose = TRUE))
    retries_df <- rows_to_tibble(retries) %>% mutate(bloque_sondeo = label)
    df <- df %>% filter(!code %in% rate_limited) %>% bind_rows(retries_df)
  }

  write_csv(df, save_path)
  message(sprintf("[discover] %s — guardado: %s (%d filas ok)", label, save_path, sum(df$status == "ok", na.rm = TRUE)))
  invisible(df)
}

# ------------ rangos a sondear ----------------------------------
# Rangos conservadores. Con PAUSE_BASE=1s, el discovery completo
# tarda ~30 min. Si necesitás extender un rango, editá acá.
# Códigos verificados como ancla:
#   95087 IMAE general (rango 95xxx); 95141 Comercio (mismo rango).
#   28746 Términos de intercambio (rango 28xxx).
#   4814 Crédito sector privado en colones (rango 4xxx).
#
# Nota: el IMAE Régimen Definitivo y Régimen Especial vive en
# el rango 95xxx (mismo del IMAE general por sección). El sondeo
# IMAE-CIIU lo captura por nombre. El bloque IMAE-REGIME se deja
# vacío por default; ponelo si querés probar otros rangos.

imae_codes   <- 95000:95250
regime_codes <- integer(0)   # capturado dentro de IMAE-CIIU
export_codes <- unique(c(25500:26000, 4900:5050, 31000:31100))
credit_codes <- unique(c(4500:5050, 11800:12100, 26000:26200))

# ------------ ejecutar -------------------------------------------
res <- list()
res$imae    <- probe_range(imae_codes,    "IMAE-CIIU",    file.path(OUT, "discovery_imae_ciiu.csv"))
res$regime  <- probe_range(regime_codes,  "IMAE-REGIME",  file.path(OUT, "discovery_regime.csv"))
res$exports <- probe_range(export_codes,  "EXPORTS",      file.path(OUT, "discovery_exports.csv"))
res$credit  <- probe_range(credit_codes,  "CREDIT",       file.path(OUT, "discovery_credit.csv"))

full <- bind_rows(res)
write_csv(full, file.path(OUT, "discovery_full_log.csv"))

# ------------ anotación CIIU / régimen / moneda -----------------
ciiu_hints <- tribble(
  ~seccion, ~patron,
  "A",  "Agricultura|silvicultura|pesca|agropecuari",
  "B",  "Mina|cantera",
  "C",  "manufactur",
  "DE", "Electricidad|agua|saneamiento",
  "F",  "Construcc",
  "G",  "Comercio",
  "H",  "Transport|Almacenam",
  "I",  "Alojam|comida",
  "J",  "Informaci.n y comunicaci",
  "K",  "Financier|seguros",
  "L",  "Inmobiliar",
  "MN", "profesional|cient|t.cnic|administ|apoyo",
  "O",  "Administraci.n p.blica|seguridad social",
  "PQ", "Ense.an|salud",
  "RS", "Otras actividades"
)
regime_hints <- tribble(
  ~regimen, ~patron,
  "DEFINITIVO", "definitivo|r.gimen definitivo",
  "ESPECIAL",   "especial|zonas? francas?|perfeccionamiento|reexportaci"
)

ann <- full %>% filter(status == "ok") %>%
  mutate(
    es_nivel = grepl("\\bNivel\\b|\\. Nivel\\b", name, ignore.case = TRUE),
    es_var_mensual = grepl("Variaci.n mensual", name, ignore.case = TRUE),
    es_var_interanual = grepl("Variaci.n interanual", name, ignore.case = TRUE),
    en_moneda_usd = grepl("USD|d.lar|dolar", name, ignore.case = TRUE),
    en_moneda_crc = grepl("colones|CRC|moneda nacional", name, ignore.case = TRUE),
    ciiu_sugerido = NA_character_, regimen_sugerido = NA_character_
  )
for (i in seq_len(nrow(ciiu_hints))) {
  m <- grepl(ciiu_hints$patron[i], ann$name, ignore.case = TRUE)
  ann$ciiu_sugerido[m & is.na(ann$ciiu_sugerido)] <- ciiu_hints$seccion[i]
}
for (i in seq_len(nrow(regime_hints))) {
  m <- grepl(regime_hints$patron[i], ann$name, ignore.case = TRUE)
  ann$regimen_sugerido[m & is.na(ann$regimen_sugerido)] <- regime_hints$regimen[i]
}
write_csv(ann, file.path(OUT, "discovery_anotado.csv"))

# Resúmenes en consola
message("\n========================================================")
message("CANDIDATOS IMAE por sección CIIU (niveles)")
message("========================================================")
print(ann %>% filter(es_nivel, !is.na(ciiu_sugerido)) %>%
        group_by(ciiu_sugerido) %>% slice_head(n = 3) %>%
        select(code, ciiu_sugerido, name, periodicidad, n) %>% arrange(ciiu_sugerido, code), n = Inf)

message("\n========================================================")
message("CANDIDATOS por régimen (definitivo / especial)")
message("========================================================")
print(ann %>% filter(!is.na(regimen_sugerido)) %>% slice_head(n = 30) %>%
        select(code, regimen_sugerido, name, periodicidad, n), n = Inf)

message("\n========================================================")
message("CANDIDATOS exportaciones con sección CIIU")
message("========================================================")
print(ann %>% filter(grepl("xporta", name, ignore.case = TRUE), !is.na(ciiu_sugerido)) %>%
        select(code, ciiu_sugerido, name, periodicidad, n) %>% arrange(ciiu_sugerido, code), n = Inf)

message("\n========================================================")
message("CANDIDATOS crédito con sección CIIU y moneda")
message("========================================================")
print(ann %>% filter(grepl("cr.dito|cartera|saldo", name, ignore.case = TRUE), !is.na(ciiu_sugerido)) %>%
        select(code, ciiu_sugerido, name, en_moneda_usd, en_moneda_crc, periodicidad, n) %>% arrange(ciiu_sugerido, code), n = Inf)

# ------------ propuesta automática de codes_seleccionados -------
# Heurística: por cada (bloque, sección, moneda, régimen), tomar
# el código con mayor n_obs. Si hay empate, el código menor.

pick_best <- function(df) {
  if (nrow(df) == 0) return(NA_character_)
  df %>% arrange(desc(n), code) %>% slice(1) %>% pull(code) %>% as.character()
}

SECCIONES <- c("A","C","F","G","H","I","J","K","L","MN")

prop <- list()

# 1) IMAE general (fijo)
prop[[length(prop)+1]] <- tibble(bloque = "imae_ciiu", seccion = "TOTAL",
                                  moneda = "", regimen = "",
                                  codigo_sdde = "95087",
                                  etiqueta = "IMAE general (verificado)")

# 2) IMAE por sección — preferir "Nivel" mensual
for (s in SECCIONES) {
  cand <- ann %>% filter(ciiu_sugerido == s,
                          es_nivel,
                          grepl("ensual", periodicidad, ignore.case = TRUE) | is.na(periodicidad))
  code <- if (s == "G") "95141" else pick_best(cand)  # Comercio verificado
  if (!is.na(code)) {
    nm <- ann %>% filter(code == !!code) %>% pull(name) %>% .[1]
    prop[[length(prop)+1]] <- tibble(bloque = "imae_ciiu", seccion = s,
                                      moneda = "", regimen = "",
                                      codigo_sdde = code,
                                      etiqueta = nm %||% paste0("IMAE ", s))
  }
}

# 3) IMAE por régimen
for (rg in c("DEFINITIVO","ESPECIAL")) {
  cand <- ann %>% filter(regimen_sugerido == rg,
                          es_nivel | grepl("IMAE", name, ignore.case = TRUE))
  code <- pick_best(cand)
  if (!is.na(code)) {
    nm <- ann %>% filter(code == !!code) %>% pull(name) %>% .[1]
    prop[[length(prop)+1]] <- tibble(bloque = "imae_regimen", seccion = "",
                                      moneda = "", regimen = rg,
                                      codigo_sdde = code,
                                      etiqueta = nm %||% paste0("IMAE regimen ", rg))
  }
}

# 4) Exportaciones por sección
for (s in SECCIONES) {
  cand <- ann %>% filter(ciiu_sugerido == s,
                          grepl("xporta", name, ignore.case = TRUE))
  code <- pick_best(cand)
  if (!is.na(code)) {
    nm <- ann %>% filter(code == !!code) %>% pull(name) %>% .[1]
    prop[[length(prop)+1]] <- tibble(bloque = "exports", seccion = s,
                                      moneda = "", regimen = "",
                                      codigo_sdde = code,
                                      etiqueta = nm %||% paste0("Exportaciones ", s))
  }
}

# 5) Crédito por sección × moneda
for (s in SECCIONES) {
  for (mon in c("USD","CRC")) {
    pat_moneda <- if (mon == "USD") "USD|d.lar|dolar" else "colones|CRC|moneda nacional"
    cand <- ann %>% filter(ciiu_sugerido == s,
                            grepl("cr.dito|cartera|saldo", name, ignore.case = TRUE),
                            grepl(pat_moneda, name, ignore.case = TRUE))
    code <- pick_best(cand)
    if (!is.na(code)) {
      nm <- ann %>% filter(code == !!code) %>% pull(name) %>% .[1]
      prop[[length(prop)+1]] <- tibble(bloque = "credit", seccion = s,
                                        moneda = mon, regimen = "",
                                        codigo_sdde = code,
                                        etiqueta = nm %||% paste0("Credito ", s, " ", mon))
    }
  }
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

prop_df <- bind_rows(prop)
prop_path <- "cierre_v6/codes_seleccionados_propuesto.csv"
write_csv(prop_df, prop_path)

message("\n========================================================")
message("PROPUESTA AUTOMÁTICA — codes_seleccionados_propuesto.csv")
message("========================================================")
print(prop_df, n = Inf)

# Si NO existe codes_seleccionados.csv, copiar la propuesta para
# que el script 02 pueda correr sin intervención. Si SI existe, no
# se sobreescribe (respeta lo que el usuario haya ajustado).
seleccionados_path <- "cierre_v6/codes_seleccionados.csv"
if (!file.exists(seleccionados_path)) {
  file.copy(prop_path, seleccionados_path, overwrite = FALSE)
  message("\n[01] codes_seleccionados.csv creado a partir de la propuesta automática.")
  message("    Si querés ajustarlo, editalo antes de correr el script 02.")
} else {
  message("\n[01] codes_seleccionados.csv ya existe — no se sobreescribe.")
  message("    Mira la propuesta en codes_seleccionados_propuesto.csv si querés actualizar.")
}

message("\n[01] Listo. Outputs:")
message("  cierre_v6/outputs/discovery_anotado.csv  ← principal")
message("  cierre_v6/outputs/discovery_imae_ciiu.csv")
message("  cierre_v6/outputs/discovery_regime.csv")
message("  cierre_v6/outputs/discovery_exports.csv")
message("  cierre_v6/outputs/discovery_credit.csv")
message("  cierre_v6/codes_seleccionados_propuesto.csv  ← propuesta automática")
message("  cierre_v6/codes_seleccionados.csv  ← input del script 02")
