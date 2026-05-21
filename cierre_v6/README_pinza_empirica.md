# Pinza empírica del bloque sectorial — guía de ejecución

Los scripts viven dentro del proyecto `pipeline_fx_forecast_cr` para que reaprovechen `scripts/utils_bccr.R` (credenciales, descarga, cache) y para que `.Renviron` cargue solo. Abrí el `.Rproj`, parate en la raíz del proyecto, y corré los tres scripts en orden.

## Paso 1 — descubrir códigos

```r
source("cierre_v6/01_discover_bccr_codes.R")
```

Sondea cuatro rangos del SDDE (IMAE-CIIU 95000–95300; régimen 35400–35470 y 91000–91200; exportaciones varias franjas; crédito varias franjas) y registra qué indicador devuelve cada código, con etiquetas automáticas de sección CIIU, régimen, moneda y nivel/variación. Tarda 6–10 minutos.

Salidas en `cierre_v6/outputs/`:

- `discovery_anotado.csv` — el principal, con columnas `ciiu_sugerido`, `regimen_sugerido`, `es_nivel`, `en_moneda_usd`, `en_moneda_crc`.
- `discovery_imae_ciiu.csv`, `discovery_regime.csv`, `discovery_exports.csv`, `discovery_credit.csv` — desglosados.

## Paso 2 — armar `cierre_v6/codes_seleccionados.csv` y descargar

Después de inspeccionar el discovery, escribís un CSV con esta forma:

```csv
bloque,seccion,moneda,regimen,codigo_sdde,etiqueta
imae_ciiu,TOTAL,,,95087,IMAE general
imae_ciiu,A,,,xxxxx,Agricultura - Nivel
imae_ciiu,C,,,xxxxx,Manufactura - Nivel
imae_ciiu,F,,,xxxxx,Construccion - Nivel
imae_ciiu,G,,,95141,Comercio - Nivel
imae_ciiu,H,,,xxxxx,Transporte - Nivel
imae_ciiu,I,,,xxxxx,Alojamiento - Nivel
imae_ciiu,J,,,xxxxx,Informacion y comunicaciones - Nivel
imae_ciiu,K,,,xxxxx,Financieras y seguros - Nivel
imae_ciiu,MN,,,xxxxx,Profesional - Nivel
imae_regimen,,,DEFINITIVO,xxxxx,IMAE Regimen Definitivo - Nivel
imae_regimen,,,ESPECIAL,xxxxx,IMAE Regimen Especial - Nivel
exports,A,,,xxxxx,Exportaciones agropecuarias
exports,C,,,xxxxx,Exportaciones manufactura
exports,I,,,xxxxx,Exportaciones turismo
exports,J,,,xxxxx,Exportaciones TIC
exports,MN,,,xxxxx,Exportaciones servicios profesionales
credit,A,CRC,,xxxxx,Credito agropecuario colones
credit,A,USD,,xxxxx,Credito agropecuario dolares
credit,C,CRC,,xxxxx,Credito manufactura colones
credit,C,USD,,xxxxx,Credito manufactura dolares
credit,F,CRC,,xxxxx,Credito construccion colones
credit,F,USD,,xxxxx,Credito construccion dolares
credit,G,CRC,,xxxxx,Credito comercio colones
credit,G,USD,,xxxxx,Credito comercio dolares
credit,H,CRC,,xxxxx,Credito transporte colones
credit,H,USD,,xxxxx,Credito transporte dolares
credit,I,CRC,,xxxxx,Credito alojamiento colones
credit,I,USD,,xxxxx,Credito alojamiento dolares
credit,J,CRC,,xxxxx,Credito TIC colones
credit,J,USD,,xxxxx,Credito TIC dolares
credit,MN,CRC,,xxxxx,Credito profesional colones
credit,MN,USD,,xxxxx,Credito profesional dolares
```

Reglas:

- Los códigos que conocemos están listos (95087 IMAE general, 95141 Comercio).
- Si un código no aparece en el discovery, dejá `POR_DESCUBRIR` y el script lo saltea con aviso.
- Para exportaciones por actividad: si el SDDE sólo publica algunas secciones, completá esas. No inventes.
- Para crédito por moneda: si el SDDE entrega una única serie por actividad sin separación CRC/USD, dejala vacía. La proxy σ_C se reportará como NA y se usará el prior con caveat.
- Para régimen: si encontrás Definitivo y Especial, sumalos. Es una alternativa a la dualidad CIIU.

Después corré:

```r
source("cierre_v6/02_download_pinza_empirica.R")
```

Descarga 2010-01 a hoy. Salida: `cierre_v6/outputs/raw/*.csv` y `cierre_v6/outputs/raw_pinza_manifest.csv`. Reutiliza `download_bccr_series()` de `scripts/utils_bccr.R` (cache, headers, parser).

## Paso 3 — calcular σ_Y, σ_C, cuadrantes y descriptivo

```r
source("cierre_v6/03_compute_pinza_empirica.R")
```

Produce:

- `sigma_Y_observado.csv` — proxy `exports_avg_2018_2023 / IMAE_avg_2018_2023`, normalizada al máximo.
- `sigma_C_observado.csv` — `crédito_USD / (crédito_USD + crédito_CRC)` en 2018-2023.
- `cuadrantes_observados.csv` — reasignación CONFIRMA / CAMBIA / INDETERMINADO contra los priors.
- `tabla_sectorial_v6.csv` — crecimiento 2015-2025 por sección.
- `indices_bloques_v6.csv` y `bloque_growth_v6.csv` — bloques externo vs doméstico data-driven.
- `regimen_descomposicion.csv` — definitivo vs especial si hay datos.

## Outputs que me tenés que mandar

Copiá el contenido de estos ocho archivos (los pegás como bloques de texto o los subís):

1. `cierre_v6/outputs/discovery_anotado.csv`  (ya filtrado a `status == "ok"` para reducir)
2. `cierre_v6/outputs/raw_pinza_manifest.csv`
3. `cierre_v6/outputs/sigma_Y_observado.csv`
4. `cierre_v6/outputs/sigma_C_observado.csv`
5. `cierre_v6/outputs/cuadrantes_observados.csv`
6. `cierre_v6/outputs/tabla_sectorial_v6.csv`
7. `cierre_v6/outputs/indices_bloques_v6.csv` + `bloque_growth_v6.csv`
8. `cierre_v6/outputs/regimen_descomposicion.csv`

Con eso escribo la §5 unificada **con datos observados**, la matriz de trazabilidad final, la nota metodológica de la pinza y la tabla de mapeo final (actividad → código → σ_Y → σ_C → cuadrante → confianza).

## Notas

- Los scripts respetan la API: pausas de 0,15–0,3 s entre llamadas.
- Si un rango no devolvió nada relevante, sumá rangos al script 01.
- Los outputs se escriben en `cierre_v6/outputs/` (relativo a la raíz del proyecto), no se mezclan con `output/tables/` del pipeline principal.
- Los JSON crudos del SDDE quedan en `data_raw/bccr/` (cache estándar del pipeline) gracias a `save_raw = TRUE` de `download_bccr_series`.
