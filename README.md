# pipeline_fx_forecast_cr

**Forecasting, Tail Risk, and Regime Detection for the CRC/USD Exchange Rate**
**+ IS-LM-BP de cuatro cuadrantes: contrafactual del impuesto silencioso**

---

## Research Questions

1. What is the plausible distribution of CRC/USD trajectories over 1–24 week horizons?
2. What is the maximum plausible CRC appreciation (downside) at 95% confidence, given current conditions?
3. Is there evidence of a transition from FX abundance to compression or stress?
4. **(ISLMBP add-on)** ¿Cuánto se amplifica la asimetría distributiva sectorial (impuesto silencioso) bajo un choque externo adverso (Ormuz) si el BCCR sostiene el régimen de apreciación vía TPM alta?

---

## Architecture: Weekly-First + Monthly Quadrant Add-On

The pipeline operates at **weekly frequency (Friday close)** as the primary modelling layer for FX forecasting.

**Add-on IS-LM-BP de cuatro cuadrantes (scripts 20–25):** módulo mensual estructural que descompone la economía costarricense en cuatro cuadrantes sectoriales según composición monetaria de ingresos y costos.  Calibra el motor con OLS+HAC, simula tres escenarios del choque de Ormuz, y produce métricas integradas del impuesto silencioso.

### Cuadrante 2×2 sectorial

|                       | Ingresos colones                              | Ingresos dólares                                 |
|-----------------------|-----------------------------------------------|--------------------------------------------------|
| **Costos colones**    | **NH** Hedged CRC-CRC (servicios locales)     | **TV** Muriendo (bananos, construcción preventa) |
| **Costos dólares**    | **NB** Beneficiado (comercio importador)      | **TH** Hedged USD-USD (zona franca, TIC)         |

El impuesto silencioso opera transfiriendo márgenes del cuadrante TV hacia el cuadrante NB, con los cuadrantes hedged (TH, NH) en posición neutral.

### Anclaje empírico de la pinza sectorial

- **`cost_share_usd` (eje x):** construido desde el cuadro 144 BCCR (crédito al sector privado por actividad × moneda).  Series por sector marcadas `POR_VERIFICAR` en el catálogo.
- **`income_share_usd` (eje y):** construido desde el cuadro 82 BCCR (exportaciones FOB por actividad).  Series por sector marcadas `POR_VERIFICAR` en el catálogo.
- **Fallback a priori:** valores en `SECTOR_MATRIX` (en `utils_islmbp.R`) calibrados a partir de juicio económico documentado, anclado en la propuesta BCCR marzo 2026 (Anexo C) y extendido a la geometría bidimensional.

### Mapeo IS-LM-BP canónico al motor

| Pieza canónica | Equivalente en el motor |
|----------------|------------------------|
| Curva IS       | Desagregada en cuatro: IS_TV + IS_TH + IS_NH + IS_NB |
| Curva LM       | Implícita en regla de Taylor (TPM como instrumento)  |
| Curva BP       | Identidad de FX + ITCER (módulo BP explícito pendiente) |
| Phillips       | Cierre nominal con pass-through cambiario y de petróleo |

### Ecuaciones del motor

| Eq. | Variable dependiente | Regresores | Hipótesis del signo del ITCER |
|-----|---------------------|------------|--------------------------------|
| IS_TV | `y_TV_yoy_log` | `us_ip_yoy`, `itcer_yoy`, `tot_yoy`, lag | **β NEGATIVO fuerte** (asfixia margenes) |
| IS_TH | `y_TH_yoy_log` | `us_ip_yoy`, `tot_yoy`, lag | omitido (hedge natural) |
| IS_NH | `y_NH_yoy_log` | `r_real`, `cr_col_yoy`, lag | omitido (poca transabilidad) |
| IS_NB | `y_NB_yoy_log` | `r_real`, `itcer_yoy`, lag | **β POSITIVO** (insumos importados abaratados) |
| Taylor | `tpm` | `tpm_lag1`, `inflation_yoy`, `output_gap` [, `q_gap`] | — |
| Phillips | `inflation_yoy_d1` | lag, `output_gap`, `fx_yoy`, `wti_yoy` | — |
| FX | `fx_sell_yoy_log` | `rate_diff_cr_us`, `tot_yoy`, `vix_d1` | — |

### Métricas del impuesto silencioso

- `tax_squeeze_TV = y_TH - y_TV` — brecha asimétrica en el bloque transable
- `tax_subsidy_NB = y_NB - y_NH` — subsidio asimétrico en el bloque doméstico
- `silent_tax_amplitude = squeeze + subsidy` — amplitud total

### Tres escenarios de Ormuz

| Esc. | Brent USD | Duración | ΔToT | ΔVIX | ΔFedFunds | Fuente |
|------|-----------|----------|------|------|-----------|--------|
| A — Moderado | 98 | 3m | −4% | +6 | +0.50pp | Dallas Fed Mar 2026 |
| B — Medio | 130 | 6m | −9% | +14 | +1.00pp | Goldman / Bloomberg base |
| C — Severo | 180 | 9m | −16% | +28 | +1.50pp | Bloomberg cota superior |

---

## Quick Start

### 1. Prerequisites

- **R ≥ 4.2** with RStudio.
- BCCR SDDE account ([register](https://gee.bccr.fi.cr/Indicadores/Suscripciones/)).
- FRED API key.

### 2. Credentials

```bash
cp .Renviron.example .Renviron
```

### 3. Run

```r
# Pipeline completo (forecasting + ISLMBP):
scripts <- sort(list.files("scripts", "^[0-9]+.*\\.R$", full.names=TRUE))
for (s in scripts) source(s)

# Sólo bloque ISLMBP (asume features_monthly.rds existe):
source("run_islmbp.R")
```

### 4. Outputs del bloque ISLMBP

- **Tablas:** `output/tables/paper_tabla{1..7}_*.csv` + `paper_hechos_estilizados.csv`
- **Figuras:** `output/figures/paper_fig{1..6}_*.png`
- **Diagnóstico:** `data_intermediate/islmbp/coverage_report.csv` + `quadrant_mapping.csv`

---

## Configuration

| File | Purpose |
|------|---------|
| `config/series_bccr_template.csv` | Catálogo BCCR (incluye placeholders crédito por actividad/moneda y exportaciones por actividad) |
| `config/series_external_template.csv` | FRED catalog |
| `config/horizons.csv` | Forecast horizons |
| `config/model_specs.yml` | Forecasting parameters |
| `config/regime_rules.yml` | Regime thresholds |
| `config/islmbp_specs.yml` | IS-LM-BP parameters (cuadrante 2×2, pesos PIB, métricas) |
| `config/shock_scenarios.yml` | Tres escenarios de Ormuz |

---

## Códigos BCCR pendientes de verificación

Antes de la primera corrida productiva, verificar en gee.bccr.fi.cr los códigos de las series marcadas `POR_VERIFICAR` en `notes`:

- **IMAE sectoriales** (cuadro 954): `imae_agro`, `imae_comer`, `imae_const`, `imae_aloj`, `imae_transp`, `imae_infocom`, `imae_prof`
- **Crédito por actividad × moneda** (cuadro 144): `cartera_{agro,const,comer,transp,aloj,infocom,prof}_{crc,usd}`
- **Exportaciones por actividad** (cuadro 82): `exports_fob_{agro,const,infocom,prof,aloj}`
- **Crédito agregado por moneda**: `credit_col`, `credit_usd`
- **Términos de intercambio**: `tot`

Si alguna serie no está disponible, el pipeline degrada gracefully: los shares observados quedan `NA` y el motor cae al fallback de priors en `SECTOR_MATRIX`.

---

## Dependencies

**Core:** dplyr, tidyr, readr, ggplot2, ggrepel, lubridate, zoo, httr, jsonlite, yaml
**Modelling:** forecast, glmnet, quantreg, vars, sandwich, lmtest, broom
**Plotting:** scales, gridExtra

All installed automatically by `00_setup.R`.

---

## Reproducir el pipeline en local en 5 pasos

```bash
# 1. Clonar el repositorio
git clone https://github.com/<USER>/pipeline_fx_forecast_cr.git
cd pipeline_fx_forecast_cr

# 2. Crear .Renviron con tus credenciales BCCR SDDE
cp .Renviron.example .Renviron
#    editar .Renviron y colocar BCCR_EMAIL y BCCR_TOKEN
#    obtener el token en https://gee.bccr.fi.cr/Indicadores/Suscripciones/

# 3. Abrir el proyecto R
open pipeline_fx_forecast_cr.Rproj
```

```r
# 4. Desde dentro de RStudio, instalar dependencias y correr setup
source("scripts/00_setup.R")

# 5. Correr la pinza empírica completa (descubrimiento + descarga + cómputo)
source("cierre_v6/00_run_pinza_completa.R")
```

Los outputs de la pinza quedan en `cierre_v6/outputs/`. La descarga inicial del BCCR toma 3-4 minutos por la pausa anti rate-limit del SDDE.

### Reproducir sólo la pinza empírica del paper

```r
source("cierre_v6/02_download_pinza_empirica.R")    # descarga 47 series
source("cierre_v6/10_validar_y_computar_pinza.R")   # validación hipótesis MN/ME
source("cierre_v6/11_pinza_v2.R")                   # σ_C ventana corta + larga
```

Outputs centrales:
- `sigma_C_v2_corto.csv` — foto sectorial enero 2024 a julio 2025
- `sigma_C_v2_largo.csv` — serie anual TOTAL 2010 a 2025
- `sigma_C_v2_largo_resumen.csv` — hitos clave (mínimo, máximo, años pivote)
- `sigma_C_v2_triangul.csv` — sanity check contable

---

## Citar este trabajo

Si usás el pipeline o los hallazgos en tu propia investigación, citá el paper asociado:

> Soto Rivera, M. (2026). *Cuando la abundancia no se absorbe: una conjetura sobre la apreciación real y el ajuste sectorial en Costa Rica bajo Mundell-Fleming, 2010–2025*. Working paper.

BibTeX:

```bibtex
@unpublished{sotorivera2026abundancia,
  author = {Soto Rivera, Mauricio},
  title  = {Cuando la abundancia no se absorbe: una conjetura sobre la apreciación real y el ajuste sectorial en Costa Rica bajo Mundell-Fleming, 2010--2025},
  year   = {2026},
  note   = {Working paper. Pipeline disponible en https://github.com/<USER>/pipeline_fx_forecast_cr}
}
```

---

## Licencia

Código distribuido bajo [licencia MIT](LICENSE). El paper asociado y los documentos de respaldo se distribuyen bajo Creative Commons Atribución 4.0 Internacional (CC-BY 4.0) cuando se publiquen.

---

## Contribuciones y discusión

Issues y pull requests son bienvenidos. Para discusión académica o aportes al marco analítico, abrir un issue con el tag `discussion`. Para reportes de bugs en el pipeline o problemas de reproducibilidad, abrir issue con tag `bug` y adjuntar la versión de R y los resultados de `sessionInfo()`.

---

## Disclaimer

Las opiniones expresadas en este trabajo y en los materiales asociados son exclusivamente del autor y no representan la posición del Banco Central de Costa Rica ni de sus órganos adscritos.
