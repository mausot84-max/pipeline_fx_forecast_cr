# pipeline_fx_forecast_cr

**Forecasting, Tail Risk, and Regime Detection for the CRC/USD Exchange Rate**
**+ Crédito como indicador del IS bajo apreciación cambiaria sostenida**

Pipeline asociado al paper *"Abundancia cambiaria y la brecha creciente de dos economías. Apreciación cambiaria, asimetría sectorial y la divergencia entre producto territorial e ingreso nacional en Costa Rica bajo Mundell-Fleming, 2010–2025"* (Soto Rodríguez, 2026).

---

## Preguntas de investigación

1. ¿Cuál es la distribución plausible de las trayectorias CRC/USD a horizontes de 1 a 24 semanas?
2. ¿Cuál es la máxima apreciación plausible del CRC al 95 % de confianza dado el régimen actual?
3. ¿Existe evidencia de transición desde el régimen de abundancia hacia compresión o estrés?
4. ¿Se observa la propagación al lado real predicha por el aparato IS-LM-BP vía el canal del crédito?

---

## Arquitectura: forecasting semanal + Prueba 5 mensual sobre crédito

El pipeline opera en frecuencia semanal (cierre de viernes) como capa primaria para forecasting cambiario.

**Módulo del paper (scripts 27 a 30):** Prueba 5 sobre la dinámica del crédito al sector privado. Reemplaza, en la iteración v8 del paper, al módulo de pinza sectorial σ^Y / σ^C y al ejercicio econométrico IS-TV agregado. La justificación teórica es que, si la curva IS se desplaza a la izquierda en el bloque doméstico bajo apreciación nominal sostenida, el indicador observable más temprano y limpio no es el IMAE agregado sino la dinámica del crédito (Borio, 2014; Schularick & Taylor, 2012; Drehmann, Borio & Tsatsaronis, 2012; Mian, Sufi & Verner, 2017).

### Tres registros del crédito como evidencia del mecanismo IS

| Registro | Frecuencia | Período           | Test central                                | Script   |
|----------|------------|-------------------|---------------------------------------------|----------|
| 1        | Mensual    | 2010-01 a 2025-07 | Chow / sup-Wald @ 2022 sobre serie agregada | `27_credit_aggregate_long.R` |
| 2        | Mensual    | 2024-01 a 2025-07 | Ranking sectorial 2025 por moneda           | `28_credit_sectoral_144.R`   |
| 3        | Trimestral | 2022-12 a presente| Razón CEC / total ME (SUGEF Acuerdo 2-10)   | `29_sugef_cec_sec.R`         |

Los tres registros convergen sobre un hallazgo único: la asimetría por moneda del frenazo del crédito —concentrada en el componente USD a deudores sin generación de divisas— es la firma cuantitativa del desplazamiento IS bajo apreciación cambiaria sostenida.

### Extensión al escenario adverso (sección 8 del paper)

`30_credit_stress_propagation.R` aplica la elasticidad sectorial del crédito al IMAE estimada en los registros 1 y 2 al escenario Ormuz definido en `config/shock_scenarios.yml` y simulado en `23_islmbp_simulate.R`. Produce la proyección contrafactual del crédito bajo shock para cerrar el aparato narrativo de la sección 8.

---

## Quick Start

### 1. Requisitos

- R ≥ 4.2 con RStudio
- Cuenta BCCR SDDE ([registro](https://gee.bccr.fi.cr/Indicadores/Suscripciones/))
- API key de FRED

### 2. Credenciales

```bash
cp .Renviron.example .Renviron
# editar y colocar BCCR_EMAIL, BCCR_TOKEN, FRED_API_KEY
```

### 3. Correr la Prueba 5 completa

```r
source("scripts/00_setup.R")

# Prueba 5 — los tres registros
source("scripts/27_credit_aggregate_long.R")
source("scripts/28_credit_sectoral_144.R")
source("scripts/29_sugef_cec_sec.R")   # requiere Excel SUGEF en data_raw/sugef/

# Extensión Ormuz al crédito (sección 8 del paper)
source("scripts/30_credit_stress_propagation.R")

# Validación independiente (6 checks contra tolerancias documentadas)
source("valida_paper.R")
```

### 4. Outputs

- `output/credit_breakpoints_baiperron.csv` — fechas de quiebre endógenas
- `output/credit_supwald_2022.csv` — estadístico y p-valor del Chow @ 2022
- `output/credit_means_subperiods.csv` — medias pre/post 2022 por moneda
- `output/credit_sectoral_deceleration_ranking.csv` — ranking sectorial 2025
- `output/sugef_cec_ratios.csv` — razones SUGEF CEC/total ME y flujos
- `output/credit_stress_propagation_table.csv` — proyección Ormuz al crédito
- `output/fig_credit_*.png` — gráficos correspondientes

---

## Configuration

| File                              | Purpose                                                   |
|-----------------------------------|-----------------------------------------------------------|
| `config/series_bccr_template.csv` | Catálogo BCCR (crédito SBN por actividad y moneda)        |
| `config/series_external_template.csv` | Catálogo FRED                                          |
| `config/horizons.csv`             | Horizontes de forecasting                                 |
| `config/model_specs.yml`          | Parámetros de los modelos de forecasting                  |
| `config/regime_rules.yml`         | Umbrales del detector de régimen                          |
| `config/shock_scenarios.yml`      | Tres escenarios de Ormuz                                  |

---

## SUGEF Acuerdo 2-10 — preparación manual

El reporte SUGEF de exposición cambiaria (clasificación CEC / SEC) se publica trimestralmente desde diciembre 2022 en `sugef.fi.cr`. El pipeline lee los archivos Excel desde `data_raw/sugef/` con nomenclatura `sugef_cec_sec_YYYY_QN.xlsx`. Descargar manualmente y colocar en esa carpeta antes de correr `29_sugef_cec_sec.R`. El parser inicial cubre el layout vigente de la SUGEF; ajustar `parse_sugef()` si la estructura del Excel cambia.

---

## Reproducir el pipeline en local en 5 pasos

```bash
# 1. Clonar el repositorio
git clone https://github.com/mausot84-max/pipeline_fx_forecast_cr.git
cd pipeline_fx_forecast_cr

# 2. Configurar credenciales
cp .Renviron.example .Renviron
#    editar y colocar BCCR_EMAIL, BCCR_TOKEN, FRED_API_KEY

# 3. Abrir el proyecto R
open pipeline_fx_forecast_cr.Rproj
```

```r
# 4. Dependencias
source("scripts/00_setup.R")

# 5. Prueba 5 + validación
source("scripts/27_credit_aggregate_long.R")
source("scripts/28_credit_sectoral_144.R")
source("scripts/29_sugef_cec_sec.R")
source("scripts/30_credit_stress_propagation.R")
source("valida_paper.R")
```

Tiempo total estimado: 8 a 12 minutos (la pausa anti rate-limit del SDDE domina).

---

## Validación independiente — los seis checks

El script `valida_paper.R` verifica seis resultados centrales del paper contra tolerancias documentadas:

| ID | Check                                          | Tolerancia                                    |
|----|------------------------------------------------|-----------------------------------------------|
| c1 | Manifest de descargas crédito                  | ≥18 OK y fail ≤8 (fails son agregadores)      |
| c2 | Desaceleración crédito USD post-2022           | Δ < −6 pp, post < 3 %                         |
| c3 | Estabilidad del crédito CRC post-2022          | \|Δ\| < 2 pp                                  |
| c4 | Chow test @ 2022 sobre crédito USD             | F > 15, p < 0.001                             |
| c5 | Ranking sectorial 2025 USD                     | Vivienda < −10 %, Industria < −5 %            |
| c6 | SUGEF razón CEC / total ME                     | entre 55 % y 70 %                             |

El check c6 se marca SKIP si el archivo SUGEF no está en `data_raw/sugef/`; la cifra ~ 61,8 % al cierre 2022 corresponde al reporte público de SUGEF Acuerdo 2-10 y se cita directamente en el paper.

---

## Dependencias R

**Core:** dplyr, tidyr, readr, ggplot2, lubridate, zoo, httr, jsonlite, yaml
**Modelos:** strucchange (Bai-Perron, sup-Wald), sandwich, lmtest, broom
**Excel:** readxl (parser SUGEF)

Instaladas automáticamente por `00_setup.R`.

---

## Citar este trabajo

> Soto Rodríguez, M. (2026). *Abundancia cambiaria y la brecha creciente de dos economías. Apreciación cambiaria, asimetría sectorial y la divergencia entre producto territorial e ingreso nacional en Costa Rica bajo Mundell-Fleming, 2010–2025*. Working paper. II Concurso de Investigación Económica Eduardo Lizano Fait, Academia de Centroamérica.

BibTeX:

```bibtex
@unpublished{sotorodriguez2026abundancia,
  author = {Soto Rodríguez, Mauricio},
  title  = {Abundancia cambiaria y la brecha creciente de dos economías: Apreciación cambiaria, asimetría sectorial y la divergencia entre producto territorial e ingreso nacional en Costa Rica bajo Mundell-Fleming, 2010--2025},
  year   = {2026},
  note   = {Working paper. Pipeline disponible en https://github.com/mausot84-max/pipeline_fx_forecast_cr}
}
```

---

## Literatura de respaldo de la Prueba 5

- Borio, C. (2014). "The financial cycle and macroeconomics: What have we learnt?" *Journal of Banking & Finance*, 45, 182-198.
- Schularick, M. & Taylor, A. M. (2012). "Credit Booms Gone Bust: Monetary Policy, Leverage Cycles, and Financial Crises, 1870–2008." *American Economic Review*, 102(2), 1029-61.
- Drehmann, M., Borio, C., & Tsatsaronis, K. (2012). "Characterising the financial cycle: don't lose sight of the medium term." *BIS Working Papers* No. 380.
- Mian, A., Sufi, A., & Verner, E. (2017). "Household Debt and Business Cycles Worldwide." *Quarterly Journal of Economics*, 132(4), 1755-1817.
- Aikman, D. et al. (2018). "Measuring risks to UK financial stability." *Bank of England Staff Working Paper* No. 738.

---

## Licencia

Código distribuido bajo [licencia MIT](LICENSE).

---

## Disclaimer

Las opiniones expresadas en este trabajo y en los materiales asociados son exclusivamente del autor y no representan la posición del Banco Central de Costa Rica ni de sus órganos adscritos.
