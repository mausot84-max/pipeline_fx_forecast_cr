# ============================================================
# 00_setup.R — Project setup, packages, logging, globals
# ============================================================
#
# WEEKLY-FIRST ARCHITECTURE
# Primary modelling frequency: weekly (Friday close).
# Monthly macro variables enter the weekly panel via carry-
# forward with publication lag.  A monthly dataset is exported
# for supplementary analysis only.
# ============================================================

cat("====================================================\n")
cat("  pipeline_fx_forecast_cr — Setup\n")
cat("  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("====================================================\n\n")

# ------ NULL-COALESCING OPERATOR ------------------------------
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || identical(lhs, "")) rhs else lhs
}

# ------ WORKING DIRECTORY -------------------------------------
if (!file.exists("pipeline_fx_forecast_cr.Rproj"))
  stop("Set working directory to the project root.")

# ------ PACKAGES ----------------------------------------------
required_pkgs <- c(
  "dplyr","tidyr","readr","stringr","purrr","tibble","ggplot2",
  "lubridate","zoo","xts",
  "httr","jsonlite","yaml",
  "forecast","glmnet","quantreg","vars",
  "sandwich","lmtest",
  "scales","gridExtra","broom","ggrepel"
)

missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("[SETUP] Installing:", paste(missing, collapse=", "), "\n")
  install.packages(missing, repos = "https://cran.r-project.org")
}
suppressPackageStartupMessages(
  for (p in required_pkgs) library(p, character.only = TRUE)
)

# Resolve MASS::select masking dplyr::select (loaded by vars)
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename

cat("[SETUP] Packages OK.\n")

# ------ DIRECTORIES -------------------------------------------
for (d in c("config",
            "data_raw/bccr","data_raw/external",
            "data_intermediate/clean","data_intermediate/features",
            "data_intermediate/diagnostics",
            "data_intermediate/islmbp",
            "data_final","models",
            "output/tables","output/figures","output/logs"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ------ UTILITIES ---------------------------------------------
for (s in c("scripts/utils_bccr.R","scripts/utils_external.R",
            "scripts/utils_features.R","scripts/utils_modeling.R",
            "scripts/utils_plotting.R",
            "scripts/utils_islmbp.R")) {
  if (file.exists(s)) { source(s); cat("[SETUP] Sourced:", s, "\n") }
  else warning("[SETUP] Missing: ", s)
}

# ------ LOGGING -----------------------------------------------
LOG_FILE <- paste0("output/logs/pipeline_",
                    format(Sys.Date(), "%Y%m%d"), ".log")

log_msg <- function(msg, level = "INFO") {
  entry <- sprintf("[%s] [%s] %s\n",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  cat(entry); cat(entry, file = LOG_FILE, append = TRUE)
}
log_msg("Session started.")

# ------ CONFIGURATION ----------------------------------------
horizons_cfg <- readr::read_csv("config/horizons.csv",  show_col_types = FALSE)
model_specs  <- yaml::read_yaml("config/model_specs.yml")
regime_rules <- yaml::read_yaml("config/regime_rules.yml")

# ISLMBP-specific configs (optional, gracefully skipped if absent)
if (file.exists("config/islmbp_specs.yml"))
  islmbp_specs <- yaml::read_yaml("config/islmbp_specs.yml")
if (file.exists("config/shock_scenarios.yml"))
  shock_scenarios_cfg <- yaml::read_yaml("config/shock_scenarios.yml")

log_msg("Config loaded.")

# ------ GLOBALS -----------------------------------------------
GLOBAL_START_DATE <- as.Date("2010-01-01")
WEEK_END_DAY      <- 5L           # 5 = Friday (wday, week_start=1)

BT_MIN_TRAIN <- model_specs$backtesting$min_train_obs %||% 104
BT_STEP      <- model_specs$backtesting$step_size     %||% 4
BT_SCHEME    <- model_specs$backtesting$scheme         %||% "expanding"

HORIZONS_W <- horizons_cfg %>% filter(frequency=="weekly")  %>% pull(horizon_periods)
HORIZONS_M <- horizons_cfg %>% filter(frequency=="monthly") %>% pull(horizon_periods)

log_msg(paste("Weekly horizons:", paste(HORIZONS_W, collapse=",")))
cat("\n[SETUP] Done.  Architecture: WEEKLY-FIRST (Friday close) + ISLMBP monthly add-on.\n\n")
