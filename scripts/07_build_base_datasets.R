# ============================================================
# 07_build_base_datasets.R — Weekly + monthly master panels
# ============================================================
# WEEKLY (primary): all series aggregated to Friday close.
#   - Daily/weekly -> end-of-week (Friday).
#   - Monthly macro -> carried forward with 30-day pub lag.
#   - Quarterly -> carried forward with 90-day pub lag.
# MONTHLY (auxiliary): all series to end-of-month.
# ============================================================

log_msg("=== 07: Building base datasets ===")

clean_bccr <- readRDS("data_intermediate/clean/clean_bccr.rds")
clean_ext  <- readRDS("data_intermediate/clean/clean_external.rds")

# ------ AGGREGATION METHOD ------------------------------------
agg_method <- function(family) {
  if (family %in% c("flows")) return("sum")
  if (family %in% c("local_real","sentiment","external_real")) return("mean")
  "last"  # prices, rates, indices
}

lookup_meta <- function(vname) {
  row <- bind_rows(
    catalog_bccr %>% filter(var_name == vname),
    catalog_ext  %>% filter(var_name == vname)
  )
  if (nrow(row) == 0) return(list(freq = "daily", family = "other"))
  list(freq   = row$frequency[1],
       family = row$family[1])
}

# ------ BUILD WEEKLY PANEL ------------------------------------
log_msg("Building weekly panel (Friday close) ...")

fx_daily <- clean_bccr[["fx_sell"]]
if (is.null(fx_daily)) stop("fx_sell is required.")

fx_weekly <- aggregate_to_weekly(fx_daily, "date", "fx_sell", "last") %>%
  rename(fx_sell = value)
master_weekly <- fx_weekly

merge_weekly <- function(master, sdf, vname) {
  if (is.null(sdf)) return(master)
  val_col <- names(sdf)[2]
  meta <- lookup_meta(vname)
  meth <- agg_method(meta$family)

  if (meta$freq %in% c("daily","weekly")) {
    agg <- aggregate_to_weekly(sdf, "date", val_col, meth) %>%
      rename(!!vname := value)
  } else if (meta$freq == "monthly") {
    agg <- sdf %>% rename(!!vname := !!val_col) %>%
      mutate(date = date + 30)   # 30-day publication lag
  } else if (meta$freq == "quarterly") {
    agg <- sdf %>% rename(!!vname := !!val_col) %>%
      mutate(date = date + 90)
  } else {
    agg <- aggregate_to_weekly(sdf, "date", val_col, meth) %>%
      rename(!!vname := value)
  }

  master %>%
    left_join(agg, by = "date", suffix = c("",".dup")) %>%
    select(-ends_with(".dup"))
}

for (vn in setdiff(names(clean_bccr), "fx_sell"))
  master_weekly <- merge_weekly(master_weekly, clean_bccr[[vn]], vn)
for (vn in names(clean_ext))
  master_weekly <- merge_weekly(master_weekly, clean_ext[[vn]], vn)

master_weekly <- master_weekly %>% arrange(date) %>%
  tidyr::fill(everything(), .direction = "down")

log_msg(paste("Weekly:", nrow(master_weekly), "x", ncol(master_weekly)))

# ------ BUILD MONTHLY PANEL -----------------------------------
log_msg("Building monthly panel (end-of-month) ...")

fx_monthly <- aggregate_to_monthly(fx_daily, "date", "fx_sell", "last") %>%
  rename(fx_sell = value)
master_monthly <- fx_monthly

merge_monthly <- function(master, sdf, vname) {
  if (is.null(sdf)) return(master)
  val_col <- names(sdf)[2]
  meta <- lookup_meta(vname)
  meth <- agg_method(meta$family)

  if (meta$freq %in% c("daily","weekly")) {
    agg <- aggregate_to_monthly(sdf, "date", val_col, meth) %>%
      rename(!!vname := value)
  } else {
    agg <- sdf %>% rename(!!vname := !!val_col) %>%
      mutate(date = lubridate::ceiling_date(date, "month") - 1)
  }
  master %>%
    left_join(agg, by = "date", suffix = c("",".dup")) %>%
    select(-ends_with(".dup"))
}

for (vn in setdiff(names(clean_bccr), "fx_sell"))
  master_monthly <- merge_monthly(master_monthly, clean_bccr[[vn]], vn)
for (vn in names(clean_ext))
  master_monthly <- merge_monthly(master_monthly, clean_ext[[vn]], vn)

master_monthly <- master_monthly %>% arrange(date) %>%
  tidyr::fill(everything(), .direction = "down")

log_msg(paste("Monthly:", nrow(master_monthly), "x", ncol(master_monthly)))

# ------ SAVE --------------------------------------------------
saveRDS(master_weekly,  "data_intermediate/clean/master_weekly.rds")
saveRDS(master_monthly, "data_intermediate/clean/master_monthly.rds")
log_msg("=== 07 done ===")
