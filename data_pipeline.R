# =============================================================================
# WUTIS Data Pipeline
# Project: Learning When Factors Work — Regime-Based Factor Rotation
#
# What this script does:
#   1. Downloads Fama-French factors (MKT-RF, SMB, HML, RMW, CMA, MOM, RF)
#   2. Downloads 10Y Treasury yield + NBER recession dummy (FRED)
#   3. Merges, cleans, converts % to decimals
#   4. Computes log returns for each factor
#   5. Engineers all state features needed for the bandit model
#   6. Drops 11-month warm-up rows (rolling windows need 12 months of history)
#   7. Defines train/validation/test split and saves splits.json
#   8. Standardizes features (z-score) using training-period mean/sd only
#   9. Saves all outputs as CSV and JSON
#
# Outputs:
#   data/raw/ff_factors_raw.csv       — raw FF download
#   data/raw/fred_raw.csv             — raw 10Y yield + recession indicator
#   data/processed/full_timeline.csv  — full dataset, all splits, incl. _z columns (CSV)
#   data/processed/full_timeline.json — full dataset, all splits, incl. _z columns (JSON)
#   data/processed/train.csv          — training set (CSV)
#   data/processed/validation.csv     — validation set (CSV)
#   data/processed/test.csv           — test set (CSV)
#   data/processed/train.json         — training set (JSON)
#   data/processed/validation.json    — validation set (JSON)
#   data/processed/test.json          — test set (JSON)
#   data/processed/splits.json        — split boundaries and row counts
#
# Note: replaces both "factor data collection.r" and "additional_data_collection.py"
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

required <- c("tidyverse", "frenchdata", "quantmod", "lubridate", "slider", "jsonlite")
to_install <- required[!required %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(frenchdata)
library(quantmod)
library(lubridate)
library(slider)
library(jsonlite)

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. DOWNLOAD RAW DATA
# =============================================================================

cat("─── Step 1: Downloading raw data ───────────────────────────────────────\n")

# ── Fama-French factors ──────────────────────────────────────────────────────
cat("  Fetching Fama-French factors...\n")

ff3  <- download_french_data("Fama/French 3 Factors")
ff5  <- download_french_data("Fama/French 5 Factors (2x3)")
mom  <- download_french_data("Momentum Factor (Mom)")

yyyymm_to_date <- function(x) {
  as.Date(paste0(as.character(x), "01"), format = "%Y%m%d")
}

ff3_clean <- ff3$subsets$data[[1]] %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, MKT_RF = `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(c(MKT_RF, SMB, HML, RF), as.numeric))

ff5_clean <- ff5$subsets$data[[1]] %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, RMW, CMA) %>%
  mutate(across(c(RMW, CMA), as.numeric))

mom_clean <- mom$subsets$data[[1]] %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, MOM = Mom) %>%
  mutate(MOM = as.numeric(MOM))

ff_all <- ff3_clean %>%
  inner_join(ff5_clean, by = "date") %>%
  inner_join(mom_clean,  by = "date") %>%
  arrange(date) %>%
  drop_na() %>%
  select(date, MKT_RF, SMB, HML, RMW, CMA, MOM, RF)

write_csv(ff_all, "data/raw/ff_factors_raw.csv")
cat("  FF factors:", format(min(ff_all$date)), "→", format(max(ff_all$date)),
    paste0("(", nrow(ff_all), " months)\n"))

# ── 10Y Treasury yield + NBER recession dummy (FRED) ─────────────────────────
# GS10  : 10-Year Treasury Constant Maturity Rate (monthly, %)
# USREC : NBER recession indicator (1 = recession, 0 = expansion)
#         Published with a lag of ~6–12 months → we lag by 6 months to be safe.

cat("  Fetching FRED data (10Y yield + recession indicator)...\n")

fred_df <- tryCatch({
  gs10_raw  <- getSymbols("GS10",  src = "FRED", auto.assign = FALSE)
  usrec_raw <- getSymbols("USREC", src = "FRED", auto.assign = FALSE)

  gs10_df <- data.frame(
    date  = as.Date(index(gs10_raw)),
    yield_10y = as.numeric(gs10_raw)
  ) %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(yield_10y = mean(yield_10y, na.rm = TRUE), .groups = "drop")

  usrec_df <- data.frame(
    date      = floor_date(as.Date(index(usrec_raw)), "month"),
    recession = as.integer(usrec_raw)
  ) %>%
    distinct(date, .keep_all = TRUE) %>%
    # Lag by 6 months: NBER announces recessions well after they begin.
    # Using the contemporaneous indicator would be look-ahead bias.
    arrange(date) %>%
    mutate(recession_lag6 = lag(recession, 6))

  inner_join(gs10_df, usrec_df, by = "date") %>%
    select(-recession)   # drop raw (non-lagged) column — use only recession_lag6
}, error = function(e) {
  warning("  FRED download failed — continuing without it: ", e$message)
  NULL
})

if (!is.null(fred_df)) {
  write_csv(fred_df, "data/raw/fred_raw.csv")
  cat("  10Y yield:", format(min(fred_df$date)), "→", format(max(fred_df$date)),
      paste0("(", nrow(fred_df), " months)\n"))
}


# =============================================================================
# 2. MERGE AND CONVERT UNITS
# =============================================================================

cat("\n─── Step 2: Merging and cleaning ────────────────────────────────────────\n")

df <- ff_all

# Left-join FRED data (10Y yield available from 1953; recession from 1854)
if (!is.null(fred_df)) {
  df <- df %>% left_join(fred_df, by = "date")
}

# % → decimal  (FF data is in percentage points, e.g. 1.23 means 1.23%)
factor_cols <- c("MKT_RF", "SMB", "HML", "RMW", "CMA", "MOM", "RF")
df <- df %>%
  mutate(across(all_of(factor_cols), ~ .x / 100))

cat("  Rows after merge:", nrow(df), "\n")
cat("  Missing values per column:\n")
print(colSums(is.na(df[, names(df) != "date"])), quote = FALSE)


# =============================================================================
# 3. LOG RETURNS
# =============================================================================
# Continuously compounded returns: r_log = ln(1 + r)
# Used for rolling summation of returns (additive instead of multiplicative).

cat("\n─── Step 3: Computing log returns ──────────────────────────────────────\n")

non_rf_factors <- c("MKT_RF", "SMB", "HML", "RMW", "CMA", "MOM")

df <- df %>%
  mutate(across(
    all_of(non_rf_factors),
    ~ log(1 + .x),
    .names = "log_{.col}"
  ))

cat("  Log return columns added:", paste0("log_", non_rf_factors, collapse = ", "), "\n")


# =============================================================================
# 4. FEATURE ENGINEERING
# =============================================================================
# All features use only information available at month t (no look-ahead).
# At decision time (end of month t) we observe all data through month t,
# and then choose an allocation rule for month t+1.

cat("\n─── Step 4: Engineering state features ──────────────────────────────────\n")

# ── Rolling helper functions ─────────────────────────────────────────────────

# Geometric return over n months: prod(1+r) - 1
roll_geo_ret <- function(x, n) {
  slide_dbl(x, ~ prod(1 + .x) - 1, .before = n - 1, .complete = TRUE)
}

# Annualised volatility of log returns over n months: sd(log_r) * sqrt(12)
roll_ann_vol <- function(log_x, n) {
  slide_dbl(log_x, ~ sd(.x, na.rm = FALSE) * sqrt(12), .before = n - 1, .complete = TRUE)
}

# Maximum drawdown over n months (most negative peak-to-trough)
roll_max_dd <- function(x, n) {
  slide_dbl(x, function(r) {
    cumret  <- cumprod(1 + r)
    running_peak <- cummax(cumret)
    min((cumret - running_peak) / running_peak)
  }, .before = n - 1, .complete = TRUE)
}

# ── Market-level features ─────────────────────────────────────────────────────
# mkt_ret_12m and mkt_vol_12m cover MKT_RF at the market level.
# The per-factor loop below covers SMB/HML/RMW/CMA/MOM — MKT_RF excluded to
# avoid creating duplicate columns (MKT_RF_ret_12m == mkt_ret_12m).

df <- df %>%
  mutate(
    # 12-month and 3-month market geometric returns
    mkt_ret_12m = roll_geo_ret(MKT_RF, 12),
    mkt_ret_3m  = roll_geo_ret(MKT_RF,  3),

    # 12-month annualised market volatility (from log returns)
    mkt_vol_12m = roll_ann_vol(log_MKT_RF, 12),

    # Market max drawdown over trailing 12 months
    mkt_dd_12m  = roll_max_dd(MKT_RF, 12)
  )

# ── Per-factor: 12-month return and annualised volatility (SMB–MOM only) ──────
# MKT_RF is excluded here — already covered by mkt_ret_12m and mkt_vol_12m above.

five_factors <- c("SMB", "HML", "RMW", "CMA", "MOM")

for (f in five_factors) {
  ret_col <- paste0(f, "_ret_12m")
  vol_col <- paste0(f, "_vol_12m")
  log_col <- paste0("log_", f)

  df[[ret_col]] <- roll_geo_ret(df[[f]],       12)
  df[[vol_col]] <- roll_ann_vol(df[[log_col]], 12)
}

# ── Cross-factor spread: best minus worst 12-month return (all 6 factors) ─────
# Includes mkt_ret_12m for MKT_RF alongside the five per-factor ret_12m columns.

ret_12m_cols <- c("mkt_ret_12m", paste0(five_factors, "_ret_12m"))

df <- df %>%
  mutate(
    factor_spread_12m = do.call(pmax, c(select(., all_of(ret_12m_cols)), list(na.rm = TRUE))) -
                        do.call(pmin, c(select(., all_of(ret_12m_cols)), list(na.rm = TRUE)))
  )

# ── 10Y Treasury yield features ───────────────────────────────────────────────
# yield_10y       : level of 10Y yield (%)
# yield_10y_chg1m : month-over-month change — captures rate direction (rising vs falling)
# yield_10y_chg12m: 12-month change — captures broader rate regime shift
# yield_rf_spread : 10Y yield minus RF — proxy for term premium / curve slope
#
# Why: rising rate environments tend to hurt growth/momentum factors and help value.
# The spread over RF proxies the yield curve slope, a classic recession leading indicator.

if ("yield_10y" %in% names(df)) {
  df <- df %>%
    mutate(
      yield_10y_chg1m  = yield_10y - lag(yield_10y, 1),
      yield_10y_chg12m = yield_10y - lag(yield_10y, 12),
      yield_rf_spread  = yield_10y - (RF * 1200)  # RF monthly decimal → annual %; yield_10y already annual %
    )
}

# ── Recession dummy features ──────────────────────────────────────────────────
# recession_lag6: NBER recession indicator lagged 6 months (avoids look-ahead bias)
# Already computed during download; no additional transformation needed.

cat("  Feature engineering complete.\n")


# =============================================================================
# 5. DROP WARM-UP ROWS
# =============================================================================
# slide_dbl(..., .before = 11, .complete = TRUE) requires a full 12-month window.
# Positions 1–11 return NA; position 12 is the first valid row.
# We filter on mkt_ret_12m (representative of all 12-month rolling features).

df <- df %>% filter(!is.na(mkt_ret_12m))

cat("\n─── Step 5: After dropping 12-month warm-up ─────────────────────────────\n")
cat("  Rows remaining:", nrow(df), "\n")
cat("  Date range:    ", format(min(df$date)), "→", format(max(df$date)), "\n")


# =============================================================================
# 6. TRAIN / VALIDATION / TEST SPLIT
# =============================================================================

TRAIN_END <- as.Date("2005-12-01")
VAL_END   <- as.Date("2014-12-01")

df <- df %>%
  mutate(split = case_when(
    date <= TRAIN_END ~ "train",
    date <= VAL_END   ~ "validation",
    TRUE              ~ "test"
  ) %>% factor(levels = c("train", "validation", "test")))

split_summary <- df %>%
  group_by(split) %>%
  summarise(start = min(date), end = max(date), n = n(), .groups = "drop")

cat("\n─── Step 6: Split summary ───────────────────────────────────────────────\n")
print(split_summary, n = Inf)

get_split <- function(s, col) split_summary %>% filter(split == s) %>% pull({{ col }})

splits_json <- list(
  train = list(
    start = format(get_split("train", start)),
    end   = format(TRAIN_END),
    n     = as.integer(get_split("train", n))
  ),
  validation = list(
    start = format(get_split("validation", start)),
    end   = format(VAL_END),
    n     = as.integer(get_split("validation", n))
  ),
  test = list(
    start = format(get_split("test", start)),
    end   = format(get_split("test", end)),
    n     = as.integer(get_split("test", n))
  )
)

write_json(splits_json, "data/processed/splits.json", pretty = TRUE, auto_unbox = TRUE)
cat("  Saved: data/processed/splits.json\n")


# =============================================================================
# 7. STANDARDIZE FEATURES (z-score using TRAINING data only)
# =============================================================================
# We compute mean and sd on the training set only, then apply to all splits.
# This avoids any data leakage into validation/test.

cat("\n─── Step 7: Standardizing features (training mean/sd only) ─────────────\n")

feature_cols <- c(
  # Market-level features (MKT_RF)
  "mkt_ret_12m", "mkt_ret_3m", "mkt_vol_12m", "mkt_dd_12m",
  # Per-factor features (SMB, HML, RMW, CMA, MOM)
  paste0(five_factors, "_ret_12m"),
  paste0(five_factors, "_vol_12m"),
  # Cross-factor dispersion
  "factor_spread_12m"
)
# 10Y yield features are continuous → standardize; recession dummy is binary → do not standardize
if ("yield_10y" %in% names(df)) {
  feature_cols <- c(feature_cols, "yield_10y", "yield_10y_chg1m", "yield_10y_chg12m", "yield_rf_spread")
}

train_rows <- df$split == "train"

for (col in feature_cols) {
  mu  <- mean(df[[col]][train_rows], na.rm = TRUE)
  sig <- sd(df[[col]][train_rows],   na.rm = TRUE)
  if (sig > 0) {
    df[[paste0(col, "_z")]] <- (df[[col]] - mu) / sig
  }
}

z_cols <- paste0(feature_cols[feature_cols %in% names(df)], "_z")
z_cols <- z_cols[z_cols %in% names(df)]
cat("  Standardized columns:", length(z_cols), "\n")


# =============================================================================
# 8. SAVE OUTPUT
# =============================================================================

cat("\n─── Step 8: Saving outputs ──────────────────────────────────────────────\n")

# Helper: convert a data frame to a list of records (one list per row) for JSON
df_to_records <- function(df) {
  df %>%
    mutate(date = format(date)) %>%
    pmap(list)
}

# ── Full timeline (all splits combined, includes _z columns) ──────────────────
full_df <- df %>% select(-split)
write_csv(full_df, "data/processed/full_timeline.csv")
write_json(df_to_records(full_df), "data/processed/full_timeline.json",
           pretty = TRUE, auto_unbox = TRUE, na = "null")
cat("  Saved: data/processed/full_timeline.csv  (", nrow(full_df), "rows )\n")
cat("  Saved: data/processed/full_timeline.json\n")

# Drop the split label column before saving individual files
train_df <- df %>% filter(split == "train")      %>% select(-split)
val_df   <- df %>% filter(split == "validation") %>% select(-split)
test_df  <- df %>% filter(split == "test")       %>% select(-split)

# ── CSV ───────────────────────────────────────────────────────────────────────
write_csv(train_df, "data/processed/train.csv")
write_csv(val_df,   "data/processed/validation.csv")
write_csv(test_df,  "data/processed/test.csv")

cat("  Saved: data/processed/train.csv      (", nrow(train_df), "rows )\n")
cat("  Saved: data/processed/validation.csv (", nrow(val_df),   "rows )\n")
cat("  Saved: data/processed/test.csv       (", nrow(test_df),  "rows )\n")

# ── JSON ──────────────────────────────────────────────────────────────────────
write_json(df_to_records(train_df), "data/processed/train.json",
           pretty = TRUE, auto_unbox = TRUE, na = "null")
write_json(df_to_records(val_df),   "data/processed/validation.json",
           pretty = TRUE, auto_unbox = TRUE, na = "null")
write_json(df_to_records(test_df),  "data/processed/test.json",
           pretty = TRUE, auto_unbox = TRUE, na = "null")

cat("  Saved: data/processed/train.json\n")
cat("          data/processed/validation.json\n")
cat("          data/processed/test.json\n")
cat("          data/processed/splits.json\n")

cat("\n  Columns in each file (", ncol(train_df), "):\n")
cat(paste0("    ", names(train_df), collapse = "\n"), "\n")

cat("\n✓ Pipeline complete.\n")
