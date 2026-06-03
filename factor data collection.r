# =============================================================================
# Factor Data Collection Script 
# Note: Returns not yet / 100: Left as is (which is in absolute percent, not decimals)
# =============================================================================

# Packages
install.packages(c("tidyverse", "frenchdata", "quantmod", "lubridate", "zoo"))
library(tidyverse)
library(frenchdata)   
library(quantmod)     
library(lubridate)
library(zoo)

# =============================================================================
# FAMA-FRENCH CORE FACTORS
# =============================================================================
# We need:
#   MKT-RF, SMB, HML  -> Fama/French 3-Factor dataset
#   RMW, CMA          -> Fama/French 5-Factor dataset
#   MOM               -> Momentum Factor dataset
#   RF                -> included in both 3- and 5-factor datasets

# --- 3-Factor + 5-Factor data (monthly) --------------------------------------
ff3  <- download_french_data("Fama/French 3 Factors")
ff5  <- download_french_data("Fama/French 5 Factors (2x3)")
mom  <- download_french_data("Momentum Factor (Mom)")

# Each object contains a list of sub-tables; grab the monthly one
ff3_monthly  <- ff3$subsets$data[[1]]   # first subset is always monthly
ff5_monthly  <- ff5$subsets$data[[1]]
mom_monthly  <- mom$subsets$data[[1]]

# Helper: frenchdata returns dates as integers (YYYYMM) — convert to Date
yyyymm_to_date <- function(x) {
  as.Date(paste0(as.character(x), "01"), format = "%Y%m%d")
}

# Clean and rename each table
ff3_clean <- ff3_monthly %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, MKT_RF = `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(c(MKT_RF, SMB, HML, RF), as.numeric))

ff5_clean <- ff5_monthly %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, RMW, CMA) %>%
  mutate(across(c(RMW, CMA), as.numeric))

mom_clean <- mom_monthly %>%
  rename(date_raw = date) %>%
  mutate(date = yyyymm_to_date(date_raw)) %>%
  select(date, MOM = Mom) %>%
  mutate(MOM = as.numeric(MOM))

# --- Merge all three on date -------------------------------------------------
# Inner join: only keep months where ALL factors are available.
# This naturally drops any NA rows and gives us the common history.
ff_factors <- ff3_clean %>%
  inner_join(ff5_clean, by = "date") %>%
  inner_join(mom_clean, by = "date") %>%
  arrange(date) %>%
  # Final safety net: drop any remaining NA rows
  drop_na() %>%
  # Reorder columns cleanly
  select(date, MKT_RF, SMB, HML, RMW, CMA, MOM, RF)

cat("=== Core Fama-French Factors ===\n")
cat("Date range:", format(min(ff_factors$date)), "to", format(max(ff_factors$date)), "\n")
cat("Rows:      ", nrow(ff_factors), "\n")
cat("Columns:   ", paste(names(ff_factors), collapse = ", "), "\n\n")

write_csv(ff_factors, "ff_factors.csv")
cat("Saved: ff_factors.csv\n\n")