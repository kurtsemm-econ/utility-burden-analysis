# Racial Stratification & Utility Burdens — latest years debug script
# This script loads ONLY the latest IPUMS extract (e.g., 2017–2024),
# runs the same core cleaning / burden construction pipeline as the
# Quarto file, and leaves you with a `cps` object to inspect.

# -----------------------------
# 0. Packages & helpers
# -----------------------------
library(tidyverse)
library(ipumsr)
library(readxl)
library(data.table)
library(spatstat.geom)   # for weighted histograms (whist)
library(reldist)         # weighted quantiles

set.seed(308)

# Helper: safe factor labeler for IPUMS labelled integers
as_lbl <- function(x) ipumsr::as_factor(x)

# -----------------------------
# 1. File paths (EDIT THESE)
# -----------------------------
# Point these to your *latest-years* IPUMS extract (e.g., 2017–2024)
ipums_xml  <- "EDIT_ME_LATEST.xml"
ipums_dat  <- "EDIT_ME_LATEST.dat.gz"

# CPI file (same as in your Quarto project)
cpi_path   <- "CPI.xlsx"

# -----------------------------
# 2. Load IPUMS microdata (latest years only)
# -----------------------------
ddi <- read_ipums_ddi(ipums_xml)

cps_data <- read_ipums_micro(ddi, data_file = ipums_dat, verbose = FALSE) %>%
  dplyr::select(
    YEAR, HHWT,
    HHINCOME, COSTWATR, COSTELEC, COSTGAS,
    FARM, PLUMBING, SINK, HOTWATER, FOODSTMP,
    RELATE, COUNTYFIP,
    PERNUM, EDUC, AGE, RACE, HISPAN, SEX
  )

# Quick sanity check on years
print(sort(unique(cps_data$YEAR)))

# -----------------------------
# 3. Load CPI data
# -----------------------------
CPI <- read_excel(cpi_path)

cpi <- CPI %>%
  rename(
    YEAR   = Year,
    Annual = `Annual Average CPI`
  ) %>%
  mutate(
    YEAR = as.integer(YEAR)
  ) %>%
  select(YEAR, Annual)

# -----------------------------
# 4. Clean sample (mirror Quarto `clean-sample` chunk)
# -----------------------------
cps <- cps_data %>%
  # 1) Join CPI and drop years without CPI
  left_join(cpi, by = "YEAR") %>%
  filter(!is.na(Annual)) %>%

  # 2) Attach factor labels for filters
  mutate(
    PLUMBING_f = as_lbl(PLUMBING),
    SINK_f     = as_lbl(SINK),
    HOTWATER_f = as_lbl(HOTWATER),
    FOODSTMP_f = as_lbl(FOODSTMP)
  )

# IMPORTANT:
# Paste your full set of sample restrictions here,
# exactly as in the Quarto `clean-sample` chunk.
# For example, something like (this is illustrative only):
#
#   cps <- cps %>%
#     filter(
#       HHINCOME >= 1,
#       FARM == 1,                    # non-farm households (per your coding)
#       PLUMBING_f == "Complete plumbing",
#       SINK_f == "Has sink",
#       RELATE == 1,                  # head of household
#       !is.na(COUNTYFIP)
#     ) %>%
#     mutate(
#       HHINCOME = (HHINCOME/Annual)*100,
#       COSTWATR = (COSTWATR/Annual)*100,
#       COSTELEC = (COSTELEC/Annual)*100,
#       COSTGAS  = (COSTGAS /Annual)*100
#     )
#
# In your real script, remove the example above and
# paste the actual filter/mutate pipeline from the .qmd.

# -----------------------------
# 5. Equivalize (same as `equivalize` chunk)
# -----------------------------
# After your filters/deflation above, run:
#
# n_eq <- 0.5
# cps <- cps %>%
#   mutate(
#     HHINCOME = HHINCOME/(PERNUM^n_eq),
#     COSTWATR = COSTWATR/(PERNUM^n_eq),
#     COSTELEC = COSTELEC/(PERNUM^n_eq),
#     COSTGAS  = COSTGAS /(PERNUM^n_eq)
#   )

# -----------------------------
# 6. Construct burdens (same as `construct-burdens` chunk)
# -----------------------------
# Then construct the burden measures:
#
# cps <- cps %>%
#   mutate(
#     util_burden  = (COSTWATR + COSTELEC + COSTGAS)/HHINCOME,
#     water_burden = COSTWATR/HHINCOME,
#     elec_burden  = COSTELEC/HHINCOME,
#     gas_burden   = COSTGAS/HHINCOME
#   )

# -----------------------------
# 7. Debugging / exploration helpers
# -----------------------------

# Once you’ve pasted in your full cleaning pipeline and run this script,
# you can use the following to inspect the latest-years data:

# Basic structure
# glimpse(cps)

# Check counts by year
# cps %>% count(YEAR)

# Check weighted mean utility burden by year
# cps %>%
#   group_by(YEAR) %>%
#   summarise(
#     n      = n(),
#     util_w = weighted.mean(util_burden, HHWT, na.rm = TRUE)
#   )

# Check distribution of water burden
# cps %>%
#   ggplot(aes(x = water_burden)) +
#   geom_histogram(binwidth = 0.01) +
#   coord_cartesian(xlim = c(0, 1))
