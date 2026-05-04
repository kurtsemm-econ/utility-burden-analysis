############################################################
# 01_build_entropy_ready.R
# Run once per IPUMS extract
############################################################

#### Packages ####
library(ipumsr)
library(tidyverse)
library(readxl)
library(reldist)
library(haven)

#### User inputs ####
ddi_file    <- "usa_00033.xml"
data_file   <- "usa_00033.dat.gz"
output_file <- "09_16_entropy_ready.rds"

#### Import IPUMS data ####
cps_ddi  <- read_ipums_ddi(ddi_file)
cps_data <- read_ipums_micro(cps_ddi, data_file = data_file, verbose = FALSE)

#### Recode utility special values ####
# IPUMS utility-cost variables use large numeric codes for values such as
# "included in rent", "no charge", or "not used". Those should not be treated
# as dollar expenditures in the burden numerator.
recode_utility_cost <- function(x) {
  dplyr::case_when(
    x >= 99992 ~ 0,
    x == 0 ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
}

cps_data <- cps_data %>%
  dplyr::mutate(
    COSTWATR = recode_utility_cost(COSTWATR),
    COSTELEC = recode_utility_cost(COSTELEC),
    COSTGAS  = recode_utility_cost(COSTGAS)
  )

#### Clean data ####
cps_data <- cps_data %>%
  dplyr::filter(
    HHINCOME >= 1,
    FARM == 1,
    PLUMBING == 20,
    SINK == 2,
    SHOWER == 4,
    HOTWATER == 4,
    FOODSTMP == 1,
    RELATE == 1,
    !is.na(COUNTYFIP)
  )

#### CPI data ####
CPI <- read_excel("CPI.xlsx")

cpi <- CPI %>%
  dplyr::rename(
    YEAR   = Year,
    Annual = `Annual Average CPI`
  ) %>%
  dplyr::mutate(
    YEAR = stringr::str_replace_all(YEAR, "[^0-9]", ""),
    YEAR = as.integer(YEAR)
  ) %>%
  dplyr::select(YEAR, Annual)

#### Inflation adjustment ####
cps_usa <- cps_data %>%
  dplyr::left_join(cpi, by = "YEAR") %>%
  dplyr::mutate(
    HHINCOME = (HHINCOME / Annual) * 100,
    COSTWATR = (COSTWATR / Annual) * 100,
    COSTELEC = (COSTELEC / Annual) * 100,
    COSTGAS  = (COSTGAS  / Annual) * 100
  )

#### Household-size adjustment ####
n <- 0.5

if ("NUMPREC" %in% names(cps_usa)) {
  cps_usa <- cps_usa %>%
    dplyr::mutate(
      HHINCOME = HHINCOME / (NUMPREC ^ n),
      COSTWATR = COSTWATR / (NUMPREC ^ n),
      COSTELEC = COSTELEC / (NUMPREC ^ n),
      COSTGAS  = COSTGAS  / (NUMPREC ^ n)
    )
}

cps_usa <- cps_usa %>%
  dplyr::mutate(
    util_burden  = (COSTWATR + COSTELEC + COSTGAS) / HHINCOME,
    elec_burden  = COSTELEC / HHINCOME,
    gas_burden   = COSTGAS  / HHINCOME,
    water_burden = COSTWATR / HHINCOME
  )

#### Upper bounds for histograms (year-specific 98th percentile) ####
util_pctile <- cps_usa %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    util_98 = reldist::wtd.quantile(util_burden, q = 0.98, weight = HHWT),
    .groups = "drop"
  )

water_pctile <- cps_usa %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    water_98 = reldist::wtd.quantile(water_burden, q = 0.98, weight = HHWT),
    .groups = "drop"
  )

elec_pctile <- cps_usa %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    elec_98 = reldist::wtd.quantile(elec_burden, q = 0.98, weight = HHWT),
    .groups = "drop"
  )

gas_pctile <- cps_usa %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    gas_98 = reldist::wtd.quantile(gas_burden, q = 0.98, weight = HHWT),
    .groups = "drop"
  )

cps_usa <- cps_usa %>%
  dplyr::left_join(util_pctile,  by = "YEAR") %>%
  dplyr::left_join(water_pctile, by = "YEAR") %>%
  dplyr::left_join(elec_pctile,  by = "YEAR") %>%
  dplyr::left_join(gas_pctile,   by = "YEAR")

#### Age groups ####
cps_usa <- cps_usa %>%
  dplyr::mutate(
    AGE = as.numeric(AGE),
    AGE_GRP = dplyr::case_when(
      AGE >= 15 & AGE <= 19 ~ "15-19",
      AGE >= 20 & AGE <= 29 ~ "20-29",
      AGE >= 30 & AGE <= 39 ~ "30-39",
      AGE >= 40 & AGE <= 49 ~ "40-49",
      AGE >= 50 & AGE <= 59 ~ "50-59",
      AGE >= 60 & AGE <= 69 ~ "60-69",
      AGE >= 70 & AGE <= 79 ~ "70-79",
      AGE >= 80             ~ "80+",
      TRUE                  ~ NA_character_
    )
  )

#### Education categories ####
cps_usa <- cps_usa %>%
  dplyr::mutate(
    EDUC = ipumsr::as_factor(EDUC),
    EDUC4 = dplyr::case_when(
      EDUC %in% c(
        "N/A or no schooling",
        "Nursery school to grade 4",
        "Grade 5, 6, 7, or 8",
        "Grade 9",
        "Grade 10",
        "Grade 11"
      ) ~ "A: Less than HS",
      EDUC == "Grade 12" ~ "B: HS Finished",
      EDUC %in% c(
        "1 year of college",
        "2 years of college",
        "3 years of college"
      ) ~ "C: Some College",
      EDUC %in% c(
        "4 years of college",
        "5+ years of college"
      ) ~ "D: College Grad",
      TRUE ~ NA_character_
    ),
    EDUC4 = factor(
      EDUC4,
      levels = c(
        "A: Less than HS",
        "B: HS Finished",
        "C: Some College",
        "D: College Grad"
      )
    )
  )

#### Race, ethnicity, and identity ####
cps_usa <- cps_usa %>%
  dplyr::mutate(
    RACE   = ipumsr::as_factor(RACE),
    HISPAN = ipumsr::as_factor(ipumsr::lbl_na_if(HISPAN, ~ .val == 9)),
    SEX    = ipumsr::as_factor(SEX)
  ) %>%
  dplyr::mutate(
    RACE_ETHD = dplyr::case_when(
      HISPAN %in% c("Mexican", "Puerto Rican", "Cuban", "Other") ~ "hispanic",
      HISPAN == "Not Hispanic" & RACE == "White" ~ "white",
      HISPAN == "Not Hispanic" & RACE == "Black/African American" ~ "black",
      HISPAN == "Not Hispanic" & RACE == "American Indian or Alaska Native" ~ "american indian",
      HISPAN == "Not Hispanic" & RACE %in% c(
        "Chinese", "Japanese", "Other Asian or Pacific Islander"
      ) ~ "asian",
      HISPAN == "Not Hispanic" & RACE %in% c(
        "Other race, nec", "Two major races", "Three or more major races"
      ) ~ "other",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    IDENTITY = dplyr::case_when(
      RACE_ETHD == "white"           & SEX == "Male"   ~ "White Men",
      RACE_ETHD == "white"           & SEX == "Female" ~ "White Women",
      RACE_ETHD == "black"           & SEX == "Male"   ~ "Black Men",
      RACE_ETHD == "black"           & SEX == "Female" ~ "Black Women",
      RACE_ETHD == "hispanic"        & SEX == "Male"   ~ "Hispanic Men",
      RACE_ETHD == "hispanic"        & SEX == "Female" ~ "Hispanic Women",
      RACE_ETHD == "american indian" & SEX == "Male"   ~ "American Indian Men",
      RACE_ETHD == "american indian" & SEX == "Female" ~ "American Indian Women",
      RACE_ETHD == "asian"           & SEX == "Male"   ~ "Asian Men",
      RACE_ETHD == "asian"           & SEX == "Female" ~ "Asian Women",
      RACE_ETHD == "other"           & SEX == "Male"   ~ "Other Men",
      RACE_ETHD == "other"           & SEX == "Female" ~ "Other Women",
      TRUE ~ NA_character_
    ),
    IDENTITY = factor(IDENTITY)
  )

#### Keep only entropy-ready variables ####
entropy_ready <- cps_usa %>%
  dplyr::mutate(
    YEAR = as.integer(haven::zap_labels(YEAR)),
    HHWT = as.numeric(haven::zap_labels(HHWT))
  )

if ("COUNTYFIP" %in% names(entropy_ready)) {
  entropy_ready <- entropy_ready %>%
    dplyr::mutate(COUNTYFIP = as.integer(haven::zap_labels(COUNTYFIP)))
}

if ("CONSPUMA" %in% names(entropy_ready)) {
  entropy_ready <- entropy_ready %>%
    dplyr::mutate(CONSPUMA = as.integer(haven::zap_labels(CONSPUMA)))
}

if ("CPUMA0010" %in% names(entropy_ready)) {
  entropy_ready <- entropy_ready %>%
    dplyr::mutate(CPUMA0010 = as.integer(haven::zap_labels(CPUMA0010)))
}

if ("PUMA" %in% names(entropy_ready)) {
  entropy_ready <- entropy_ready %>%
    dplyr::mutate(PUMA = as.integer(haven::zap_labels(PUMA)))
}

entropy_ready <- entropy_ready %>%
  dplyr::select(
    dplyr::any_of(c(
      "YEAR", "HHWT", "COUNTYFIP", "PUMA", "CONSPUMA", "CPUMA0010",
      "IDENTITY", "EDUC4", "AGE_GRP",
      "util_burden", "util_98",
      "water_burden", "water_98",
      "elec_burden", "elec_98",
      "gas_burden", "gas_98"
    ))
  )

#### Save compact file ####
saveRDS(entropy_ready, output_file)

#### Quick checks ####
print(summary(entropy_ready$YEAR))
print(prop.table(table(entropy_ready$IDENTITY)))

check_summary <- entropy_ready %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    n = dplyr::n(),
    miss_util98 = sum(is.na(util_98)),
    .groups = "drop"
  )

if ("COUNTYFIP" %in% names(entropy_ready)) {
  check_summary <- check_summary %>%
    dplyr::left_join(
      entropy_ready %>%
        dplyr::group_by(YEAR) %>%
        dplyr::summarise(miss_county = sum(is.na(COUNTYFIP)), .groups = "drop"),
      by = "YEAR"
    )
}

if ("CPUMA0010" %in% names(entropy_ready)) {
  check_summary <- check_summary %>%
    dplyr::left_join(
      entropy_ready %>%
        dplyr::group_by(YEAR) %>%
        dplyr::summarise(miss_cpuma = sum(is.na(CPUMA0010)), .groups = "drop"),
      by = "YEAR"
    )
}

if ("PUMA" %in% names(entropy_ready)) {
  check_summary <- check_summary %>%
    dplyr::left_join(
      entropy_ready %>%
        dplyr::group_by(YEAR) %>%
        dplyr::summarise(miss_puma = sum(is.na(PUMA)), .groups = "drop"),
      by = "YEAR"
    )
}

print(check_summary)
