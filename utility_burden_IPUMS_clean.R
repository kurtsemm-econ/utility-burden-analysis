# Packages
library(ipumsr)
library(survey)
library(srvyr)
library(plyr)
library(dineq)
library(data.table)
library(tidyverse)    # includes dplyr, tibble, readr, ggplot2, purrr, scales, tidyr
library(reshape2)
library(readxl)
library(gridExtra)
library(knitr)
library(kableExtra)
library(frequency)
library(stargazer)
library(sf)
library(spatstat.explore)
library(viridis)
library(formattable)


# Import IPUMS data. R will crash if we try to do 2001-2023. 
# Do 3 different things and save the data.
# I am going to redo this because 2024 is now out, so we are going to do 2008-2016 & 2017-2024
# 2017-2023 is usa_00026
# 2009-2016 is usa_00027
# is usa_00028
cps_ddi <- read_ipums_ddi("usa_00027.xml")
cps_data <- read_ipums_micro(cps_ddi, data_file = "usa_00027.dat.gz", verbose = FALSE)


## Clean data: water cost and income > 1, non-farm, full plumbing, SNAP, household head, COUNTYFIP not missing
cps_data <- cps_data %>%
  filter(
    COSTWATR >= 1,
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

# CPI data
CPI <- read_excel("CPI.xlsx")

cpi <- CPI %>%
  dplyr::rename(
    YEAR   = Year,
    Annual = `Annual Average CPI`
  ) %>%
  dplyr::mutate(
    YEAR = str_replace_all(YEAR, "[^0-9]", ""),
    YEAR = as.integer(YEAR)
  ) %>%
  dplyr::select(YEAR, Annual)

# Combine CPI with CPS & adjust for inflation (base year CPI = 100)
cps_usa <- cps_data %>%
  left_join(cpi, by = "YEAR") %>%
  mutate(
    HHINCOME = (HHINCOME / Annual) * 100,
    COSTWATR = (COSTWATR / Annual) * 100,
    COSTELEC = (COSTELEC / Annual) * 100,
    COSTGAS  = (COSTGAS  / Annual) * 100
  )

# Adjust for household size (Pew-style equivalence scale with exponent n)
n <- 0.5

cps_usa <- cps_usa %>%
  mutate(
    HHINCOME = HHINCOME / (PERNUM^n),
    COSTWATR = COSTWATR / (PERNUM^n),
    COSTELEC = COSTELEC / (PERNUM^n),
    COSTGAS  = COSTGAS  / (PERNUM^n)
  ) %>%
  mutate(
    util_burden  = (COSTWATR + COSTELEC + COSTGAS) / HHINCOME,
    elec_burden  = COSTELEC / HHINCOME,
    gas_burden   = COSTGAS  / HHINCOME,
    water_burden = COSTWATR / HHINCOME
  )

#### Create upper bound for histogram (98th percentile) ####

# Utility burden
util_pctile <- cps_usa %>%
  group_by(YEAR) %>%
  do(util_98 = reldist::wtd.quantile(.$util_burden, q = 0.98, weight = .$HHWT)) %>%
  mutate(util_98 = unlist(util_98)) %>%
  as.data.frame()

# Water burden
water_pctile <- cps_usa %>%
  group_by(YEAR) %>%
  do(water_98 = reldist::wtd.quantile(.$water_burden, q = 0.98, weight = .$HHWT)) %>%
  mutate(water_98 = unlist(water_98)) %>%
  as.data.frame()

# Electricity burden
elec_pctile <- cps_usa %>%
  group_by(YEAR) %>%
  do(elec_98 = reldist::wtd.quantile(.$elec_burden, q = 0.98, weight = .$HHWT)) %>%
  mutate(elec_98 = unlist(elec_98)) %>%
  as.data.frame()

# Gas burden
gas_pctile <- cps_usa %>%
  group_by(YEAR) %>%
  do(gas_98 = reldist::wtd.quantile(.$gas_burden, q = 0.98, weight = .$HHWT)) %>%
  mutate(gas_98 = unlist(gas_98)) %>%
  as.data.frame()

# Join percentiles to cps_usa
cps_usa <- cps_usa %>%
  left_join(util_pctile,  by = "YEAR") %>%
  left_join(water_pctile, by = "YEAR") %>%
  left_join(elec_pctile,  by = "YEAR") %>%
  left_join(gas_pctile,   by = "YEAR")

util_list <- c("util_burden", "elec_burden", "gas_burden", "water_burden")

#### Head of household social identity ####

# Age groups
cps_usa <- cps_usa %>%
  mutate(
    AGE     = as.numeric(AGE),
    AGE_GRP = case_when(
      AGE >= 1  & AGE <=  9 ~ "09 and Less",
      AGE >  9  & AGE <= 19 ~ "19 and Less",
      AGE >  19 & AGE <= 29 ~ "29 and Less",
      AGE >  29 & AGE <= 39 ~ "39 and Less",
      AGE >  39 & AGE <= 49 ~ "49 and Less",
      AGE >  49 & AGE <= 59 ~ "59 and Less",
      AGE >  59 & AGE <= 69 ~ "69 and Less",
      AGE >  69 & AGE <= 79 ~ "79 and Less",
      AGE >  79 & AGE <= 89 ~ "89 and Less",
      AGE >= 89             ~ "89 & More",
      TRUE                  ~ NA_character_
    )
  )

# Education categories
cps_usa <- cps_usa %>%
  mutate(
    EDUC = as_factor(EDUC),
    year = as.factor(YEAR),
    EDUC4 = factor(case_when(
      EDUC %in% c(
        "N/A or no schooling", "Nursery school to grade 4",
        "Grade 5, 6, 7, or 8", "Grade 9", "Grade 10", "Grade 11"
      ) ~ "A: Less that HS",
      EDUC == "Grade 12" ~ "B: HS Finished",
      EDUC %in% c("1 year of college", "2 years of college", "3 years of college") ~ "C: Some College",
      EDUC %in% c("4 years of college", "5+ years of college") ~ "D: College Grad",
      TRUE ~ NA_character_
    ))
  )

# Race/ethnicity and identity
cps_usa <- cps_usa %>%
  mutate(
    RACE   = as_factor(RACE),
    HISPAN = as_factor(lbl_na_if(HISPAN, ~ .val == 9)),
    SEX    = as_factor(SEX)
  ) %>%
  mutate(
    RACE_ETHD = case_when(
      # Hispanic overrides race
      HISPAN %in% c("Mexican", "Puerto Rican", "Cuban", "Other") ~ "hispanic",

      # Non-Hispanic single-race categories
      HISPAN == "Not Hispanic" & RACE == "White"                          ~ "white",
      HISPAN == "Not Hispanic" & RACE == "Black/African American"         ~ "black",
      HISPAN == "Not Hispanic" & RACE == "American Indian or Alaska Native" ~ "american indian",

      # All Asian groups (codes 4,5,6)
      HISPAN == "Not Hispanic" & RACE %in% c("Chinese",
                                             "Japanese",
                                             "Other Asian or Pacific Islander") ~ "asian",

      # Other + multiracial (codes 7,8,9)
      HISPAN == "Not Hispanic" & RACE %in% c("Other race, nec",
                                             "Two major races",
                                             "Three or more major races") ~ "other",

      TRUE ~ NA_character_
    ),
    RACE_ETH = factor(RACE_ETHD)
  ) %>%
  mutate(
    IDENTITY = factor(case_when(
      RACE_ETH == "white"          & SEX == "Male"   ~ "White Men",
      RACE_ETH == "white"          & SEX == "Female" ~ "White Women",
      RACE_ETH == "black"          & SEX == "Male"   ~ "Black Men",
      RACE_ETH == "black"          & SEX == "Female" ~ "Black Women",
      RACE_ETH == "hispanic"       & SEX == "Male"   ~ "Hispanic Men",
      RACE_ETH == "hispanic"       & SEX == "Female" ~ "Hispanic Women",
      RACE_ETH == "american indian" & SEX == "Male"   ~ "American Indian Men",
      RACE_ETH == "american indian" & SEX == "Female" ~ "American Indian Women",
      RACE_ETH == "asian"          & SEX == "Male"   ~ "Asian Men",
      RACE_ETH == "asian"          & SEX == "Female" ~ "Asian Women",
      RACE_ETH == "other"          & SEX == "Male"   ~ "Other Men",
      RACE_ETH == "other"          & SEX == "Female" ~ "Other Women",
      TRUE ~ NA_character_
    ))
  )
# Robin Run code to here. 
saveRDS(cps_usa, "17_23_cps_usa.rds")
# saveRDS(cps_usa, "09_16_cps_usa.rds")


# Load US Data
df1<-readRDS("09_16_cps_usa.rds")
df2<-readRDS("17_23_cps_usa.rds")


vars_keep <- c(
  "YEAR", "HHWT", "CPUMA0010", "IDENTITY", "EDUC4",
  "util_burden", "util_98",
  "water_burden", "water_98",
  "elec_burden", "elec_98",
  "gas_burden", "gas_98"
)

df1_sub <- df1 %>% dplyr::select(dplyr::any_of(vars_keep))
df2_sub <- df2 %>% dplyr::select(dplyr::any_of(vars_keep))

cps_usa_all <- dplyr::bind_rows(df2_sub, df1_sub)

cps_entropy <- cps_usa_all %>%
  dplyr::filter(
    !is.na(IDENTITY),
    !is.na(util_burden),
    !is.na(util_98),
    !is.na(CPUMA0010),
    !is.na(HHWT)
  ) %>%
  dplyr::select(
    YEAR, HHWT, IDENTITY, EDUC4, CPUMA0010,
    util_burden, util_98
  )

#Sanity check
prop.table(table(cps_usa_all$IDENTITY))
prop.table(table(cps_entropy$IDENTITY))

