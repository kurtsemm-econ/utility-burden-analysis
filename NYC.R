#### Packages ####
library(tidyverse)
library(reshape2)
library(spatstat.explore)
library(forcats)

#### Load US Data ####
df1 <- readRDS("09_16_cps_usa.rds")
df2 <- readRDS("17_23_cps_usa.rds")

vars_keep <- c(
  "YEAR", "HHWT", "CPUMA0010", "IDENTITY", "EDUC4", "AGE_GRP",
  "util_burden", "util_98",
  "water_burden", "water_98",
  "elec_burden", "elec_98",
  "gas_burden", "gas_98"
)

df1_sub <- df1 %>% dplyr::select(dplyr::any_of(vars_keep))
df2_sub <- df2 %>% dplyr::select(dplyr::any_of(vars_keep))

cps_usa_all <- dplyr::bind_rows(df2_sub, df1_sub)

#### Basic cleaning / consistency ####
cps_entropy <- cps_usa_all %>%
  dplyr::filter(
    !is.na(YEAR),
    !is.na(HHWT),
    !is.na(IDENTITY),
    !is.na(util_burden),
    !is.na(util_98)
  ) %>%
  dplyr::mutate(
    IDENTITY = as.character(IDENTITY),
    EDUC4 = as.character(EDUC4),
    EDUC4 = dplyr::case_when(
      EDUC4 == "A: Less that HS" ~ "A: Less than HS",
      TRUE ~ EDUC4
    ),
    EDUC4 = factor(
      EDUC4,
      levels = c(
        "A: Less than HS",
        "B: HS Finished",
        "C: Some College",
        "D: College Grad"
      )
    ),
    AGE_GRP = as.character(AGE_GRP),
    #CPUMA0010 = as.integer(haven::zap_labels(CPUMA0010))
  )


