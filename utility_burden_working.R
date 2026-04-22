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

#Check Years 
summary(cps_usa$YEAR)

#### Entropy measure functions ####

ent <- function(x) {           # Equation 1
  -sum(ifelse(x > 0, x * log(x), 0))
}

H_X <- function(D) {           # Equation 3
  ent(prop.table(apply(D, MARGIN = 1, FUN = sum)))
}

H_z <- function(D) {
  X_cond_Z  <- apply(D, MARGIN = c(1, 3), FUN = sum)
  f_X_cond_Z <- prop.table(X_cond_Z, margin = 2)
  apply(f_X_cond_Z, MARGIN = 2, FUN = ent)
}

H_y <- function(D) {           # Equation 2
  X_cond_Y  <- apply(D, MARGIN = c(1, 2), FUN = sum)
  f_X_cond_Y <- prop.table(X_cond_Y, margin = 2)
  apply(f_X_cond_Y, MARGIN = 2, FUN = ent)
}

H_z_y <- function(D) {         # Equation 5
  X_cond_Z_Y <- prop.table(D, margin = c(2, 3))
  apply(X_cond_Z_Y, MARGIN = c(2, 3), FUN = ent)
}

#### Functions for Histogram ####

freqr <- function(var, top, weight, k = 34) {
  # force top to a single finite number
  top_scalar <- as.numeric(top[1])
  if (!is.finite(top_scalar)) return(rep(NA_real_, k - 1))

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)
  as.numeric(spatstat.univar::whist(var, breaks = breaks_flex, weights = weight))
}

midr <- function(var, top, weight, k = 34) {
  top_scalar <- as.numeric(top[1])
  if (!is.finite(top_scalar)) return(rep(NA_real_, k - 1))

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)
  breaks_flex[-1] - diff(breaks_flex) / 2
}

Nr <- function(var, top, k = 34) {
  top_scalar <- as.numeric(top[1])
  if (!is.finite(top_scalar)) return(rep(NA_integer_, k - 1))

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)

  # restrict to the same range covered by the breaks
  var_trim <- var[var >= 0 & var <= top_scalar]

  hist(var_trim, breaks = breaks_flex, plot = FALSE)$counts
}

#### Calculate Histogram ####
set.seed(0308)

xyz_histogram <- cps_usa %>%
  dplyr::filter(!is.na(IDENTITY), !is.na(util_burden)) %>%
  dplyr::group_by(YEAR, SEX, EDUC4, IDENTITY, .drop = FALSE) %>%
  dplyr::reframe(
    freq       = freqr(util_burden, util_98, HHWT),  # weighted counts in bins
    mid_burden = midr(util_burden, util_98, HHWT),  # bin midpoints
    N          = Nr(util_burden, util_98)           # unweighted counts
  ) %>%
  dplyr::group_by(YEAR, SEX, EDUC4, IDENTITY) %>%
  dplyr::filter(!all(is.na(freq))) %>%
  # IMPORTANT: normalize WITHIN each subgroup (YEAR × EDUC4 × IDENTITY)
  dplyr::group_by(YEAR, EDUC4, IDENTITY, .add = FALSE) %>%
  dplyr::mutate(
    rel_freq = freq / sum(freq, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Sex = factor(dplyr::case_when(
      IDENTITY %in% c(
        "Black Men", "Hispanic Men", "Asian Men",
        "American Indian Men", "Other Men", "White Men"
      ) ~ "Men",
      TRUE ~ "Women"
    )),
    Ethnicity = factor(dplyr::case_when(
      IDENTITY %in% c("Black Men", "Black Women")                     ~ "Black",
      IDENTITY %in% c("Hispanic Men", "Hispanic Women")               ~ "Hispanic",
      IDENTITY %in% c("Asian Men", "Asian Women")                     ~ "Asian",
      IDENTITY %in% c("American Indian Men", "American Indian Women") ~ "American Indian",
      IDENTITY %in% c("Other Men", "Other Women")                     ~ "Other",
      TRUE                                                            ~ "White"
    ))
  )

# 1. Create the folder if it doesn't exist
if (!dir.exists("histogram_data")) {
  dir.create("histogram_data")
}


# 2. Save your object into that folder
saveRDS(xyz_histogram, file = "histogram_data/xyz_histogram_2017_2023.rds")
saveRDS(xyz_histogram, file = "histogram_data/xyz_histogram_2009_2016.rds")
saveRDS(xyz_histogram, file = "histogram_data/xyz_histogram_2017_2023.rds")

#### Information measures ####

A_X_z <- function(d, x, y, z) {   # Informational association of Z and X (Eq. 6)
  form <- deparse(substitute(x ~ y ~ z))
  D    <- reshape2::acast(d, formula = form, value.var = "freq", fun.aggregate = length)
  ret  <- 1 - H_z(D) / H_X(D)
  ret2 <- data.frame(A_X_z = ret, row.names = NULL)
  ret2$temp <- as.character(names(ret))
  names(ret2)[2] <- deparse(substitute(z))
  ret2
}

A_X_y <- function(d, x, y, z) {   # Informational association of Y and X (Eq. 6)
  form <- deparse(substitute(x ~ y ~ z))
  D    <- reshape2::acast(d, formula = form, value.var = "freq", fun.aggregate = length)
  ret  <- 1 - H_y(D) / H_X(D)
  ret2 <- data.frame(A_X_y = ret, row.names = NULL)
  ret2$temp <- as.character(names(ret))
  names(ret2)[2] <- deparse(substitute(y))
  ret2
}

I_z_y <- function(d, x, y, z) {   # Incremental info assoc of Z, X when Y is known
  form <- deparse(substitute(x ~ y ~ z))
  D    <- reshape2::acast(d, formula = form, value.var = "freq", fun.aggregate = length)
  ret  <- (H_y(D) - H_z_y(D)) / H_X(D)
  ret2 <- data.frame(ret, row.names = NULL)
  ret2$temp <- as.character(rownames(ret))
  ret3 <- gather(ret2, key = "temp2", value = "I_z_y", -temp)
  names(ret3)[1] <- deparse(substitute(y))
  names(ret3)[2] <- deparse(substitute(z))
  ret3
}

I_y_z <- function(d, x, y, z) {   # Incremental info assoc of Y, X when Z is known
  form <- deparse(substitute(x ~ y ~ z))
  D    <- reshape2::acast(d, formula = form, value.var = "freq", fun.aggregate = length)
  ret  <- t((H_z(D) - t(H_z_y(D))) / H_X(D))
  ret2 <- data.frame(ret, row.names = NULL)
  ret2$temp <- as.character(rownames(ret))
  ret3 <- gather(ret2, key = "temp2", value = "I_y_z", -temp)
  names(ret3)[1] <- deparse(substitute(y))
  names(ret3)[2] <- deparse(substitute(z))
  ret3
}

M_x_y_z <- function(d, x, y, z) { # Interaction information of Y, Z for X
  form <- deparse(substitute(x ~ y ~ z))
  D    <- reshape2::acast(d, formula = form, value.var = "freq", fun.aggregate = length)
  ret  <- (H_X(D) - H_y(D) - t(H_z(D) - t(H_z_y(D)))) / H_X(D)
  ret2 <- data.frame(ret, row.names = NULL)
  ret2$temp <- as.character(rownames(ret))
  ret3 <- gather(ret2, key = "temp2", value = "M_x_y_z", -temp)
  names(ret3)[1] <- deparse(substitute(y))
  names(ret3)[2] <- deparse(substitute(z))
  ret3
}

# Make sure EDUC4 is ordered nicely
cps_usa <- cps_usa %>%
  mutate(
    EDUC4 = fct_relevel(
      EDUC4,
      "A: Less than HS",
      "B: HS Finished",
      "C: Some College",
      "D: College Grad"
    ),
    year = as.factor(YEAR)
  )

#### Plotting Figures ####

# Merge histogram data. 
# Change from 2008-2024
xyz_2009_2016 <- readRDS("histogram_data/xyz_histogram_2009_2016.rds")
xyz_2017_2023 <- readRDS("histogram_data/xyz_histogram_2017_2023.rds")

xyz_histogram_all <- dplyr::bind_rows(
  xyz_2009_2016,
  xyz_2017_2023
) %>%
  dplyr::filter(YEAR >= 2009)  # just to be explicit

# quick sanity check
dplyr::count(xyz_histogram_all, YEAR)
############################################
# Figure 6: Overall distribution of EDUC4  #
############################################

fig6_educ_overall <- cps_usa %>%
  dplyr::filter(!is.na(EDUC4)) %>%
  dplyr::count(EDUC4) %>%
  dplyr::mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = EDUC4, y = pct)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Education (4-category)",
    y = "Share of sample",
    title = "Figure 6. Distribution of educational attainment (EDUC4)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

fig6_educ_overall

###########################################################
# Figure 7: EDUC4 distribution over time (stacked area)   #
###########################################################

fig7_educ_over_time <- cps_usa %>%
  dplyr::filter(!is.na(EDUC4)) %>%
  dplyr::count(YEAR, EDUC4) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(pct = n / sum(n)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = YEAR, y = pct, fill = EDUC4, group = EDUC4)) +
  geom_area(position = "stack") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share of sample",
    title = "Figure 7. Educational attainment shares over time",
    fill = "Education (EDUC4)"
  ) +
  theme_minimal(base_size = 12)

fig7_educ_over_time


info_by_year_ub <- xyz_histogram_all %>%
  dplyr::mutate(
    EDUC4   = forcats::fct_recode(EDUC4, "A: Less than HS" = "A: Less that HS"),
    IDENTITY = as.character(IDENTITY)
  ) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::group_modify(~{
    d <- .x

    A_y  <- A_X_y(d, rel_freq, EDUC4, IDENTITY)
    A_z  <- A_X_z(d, rel_freq, EDUC4, IDENTITY)
    Izy  <- I_z_y(d, rel_freq, EDUC4, IDENTITY)
    Iyz  <- I_y_z(d, rel_freq, EDUC4, IDENTITY)
    M    <- M_x_y_z(d, rel_freq, EDUC4, IDENTITY)

    if ("y" %in% names(A_y))  A_y  <- dplyr::rename(A_y,  EDUC4   = y)
    if ("z" %in% names(A_z))  A_z  <- dplyr::rename(A_z,  IDENTITY = z)
    if ("y" %in% names(Izy))  Izy  <- dplyr::rename(Izy,  EDUC4   = y)
    if ("z" %in% names(Izy))  Izy  <- dplyr::rename(Izy,  IDENTITY = z)
    if ("y" %in% names(Iyz))  Iyz  <- dplyr::rename(Iyz,  EDUC4   = y)
    if ("z" %in% names(Iyz))  Iyz  <- dplyr::rename(Iyz,  IDENTITY = z)
    if ("y" %in% names(M))    M    <- dplyr::rename(M,    EDUC4   = y)
    if ("z" %in% names(M))    M    <- dplyr::rename(M,    IDENTITY = z)

    # optional: normalise IDENTITY just once here if needed
    clean_id <- function(x) trimws(gsub("\\.", " ", x))
    Izy <- Izy %>% dplyr::mutate(IDENTITY = clean_id(IDENTITY))
    A_z <- A_z %>% dplyr::mutate(IDENTITY = clean_id(IDENTITY))
    Iyz <- Iyz %>% dplyr::mutate(IDENTITY = clean_id(IDENTITY))
    M   <- M   %>% dplyr::mutate(IDENTITY = clean_id(IDENTITY))

    out <- Izy %>%
      dplyr::left_join(A_y,  by = "EDUC4") %>%
      dplyr::left_join(A_z,  by = "IDENTITY") %>%
      dplyr::left_join(Iyz,  by = c("EDUC4", "IDENTITY")) %>%
      dplyr::left_join(M,    by = c("EDUC4", "IDENTITY")) %>%
      dplyr::mutate(a_joint = A_X_y + I_z_y)

    out
  }) %>%
  dplyr::ungroup()



fig9_ub <- info_by_year_ub %>%
  ggplot(aes(x = YEAR, y = M_x_y_z, color = IDENTITY, group = IDENTITY)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ EDUC4, scales = "free_y") +
  labs(
    title = "Tripartite Mutual Information: Utility Burden × Education × Identity",
    x = "Year",
    y = "M(util_burden, education, identity)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

fig9_ub


# identities to show in panels
ids_to_plot <- c(
  "Black Women", "Hispanic Women", "White Women",
  "Black Men",   "Hispanic Men",   "White Men"
)

plot_data <- info_by_year_ub %>%
  dplyr::filter(IDENTITY %in% ids_to_plot) %>%
  dplyr::mutate(
    # nicer facet labels
    IDENTITY_panel = gsub("\\.", " ", IDENTITY),
    YEAR  = as.factor(YEAR),
    # make sure education is ordered correctly
    EDUC4 = forcats::fct_recode(
      EDUC4,
      "A: Less than HS" = "A: Less that HS"
    ),
    EDUC4 = forcats::fct_relevel(
      EDUC4,
      "A: Less than HS",
      "B: HS Finished",
      "C: Some College",
      "D: College Grad"
    )
  )

key_years <- c(2009, 2013, 2017, 2020, 2023)

plot_data_key <- info_by_year_ub %>%
  dplyr::filter(
    IDENTITY %in% c(
      "Black Women", "Hispanic Women", "White Women",
      "Black Men",   "Hispanic Men",   "White Men"
    ),
    YEAR %in% key_years
  ) %>%
  dplyr::mutate(
    IDENTITY_panel = IDENTITY,
    YEAR  = factor(YEAR, levels = key_years),
    EDUC4 = forcats::fct_relevel(
      EDUC4,
      "A: Less than HS",
      "B: HS Finished",
      "C: Some College",
      "D: College Grad"
    )
  )

fig_wds_key <- plot_data_key %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x        = M_x_y_z,
      y        = a_joint,
      group    = YEAR,
      colour   = YEAR,
      linetype = YEAR,
      shape    = EDUC4
    )
  ) +
  ggplot2::geom_path() +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ IDENTITY_panel, ncol = 3) +
  ggplot2::scale_color_grey(start = 0.2, end = 0.7, name = "Year") +
  ggplot2::scale_linetype_discrete(name = "Year") +
  ggplot2::scale_shape_manual(
    values = c(
      "A: Less than HS" = 16,
      "B: HS Finished"  = 17,
      "C: Some College" = 15,
      "D: College Grad" = 3
    ),
    name = "Education"
  ) +
  ggplot2::labs(
    x = "M(util_burden, educ, ident)",
    y = "S(educ, ident)",
    title = "Tripartite Mutual Information: Utility Burden × Education × Identity"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

fig_wds_key

