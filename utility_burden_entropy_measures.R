############################################################
# entropy_measures.R
# Main specification: YEAR x IDENTITY x CPUMA0010
# Robustness checks: YEAR x IDENTITY x EDUC4
#                    YEAR x IDENTITY x AGE_GRP
############################################################

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

#### Entropy measure functions ####

ent <- function(x) {
  -sum(ifelse(x > 0, x * log(x), 0))
}

H_X <- function(D) {
  ent(prop.table(apply(D, MARGIN = 1, FUN = sum)))
}

H_z <- function(D) {
  X_cond_Z <- apply(D, MARGIN = c(1, 3), FUN = sum)
  f_X_cond_Z <- prop.table(X_cond_Z, margin = 2)
  apply(f_X_cond_Z, MARGIN = 2, FUN = ent)
}

H_y <- function(D) {
  X_cond_Y <- apply(D, MARGIN = c(1, 2), FUN = sum)
  f_X_cond_Y <- prop.table(X_cond_Y, margin = 2)
  apply(f_X_cond_Y, MARGIN = 2, FUN = ent)
}

H_z_y <- function(D) {
  X_cond_Z_Y <- prop.table(D, margin = c(2, 3))
  apply(X_cond_Z_Y, MARGIN = c(2, 3), FUN = ent)
}

#### Histogram helper functions ####

freqr <- function(var, top, weight, k = 34) {
  top_scalar <- as.numeric(top[1])

  if (!is.finite(top_scalar) || top_scalar <= 0) {
    return(rep(NA_real_, k - 1))
  }

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)

  as.numeric(
    spatstat.univar::whist(
      x = var,
      breaks = breaks_flex,
      weights = weight
    )
  )
}

midr <- function(var, top, weight, k = 34) {
  top_scalar <- as.numeric(top[1])

  if (!is.finite(top_scalar) || top_scalar <= 0) {
    return(rep(NA_real_, k - 1))
  }

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)
  breaks_flex[-1] - diff(breaks_flex) / 2
}

Nr <- function(var, top, k = 34) {
  top_scalar <- as.numeric(top[1])

  if (!is.finite(top_scalar) || top_scalar <= 0) {
    return(rep(NA_integer_, k - 1))
  }

  breaks_flex <- seq.int(from = 0, to = top_scalar, length.out = k)
  var_trim <- var[var >= 0 & var <= top_scalar]

  hist(var_trim, breaks = breaks_flex, plot = FALSE)$counts
}

#### Build histogram object ####

build_entropy_histogram <- function(data, group_var, k = 34) {
  group_sym <- rlang::sym(group_var)

  data %>%
    dplyr::filter(
      !is.na(IDENTITY),
      !is.na(util_burden),
      !is.na(util_98),
      !is.na(HHWT),
      !is.na(!!group_sym)
    ) %>%
    dplyr::group_by(YEAR, IDENTITY, !!group_sym, .drop = FALSE) %>%
    dplyr::reframe(
      freq       = freqr(util_burden, util_98, HHWT, k = k),
      mid_burden = midr(util_burden, util_98, HHWT, k = k),
      N          = Nr(util_burden, util_98, k = k)
    ) %>%
    dplyr::filter(!all(is.na(freq))) %>%
    dplyr::group_by(YEAR, IDENTITY, !!group_sym) %>%
    dplyr::mutate(
      rel_freq = freq / sum(freq, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

#### Thin-cell filter ####

filter_histogram_cells <- function(hist_data, group_var, min_unweighted_n = 30) {
  group_sym <- rlang::sym(group_var)

  hist_data %>%
    dplyr::group_by(YEAR, IDENTITY, !!group_sym) %>%
    dplyr::mutate(
      cell_N = sum(N, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(cell_N >= min_unweighted_n)
}

#### Array builder ####

build_D_array <- function(d, x_var, y_var, z_var) {
  reshape2::acast(
    d,
    formula = stats::as.formula(paste(x_var, y_var, z_var, sep = " ~ ")),
    value.var = "rel_freq",
    fun.aggregate = sum,
    fill = 0
  )
}

#### Information measures ####

A_X_z <- function(d, x_var, y_var, z_var) {
  D <- build_D_array(d, x_var, y_var, z_var)
  ret <- 1 - H_z(D) / H_X(D)

  out <- data.frame(A_X_z = ret, row.names = NULL)
  out[[z_var]] <- names(ret)
  out
}

A_X_y <- function(d, x_var, y_var, z_var) {
  D <- build_D_array(d, x_var, y_var, z_var)
  ret <- 1 - H_y(D) / H_X(D)

  out <- data.frame(A_X_y = ret, row.names = NULL)
  out[[y_var]] <- names(ret)
  out
}

I_z_y <- function(d, x_var, y_var, z_var) {
  D <- build_D_array(d, x_var, y_var, z_var)
  ret <- (H_y(D) - H_z_y(D)) / H_X(D)

  out <- as.data.frame(ret, row.names = NULL)
  out[[y_var]] <- rownames(ret)

  out_long <- tidyr::pivot_longer(
    out,
    cols = -all_of(y_var),
    names_to = z_var,
    values_to = "I_z_y"
  )

  out_long
}

I_y_z <- function(d, x_var, y_var, z_var) {
  D <- build_D_array(d, x_var, y_var, z_var)
  ret <- t((H_z(D) - t(H_z_y(D))) / H_X(D))

  out <- as.data.frame(ret, row.names = NULL)
  out[[y_var]] <- rownames(ret)

  out_long <- tidyr::pivot_longer(
    out,
    cols = -all_of(y_var),
    names_to = z_var,
    values_to = "I_y_z"
  )

  out_long
}

M_x_y_z <- function(d, x_var, y_var, z_var) {
  D <- build_D_array(d, x_var, y_var, z_var)
  ret <- (H_X(D) - H_y(D) - t(H_z(D) - t(H_z_y(D)))) / H_X(D)

  out <- as.data.frame(ret, row.names = NULL)
  out[[y_var]] <- rownames(ret)

  out_long <- tidyr::pivot_longer(
    out,
    cols = -all_of(y_var),
    names_to = z_var,
    values_to = "M_x_y_z"
  )

  out_long
}

#### Year-by-year runner ####

run_info_by_year <- function(hist_data, y_var, z_var, x_var = "rel_freq") {
  hist_data %>%
    dplyr::group_by(YEAR) %>%
    dplyr::group_modify(~{
      d <- .x

      A_y <- A_X_y(d, x_var, y_var, z_var)
      A_z <- A_X_z(d, x_var, y_var, z_var)
      Izy <- I_z_y(d, x_var, y_var, z_var)
      Iyz <- I_y_z(d, x_var, y_var, z_var)
      M   <- M_x_y_z(d, x_var, y_var, z_var)

      Izy %>%
        dplyr::left_join(A_y, by = y_var) %>%
        dplyr::left_join(A_z, by = z_var) %>%
        dplyr::left_join(Iyz, by = c(y_var, z_var)) %>%
        dplyr::left_join(M,   by = c(y_var, z_var)) %>%
        dplyr::mutate(
          a_joint = A_X_y + I_z_y
        )
    }) %>%
    dplyr::ungroup()
}

rm(df1, df2, df1_sub, df2_sub, cps_usa_all)
gc()

#### Main specification: YEAR x IDENTITY x CPUMA0010 ####

entropy_histogram_puma <- build_entropy_histogram(
  data = cps_entropy,
  group_var = "CPUMA0010",
  k = 34
)

entropy_histogram_puma <- filter_histogram_cells(
  hist_data = entropy_histogram_puma,
  group_var = "CPUMA0010",
  min_unweighted_n = 200
)

info_by_year_puma <- run_info_by_year(
  hist_data = entropy_histogram_puma,
  y_var = "IDENTITY",
  z_var = "CPUMA0010"
)

#### Robustness: YEAR x IDENTITY x EDUC4 ####

entropy_histogram_educ <- build_entropy_histogram(
  data = cps_entropy %>% dplyr::filter(!is.na(EDUC4)),
  group_var = "EDUC4",
  k = 34
)

entropy_histogram_educ <- filter_histogram_cells(
  hist_data = entropy_histogram_educ,
  group_var = "EDUC4",
  min_unweighted_n = 30
)

info_by_year_educ <- run_info_by_year(
  hist_data = entropy_histogram_educ,
  y_var = "IDENTITY",
  z_var = "EDUC4"
)

#### Robustness: YEAR x IDENTITY x AGE_GRP ####

entropy_histogram_age <- build_entropy_histogram(
  data = cps_entropy %>% dplyr::filter(!is.na(AGE_GRP)),
  group_var = "AGE_GRP",
  k = 34
)

entropy_histogram_age <- filter_histogram_cells(
  hist_data = entropy_histogram_age,
  group_var = "AGE_GRP",
  min_unweighted_n = 30
)

info_by_year_age <- run_info_by_year(
  hist_data = entropy_histogram_age,
  y_var = "IDENTITY",
  z_var = "AGE_GRP"
)

#### Create folders if needed ####

if (!dir.exists("histogram_data")) {
  dir.create("histogram_data", recursive = TRUE)
}

if (!dir.exists("data_results")) {
  dir.create("data_results", recursive = TRUE)
}

#### Save outputs ####

saveRDS(entropy_histogram_puma, "histogram_data/entropy_histogram_puma.rds")
saveRDS(entropy_histogram_educ, "histogram_data/entropy_histogram_educ.rds")
saveRDS(entropy_histogram_age,  "histogram_data/entropy_histogram_age.rds")

saveRDS(info_by_year_puma, "data_results/info_by_year_puma.rds")
saveRDS(info_by_year_educ, "data_results/info_by_year_educ.rds")
saveRDS(info_by_year_age,  "data_results/info_by_year_age.rds")

#### Quick checks ####

print("Years in main specification:")
print(sort(unique(info_by_year_puma$YEAR)))

print("Years in education robustness check:")
print(sort(unique(info_by_year_educ$YEAR)))

print("Years in age robustness check:")
print(sort(unique(info_by_year_age$YEAR)))