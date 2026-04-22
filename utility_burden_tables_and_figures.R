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