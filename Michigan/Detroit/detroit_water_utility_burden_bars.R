library(tidyverse)
library(readxl)
library(scales)

# Detroit-area water utility burden bar chart, 2016-2024 where reliable.
# Detroit ConsPUMA geography is safely available in the cleaned RDS inputs for 2016-2021.

analysis_years <- 2016:2024
detroit_cpumas <- c(513L, 514L)
wayne_outside_detroit_cpumas <- c(507L, 508L, 509L, 510L, 511L, 512L)
min_n <- 10

dir.create("outputs", showWarnings = FALSE)

weighted_median <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]

  if (length(x) == 0 || sum(w) == 0) {
    return(NA_real_)
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

utility_data <- bind_rows(
  readRDS("09_16_entropy_ready.rds") %>%
    filter(YEAR %in% analysis_years) %>%
    mutate(source_file = "09_16_entropy_ready.rds"),
  readRDS("17_24_entropy_ready.rds") %>%
    filter(YEAR %in% analysis_years) %>%
    mutate(source_file = "17_24_entropy_ready.rds")
) %>%
  mutate(
    YEAR = as.integer(YEAR),
    CPUMA0010 = as.integer(CPUMA0010),
    HHWT = as.numeric(HHWT)
  )

area_cpumas <- c(detroit_cpumas, wayne_outside_detroit_cpumas)

available_years <- sort(unique(utility_data$YEAR))
available_geo_years <- utility_data %>%
  filter(CPUMA0010 %in% area_cpumas) %>%
  distinct(YEAR) %>%
  arrange(YEAR) %>%
  pull(YEAR)

missing_data_years <- setdiff(analysis_years, available_years)
missing_geo_years <- setdiff(analysis_years, available_geo_years)

if (length(missing_data_years) > 0) {
  message(
    "Missing requested years in cleaned RDS files: ",
    paste(missing_data_years, collapse = ", ")
  )
}

if (length(setdiff(missing_geo_years, missing_data_years)) > 0) {
  message(
    "Requested years present but not safely identifiable as Detroit/Wayne CPUMAs: ",
    paste(setdiff(missing_geo_years, missing_data_years), collapse = ", ")
  )
}

cpuma_components <- read_excel("CPUMA0010_PUMA2010_components.xls")

detroit_lookup <- cpuma_components %>%
  filter(State_Name == "Michigan", CPUMA0010 %in% area_cpumas) %>%
  mutate(
    CPUMA0010 = as.integer(CPUMA0010),
    Area = case_when(
      CPUMA0010 %in% detroit_cpumas ~ "Detroit city area",
      CPUMA0010 %in% wayne_outside_detroit_cpumas ~ "Wayne County outside Detroit",
      TRUE ~ PUMA_Name
    )
  ) %>%
  group_by(CPUMA0010, Area) %>%
  summarise(
    PUMA_components = str_c(PUMA_Name, collapse = " / "),
    .groups = "drop"
  )

bar_values <- utility_data %>%
  filter(CPUMA0010 %in% area_cpumas) %>%
  left_join(detroit_lookup, by = "CPUMA0010") %>%
  group_by(YEAR, Area) %>%
  summarise(
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_water_burden = weighted_median(water_burden, HHWT),
    mean_water_burden_capped = weighted.mean(
      pmin(water_burden, water_98),
      HHWT,
      na.rm = TRUE
    ),
    PUMA_components = str_c(sort(unique(PUMA_components)), collapse = " / "),
    .groups = "drop"
  ) %>%
  mutate(
    median_water_burden = if_else(n >= min_n, median_water_burden, NA_real_),
    mean_water_burden_capped = if_else(n >= min_n, mean_water_burden_capped, NA_real_),
    median_water_burden_pct = 100 * median_water_burden,
    mean_water_burden_capped_pct = 100 * mean_water_burden_capped,
    Area = factor(
      Area,
      levels = c(
        "Detroit city area",
        "Wayne County outside Detroit"
      )
    )
  )

write_csv(
  bar_values %>%
    select(
      YEAR, Area, PUMA_components, n, hhwt_sum,
      median_water_burden, median_water_burden_pct,
      mean_water_burden_capped, mean_water_burden_capped_pct
    ),
  "outputs/detroit_water_utility_burden_bars_2016_2021.csv"
)

write_lines(
  c(
    "Detroit water utility burden bar chart data availability",
    paste0("Requested years: ", paste(analysis_years, collapse = ", ")),
    paste0("Available years in cleaned RDS files: ", paste(available_years, collapse = ", ")),
    paste0("Available Detroit/Wayne CPUMA years: ", paste(available_geo_years, collapse = ", ")),
    paste0(
      "Missing requested years from cleaned RDS files: ",
      if_else(length(missing_data_years) == 0, "none", paste(missing_data_years, collapse = ", "))
    ),
    paste0(
      "Present but not safely identifiable as Detroit/Wayne CPUMAs: ",
      if_else(
        length(setdiff(missing_geo_years, missing_data_years)) == 0,
        "none",
        paste(setdiff(missing_geo_years, missing_data_years), collapse = ", ")
      )
    ),
    "",
    "Note: 2022-2023 have PUMA values, but CPUMA0010 is missing and STATEFIP was not retained in the cleaned RDS.",
    "Because PUMA codes repeat across states, this script does not infer Detroit-area rows from PUMA alone."
  ),
  "outputs/detroit_water_utility_burden_bars_data_note.txt"
)

p <- ggplot(
  bar_values,
  aes(
    x = factor(YEAR),
    y = median_water_burden_pct,
    fill = Area
  )
) +
  geom_col(
    position = position_dodge2(width = 0.78, preserve = "single"),
    width = 0.7,
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(
      label = if_else(
        is.na(median_water_burden_pct),
        "",
        paste0(percent(median_water_burden, accuracy = 0.1), "\nn=", comma(n))
      )
    ),
    position = position_dodge2(width = 0.78, preserve = "single"),
    vjust = -0.35,
    size = 2.6,
    color = "grey15"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_fill_manual(
    values = c(
      "Detroit city area" = "#0b5d7e",
      "Wayne County outside Detroit" = "#d95f02"
    ),
    name = NULL
  ) +
  labs(
    title = "Detroit, Michigan Water Utility Burden",
    subtitle = paste0(
      "Water bill / household income; weighted median by ConsPUMA, ",
      min(available_geo_years), "-", max(available_geo_years)
    ),
    x = NULL,
    y = "Median water burden",
    caption = str_wrap(paste(
      "Bars are weighted medians; labels show percentages and sampled-household counts.",
      "CPUMA refers to Consistent Public Use Microdata Area, a harmonized Census geography used to compare areas across years.",
      "Mappable Detroit/Wayne CPUMA geography is available for 2016-2021.",
      "Sources: IPUMS CPS microdata; official IPUMS ConsPUMA crosswalk."
    ), width = 120)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, hjust = 0, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "outputs/detroit_water_utility_burden_bars_2016_2021.png",
  p,
  width = 10,
  height = 6.5,
  dpi = 320
)

ggsave(
  "outputs/detroit_water_utility_burden_bars_2016_2021.pdf",
  p,
  width = 10,
  height = 6.5
)
