library(tidyverse)
library(sf)
library(readxl)
library(scales)

# Flint-area water utility burden heatmap, 2016-2024 where data are available.
# The current cleaned files contain 2016-2023; the script writes an availability
# note when requested years are missing from the RDS inputs.

analysis_years <- 2016:2024
flint_cpumas <- c(488L, 489L, 490L)
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

available_years <- sort(unique(utility_data$YEAR))
available_geo_years <- utility_data %>%
  filter(CPUMA0010 %in% flint_cpumas) %>%
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
    "Requested years present but not mappable to Flint CPUMAs: ",
    paste(setdiff(missing_geo_years, missing_data_years), collapse = ", ")
  )
}

cpuma_components <- read_excel("CPUMA0010_PUMA2010_components.xls")

flint_lookup <- cpuma_components %>%
  filter(State_Name == "Michigan", CPUMA0010 %in% flint_cpumas) %>%
  mutate(
    CPUMA0010 = as.integer(CPUMA0010),
    Area = case_when(
      CPUMA0010 == 490L ~ "Flint city area",
      CPUMA0010 == 489L ~ "Central Genesee outside Flint",
      CPUMA0010 == 488L ~ "Outer Genesee / nearby counties",
      TRUE ~ PUMA_Name
    )
  ) %>%
  group_by(CPUMA0010, Area) %>%
  summarise(
    PUMA_components = str_c(PUMA_Name, collapse = " / "),
    .groups = "drop"
  )

flint_data <- utility_data %>%
  filter(CPUMA0010 %in% flint_cpumas)

yearly_benchmark <- flint_data %>%
  group_by(YEAR) %>%
  summarise(
    flint_area_median_water_burden = weighted_median(water_burden, HHWT),
    .groups = "drop"
  )

flint_summary <- flint_data %>%
  group_by(YEAR, CPUMA0010) %>%
  summarise(
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_water_burden = weighted_median(water_burden, HHWT),
    mean_water_burden_capped = weighted.mean(
      pmin(water_burden, water_98),
      HHWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(yearly_benchmark, by = "YEAR") %>%
  mutate(
    median_water_burden = if_else(n >= min_n, median_water_burden, NA_real_),
    mean_water_burden_capped = if_else(n >= min_n, mean_water_burden_capped, NA_real_),
    water_burden_pct = 100 * median_water_burden,
    water_burden_index = 100 * median_water_burden / flint_area_median_water_burden
  ) %>%
  left_join(flint_lookup, by = "CPUMA0010")

cpuma_shapes <- st_read(
  "ipums_cpuma0010_shp/ipums_cpuma0010/ipums_cpuma0010.shp",
  quiet = TRUE
) %>%
  mutate(CPUMA0010 = as.integer(CPUMA0010)) %>%
  filter(CPUMA0010 %in% flint_cpumas)

map_data <- cpuma_shapes[rep(seq_len(nrow(cpuma_shapes)), each = length(available_geo_years)), ] %>%
  mutate(YEAR = rep(available_geo_years, times = nrow(cpuma_shapes))) %>%
  left_join(flint_summary, by = c("YEAR", "CPUMA0010")) %>%
  mutate(
    label = case_when(
      is.na(water_burden_pct) ~ paste0(Area, "\ninsufficient sample"),
      TRUE ~ paste0(Area, "\n", round(water_burden_pct, 2), "%")
    )
  )

map_labels <- st_point_on_surface(map_data)

write_csv(
  st_drop_geometry(map_data) %>%
    select(
      YEAR, CPUMA0010, Area, PUMA_components, n, hhwt_sum,
      median_water_burden, water_burden_pct,
      mean_water_burden_capped, water_burden_index
    ),
  "outputs/flint_water_utility_burden_heatmap_values_2016_2024.csv"
)

write_lines(
  c(
    "Flint water utility burden heatmap data availability",
    paste0("Requested years: ", paste(analysis_years, collapse = ", ")),
    paste0("Available years in cleaned RDS files: ", paste(available_years, collapse = ", ")),
    paste0("Available Flint CPUMA map years: ", paste(available_geo_years, collapse = ", ")),
    paste0(
      "Missing requested years from cleaned RDS files: ",
      if_else(length(missing_data_years) == 0, "none", paste(missing_data_years, collapse = ", "))
    ),
    paste0(
      "Present but not safely mappable to Flint CPUMAs: ",
      if_else(
        length(setdiff(missing_geo_years, missing_data_years)) == 0,
        "none",
        paste(setdiff(missing_geo_years, missing_data_years), collapse = ", ")
      )
    ),
    "",
    "Note: The file named 17_24_entropy_ready.rds currently contains 2017-2023 rows.",
    "The 2022-2023 rows have PUMA values, but CPUMA0010 is missing and STATEFIP was not retained in the cleaned RDS.",
    "Because PUMA codes repeat across states, this script does not infer Flint-area CPUMAs from PUMA alone."
  ),
  "outputs/flint_water_utility_burden_heatmap_data_note.txt"
)

p <- ggplot(map_data) +
  geom_sf(aes(fill = water_burden_index), color = "white", linewidth = 0.35) +
  geom_sf_text(
    data = map_labels,
    aes(label = label),
    size = 2.4,
    color = "grey10",
    fontface = "bold",
    lineheight = 0.9
  ) +
  facet_wrap(~ YEAR, ncol = 4) +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    values = rescale(c(50, 80, 100, 140, 220)),
    limits = c(50, 220),
    oob = squish,
    na.value = "grey82",
    labels = function(x) paste0(round(x), " = Flint-area median 100"),
    name = "Relative water\nburden index"
  ) +
  labs(
    title = "Flint, Michigan Water Utility Burden Heatmap",
    subtitle = paste0(
      "Weighted median water burden by ConsPUMA, ",
      min(available_geo_years), "-", max(available_geo_years),
      "; indexed within year to the Flint-area median"
    ),
    caption = paste(
      "Water utility burden = household water expenditure / household income.",
      "Values shown on the map are weighted medians.",
      "Gray areas indicate fewer than", min_n, "sampled households after filters.",
      "Mappable Flint CPUMA geography is available for 2016-2021 in the cleaned RDS files.",
      "Sources: IPUMS CPS microdata; official IPUMS ConsPUMA boundary file."
    )
  ) +
  coord_sf(datum = NA) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, hjust = 0, margin = margin(t = 10)),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "outputs/flint_water_utility_burden_heatmap_2016_2024.png",
  p,
  width = 12,
  height = 8.5,
  dpi = 320
)

ggsave(
  "outputs/flint_water_utility_burden_heatmap_2016_2024.pdf",
  p,
  width = 12,
  height = 8.5
)
