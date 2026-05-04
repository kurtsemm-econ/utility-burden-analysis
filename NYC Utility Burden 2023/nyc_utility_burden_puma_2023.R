library(tidyverse)
library(sf)
library(readxl)
library(Hmisc)
library(scales)

# ---------------------------------------------------------
# NYC utility burden maps using the refreshed 2017-2023 file
# ---------------------------------------------------------

analysis_year <- 2023
min_group_n <- 30

utility_data <- readRDS("17_24_entropy_ready.rds")
puma_shapes <- st_read("ny_puma10_shp/tl_2010_36_puma10.shp", quiet = TRUE)
cpuma_components <- read_excel("CPUMA0010_PUMA2010_components.xls")

nyc_lookup <- cpuma_components %>%
  filter(State_Name == "New York", str_detect(PUMA_Name, "^NYC-")) %>%
  transmute(
    PUMA = as.integer(PUMA),
    Borough = str_extract(
      PUMA_Name,
      "(?<=NYC-)[A-Za-z ]+(?= Community District)"
    ) %>% str_squish(),
    Area = str_remove(PUMA_Name, "^NYC-")
  ) %>%
  distinct()

nyc_pumas <- sort(unique(nyc_lookup$PUMA))

nyc_data <- utility_data %>%
  filter(YEAR == analysis_year, PUMA %in% nyc_pumas) %>%
  left_join(nyc_lookup, by = "PUMA")

nyc_citywide_median <- Hmisc::wtd.quantile(
  nyc_data$util_burden,
  weights = nyc_data$HHWT,
  probs = 0.5,
  na.rm = TRUE
)[1]

overall_summary <- nyc_data %>%
  group_by(PUMA, Borough, Area) %>%
  summarise(
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_util_burden = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1],
    .groups = "drop"
  ) %>%
  mutate(
    median_burden_pct = 100 * median_util_burden,
    burden_index = 100 * median_util_burden / nyc_citywide_median
  )

map_shapes <- puma_shapes %>%
  mutate(PUMA = as.integer(PUMACE10)) %>%
  filter(PUMA %in% nyc_pumas) %>%
  left_join(overall_summary, by = "PUMA")

borough_outline <- map_shapes %>%
  group_by(Borough) %>%
  summarise(.groups = "drop")

borough_labels <- st_point_on_surface(borough_outline)

write_csv(
  st_drop_geometry(map_shapes) %>%
    select(PUMA, Borough, Area, n, hhwt_sum, median_util_burden, median_burden_pct, burden_index),
  "outputs/nyc_puma_utility_burden_2023.csv"
)

p_overall <- ggplot(map_shapes) +
  geom_sf(aes(fill = burden_index), color = "white", linewidth = 0.22) +
  geom_sf(data = borough_outline, fill = NA, color = "grey20", linewidth = 0.7) +
  geom_sf_text(
    data = borough_labels,
    aes(label = Borough),
    size = 3.2,
    color = "grey10",
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours = c("#fff7ec", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"),
    values = rescale(c(
      min(map_shapes$burden_index, na.rm = TRUE),
      80, 100, 140,
      max(map_shapes$burden_index, na.rm = TRUE)
    )),
    labels = function(x) paste0(round(x), " = NYC median 100"),
    name = "Relative burden\nindex"
  ) +
  labs(
    title = "NYC Utility Burden by PUMA",
    subtitle = paste0(
      "Weighted median burden in ", analysis_year,
      ", indexed to the citywide median"
    ),
    caption = paste(
      "Utility burden = (gas + electric + water) / household income.",
      "Gray PUMAs indicate no sampled households in the filtered analytic file.",
      "Map uses 2010 PUMA boundaries and the refreshed 2017-2023 cleaned file.",
      "Sources: IPUMS CPS; U.S. Census TIGER/Line PUMA boundaries."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "inside",
    legend.position.inside = c(0.83, 0.23),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "outputs/nyc_utility_burden_puma_heatmap_2023.png",
  p_overall,
  width = 8.5,
  height = 9,
  dpi = 320
)

ggsave(
  "outputs/nyc_utility_burden_puma_heatmap_2023.pdf",
  p_overall,
  width = 8.5,
  height = 9
)

# --------------------------------------------
# Black vs White comparison at borough level
# --------------------------------------------

grouped_data <- nyc_data %>%
  mutate(
    group = case_when(
      IDENTITY %in% c("Black Men", "Black Women") ~ "Black households",
      IDENTITY %in% c("White Men", "White Women") ~ "White households",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group))

borough_group_summary <- grouped_data %>%
  group_by(Borough, group) %>%
  summarise(
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_util_burden = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1],
    .groups = "drop"
  ) %>%
  mutate(
    median_burden_pct = 100 * median_util_burden,
    burden_index = 100 * median_util_burden / nyc_citywide_median
  ) %>%
  mutate(
    median_util_burden = if_else(n >= min_group_n, median_util_burden, NA_real_),
    median_burden_pct = if_else(n >= min_group_n, median_burden_pct, NA_real_),
    burden_index = if_else(n >= min_group_n, burden_index, NA_real_)
  )

write_csv(
  borough_group_summary,
  "outputs/nyc_black_white_borough_2023.csv"
)

borough_group_map <- borough_outline %>%
  left_join(borough_group_summary, by = "Borough") %>%
  filter(!is.na(group))

make_group_map <- function(group_name, output_stub) {
  group_map <- borough_group_map %>%
    filter(group == group_name) %>%
    mutate(
      combined_label = if_else(
        is.na(median_burden_pct),
        paste0(Borough, "\nInsufficient\nsample"),
        paste0(Borough, "\n", round(median_burden_pct, 1), "%")
      )
    )

  group_labels <- st_point_on_surface(group_map)

  p_group <- ggplot(group_map) +
    geom_sf(aes(fill = burden_index), color = "white", linewidth = 0.7) +
    geom_sf_text(
      data = group_labels,
      aes(label = combined_label),
      size = 3.3,
      color = "grey10",
      fontface = "bold",
      lineheight = 0.95
    ) +
    scale_fill_gradientn(
      colours = c("#fff7ec", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"),
      values = rescale(c(60, 80, 100, 140, 220)),
      limits = c(60, 220),
      oob = squish,
      na.value = "grey78",
      labels = function(x) paste0(round(x), " = NYC median 100"),
      name = "Relative burden\nindex"
    ) +
    labs(
      title = paste0("NYC Utility Burden: ", group_name),
      subtitle = paste0(
        "Weighted median burden by borough in ", analysis_year,
        "; values shown directly on the map"
      ),
      caption = paste(
        "Household groups are defined by the race and sex of the household head in the cleaned IPUMS CPS file.",
        "Gray boroughs indicate fewer than", min_group_n, "sampled households.",
        "Utility burden = (gas + electric + water) / household income.",
        "Sources: IPUMS CPS; U.S. Census TIGER/Line PUMA boundaries."
      )
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
      plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 10)),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(
    paste0("outputs/", output_stub, ".png"),
    p_group,
    width = 7.5,
    height = 7.5,
    dpi = 320
  )

  ggsave(
    paste0("outputs/", output_stub, ".pdf"),
    p_group,
    width = 7.5,
    height = 7.5
  )
}

make_group_map("Black households", "nyc_black_households_borough_map_2023")
make_group_map("White households", "nyc_white_households_borough_map_2023")
