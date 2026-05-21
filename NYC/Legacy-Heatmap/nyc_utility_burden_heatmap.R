library(tidyverse)
library(sf)
library(readxl)
library(Hmisc)
library(scales)

# Build an NYC utility-burden heat map from the latest year in the
# entropy-ready file that still carries ConsPUMA geography.

utility_data <- readRDS("17_23_entropy_ready.rds")
cpuma_components <- read_excel("CPUMA0010_PUMA2010_components.xls")
cpuma_shapes <- st_read(
  "ipums_cpuma0010_shp/ipums_cpuma0010/ipums_cpuma0010.shp",
  quiet = TRUE
)

nyc_lookup <- cpuma_components %>%
  filter(State_Name == "New York", str_detect(PUMA_Name, "^NYC-")) %>%
  mutate(
    Borough = str_extract(
      PUMA_Name,
      "(?<=NYC-)[A-Za-z ]+(?= Community District)"
    ) %>% str_squish(),
    Label = str_remove(PUMA_Name, "^NYC-")
  ) %>%
  group_by(CPUMA0010, Borough) %>%
  summarise(
    Label = str_c(unique(Label), collapse = " / "),
    .groups = "drop"
  )

latest_geo_year <- utility_data %>%
  filter(!is.na(CPUMA0010)) %>%
  summarise(latest_year = max(YEAR, na.rm = TRUE)) %>%
  pull(latest_year)

nyc_summary <- utility_data %>%
  filter(YEAR == latest_geo_year, CPUMA0010 %in% nyc_lookup$CPUMA0010) %>%
  group_by(CPUMA0010) %>%
  summarise(
    households = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_util_burden = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1],
    mean_util_burden_capped = weighted.mean(
      pmin(util_burden, util_98),
      HHWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

nyc_citywide_median <- utility_data %>%
  filter(YEAR == latest_geo_year, CPUMA0010 %in% nyc_lookup$CPUMA0010) %>%
  summarise(
    citywide_median = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1]
  ) %>%
  pull(citywide_median)

map_data <- cpuma_shapes %>%
  mutate(CPUMA0010 = as.integer(CPUMA0010)) %>%
  inner_join(nyc_lookup, by = "CPUMA0010") %>%
  left_join(nyc_summary, by = "CPUMA0010") %>%
  mutate(
    median_burden_index = 100 * median_util_burden / nyc_citywide_median,
    median_burden_pct = 100 * median_util_burden
  )

borough_outline <- map_data %>%
  group_by(Borough) %>%
  summarise(.groups = "drop")

borough_labels <- st_point_on_surface(borough_outline)

write_csv(
  st_drop_geometry(map_data) %>%
    select(
      CPUMA0010, Borough, Label, households, hhwt_sum,
      median_util_burden, median_burden_pct,
      mean_util_burden_capped, median_burden_index
    ),
  "outputs/nyc_utility_burden_map_values.csv"
)

p <- ggplot(map_data) +
  geom_sf(aes(fill = median_burden_index), color = "white", linewidth = 0.25) +
  geom_sf(
    data = borough_outline,
    fill = NA,
    color = "grey15",
    linewidth = 0.7
  ) +
  geom_sf_text(
    data = borough_labels,
    aes(label = Borough),
    size = 3.2,
    color = "grey10",
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours = c("#fff7ec", "#fdd49e", "#f67e4b", "#d7301f", "#7f0000"),
    values = rescale(c(
      min(map_data$median_burden_index, na.rm = TRUE),
      90, 100, 115,
      max(map_data$median_burden_index, na.rm = TRUE)
    )),
    labels = function(x) paste0(round(x), " = NYC median 100"),
    name = "Relative utility\nburden index"
  ) +
  labs(
    title = "NYC Utility Burden Hotspots",
    subtitle = paste0(
      "Weighted median utility burden by ConsPUMA, indexed to the NYC median in ",
      latest_geo_year
    ),
    caption = paste(
      "Utility burden is defined here as household utility expenditures",
      "(gas + electric + water) divided by household income.",
      "Map uses the latest year with non-missing ConsPUMA geography in the current entropy-ready files.",
      "Sources: IPUMS CPS microdata; official IPUMS ConsPUMA boundary file."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "inside",
    legend.position.inside = c(0.82, 0.24),
    legend.direction = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "outputs/nyc_utility_burden_heatmap_2021.png",
  p,
  width = 8.5,
  height = 9,
  dpi = 320
)

ggsave(
  "outputs/nyc_utility_burden_heatmap_2021.pdf",
  p,
  width = 8.5,
  height = 9
)
