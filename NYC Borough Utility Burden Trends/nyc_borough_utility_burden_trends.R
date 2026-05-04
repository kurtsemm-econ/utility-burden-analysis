library(tidyverse)
library(readxl)
library(Hmisc)
library(scales)

# ---------------------------------------------------------
# NYC borough utility burden trends, 2017-2023
# ---------------------------------------------------------

utility_data <- readRDS("17_24_entropy_ready.rds")
cpuma_components <- read_excel("CPUMA0010_PUMA2010_components.xls")

nyc_lookup <- cpuma_components %>%
  filter(State_Name == "New York", str_detect(PUMA_Name, "^NYC-")) %>%
  transmute(
    PUMA = as.integer(PUMA),
    Borough = str_extract(
      PUMA_Name,
      "(?<=NYC-)[A-Za-z ]+(?= Community District)"
    ) %>% str_squish()
  ) %>%
  distinct()

borough_levels <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")

nyc_data <- utility_data %>%
  filter(YEAR >= 2017, YEAR <= 2023, PUMA %in% nyc_lookup$PUMA) %>%
  left_join(nyc_lookup, by = "PUMA") %>%
  filter(!is.na(Borough)) %>%
  mutate(Borough = factor(Borough, levels = borough_levels))

borough_summary <- nyc_data %>%
  group_by(YEAR, Borough) %>%
  summarise(
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_util_burden = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1],
    mean_util_burden = Hmisc::wtd.mean(
      util_burden,
      weights = HHWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    median_burden_pct = 100 * median_util_burden,
    mean_burden_pct = 100 * mean_util_burden
  )

citywide_summary <- nyc_data %>%
  group_by(YEAR) %>%
  summarise(
    Borough = "NYC overall",
    n = n(),
    hhwt_sum = sum(HHWT, na.rm = TRUE),
    median_util_burden = Hmisc::wtd.quantile(
      util_burden,
      weights = HHWT,
      probs = 0.5,
      na.rm = TRUE
    )[1],
    mean_util_burden = Hmisc::wtd.mean(
      util_burden,
      weights = HHWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    median_burden_pct = 100 * median_util_burden,
    mean_burden_pct = 100 * mean_util_burden
  )

trend_summary <- bind_rows(borough_summary, citywide_summary)

write_csv(
  trend_summary,
  "outputs/nyc_borough_utility_burden_trends_2017_2023.csv"
)

change_summary <- trend_summary %>%
  filter(YEAR %in% c(2017, 2023)) %>%
  select(YEAR, Borough, median_burden_pct, mean_burden_pct, n, hhwt_sum) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = c(median_burden_pct, mean_burden_pct, n, hhwt_sum),
    names_sep = "_"
  ) %>%
  mutate(
    median_change_pct_points = median_burden_pct_2023 - median_burden_pct_2017,
    mean_change_pct_points = mean_burden_pct_2023 - mean_burden_pct_2017
  )

write_csv(
  change_summary,
  "outputs/nyc_borough_utility_burden_change_2017_2023.csv"
)

plot_data <- trend_summary %>%
  mutate(
    Borough = factor(
      Borough,
      levels = c(borough_levels, "NYC overall")
    )
  )

label_data <- plot_data %>%
  filter(YEAR == 2023) %>%
  mutate(
    label = paste0(Borough, ": ", number(median_burden_pct, accuracy = 0.1), "%")
  )

borough_colors <- c(
  "Bronx" = "#8c2d04",
  "Brooklyn" = "#cc4c02",
  "Manhattan" = "#ec7014",
  "Queens" = "#fe9929",
  "Staten Island" = "#fec44f",
  "NYC overall" = "#2b2b2b"
)

p <- ggplot(plot_data, aes(x = YEAR, y = median_burden_pct, color = Borough)) +
  geom_line(aes(group = Borough, linewidth = Borough), lineend = "round") +
  geom_point(size = 2.2) +
  geom_text(
    data = label_data,
    aes(label = label),
    hjust = 0,
    nudge_x = 0.12,
    size = 3.3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = borough_colors) +
  scale_linewidth_manual(
    values = c(
      "Bronx" = 0.9,
      "Brooklyn" = 0.9,
      "Manhattan" = 0.9,
      "Queens" = 0.9,
      "Staten Island" = 0.9,
      "NYC overall" = 1.2
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 2017:2023,
    limits = c(2017, 2024.2)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  labs(
    title = "Median Utility Burden Across NYC Boroughs",
    subtitle = "Weighted median burden among household heads, 2017-2023",
    x = NULL,
    y = "Utility burden",
    caption = paste(
      "Utility burden = (gas + electric + water) / household income.",
      "Special IPUMS utility codes such as included-in-rent and no-charge values are recoded before burden construction.",
      "Source: IPUMS CPS cleaned file with NYC boroughs assigned from 2010 PUMA crosswalk."
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(15, 120, 15, 15)
  )

ggsave(
  "outputs/nyc_borough_utility_burden_trends_2017_2023.png",
  p,
  width = 10,
  height = 6.2,
  dpi = 320
)

ggsave(
  "outputs/nyc_borough_utility_burden_trends_2017_2023.pdf",
  p,
  width = 10,
  height = 6.2
)
