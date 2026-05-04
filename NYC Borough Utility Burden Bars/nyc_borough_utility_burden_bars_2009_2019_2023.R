library(tidyverse)
library(scales)

# ---------------------------------------------------------
# NYC borough utility burden benchmark bar chart
# ---------------------------------------------------------

benchmark_data <- read_csv(
  "outputs/nyc_borough_utility_burden_benchmarks_2009_2019_2023.csv",
  show_col_types = FALSE
) %>%
  mutate(
    Borough = factor(
      Borough,
      levels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "NYC overall")
    ),
    YEAR = factor(YEAR, levels = c(2009, 2019, 2023))
  )

year_colors <- c(
  "2009" = "#c7d4e8",
  "2019" = "#6f8fb8",
  "2023" = "#163a63"
)

label_data <- benchmark_data %>%
  mutate(label = paste0(number(median_burden_pct, accuracy = 0.1), "%"))

p <- ggplot(
  benchmark_data,
  aes(x = Borough, y = median_burden_pct, fill = YEAR)
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.64,
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    data = label_data,
    aes(label = label, group = YEAR),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.0,
    color = "#1f1f1f",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = year_colors, name = NULL) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Median Utility Burden by NYC Borough",
    subtitle = "Benchmark comparison for 2009, 2019, and 2023",
    x = NULL,
    y = "Utility burden",
    caption = paste(
      "Utility burden = (gas + water + electricity bill) / household income.",
      "Bars show weighted median burden among household heads.",
      "Source: IPUMS CPS."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    axis.text.x = element_text(size = 11)
  )

ggsave(
  "outputs/nyc_borough_utility_burden_bars_2009_2019_2023.png",
  p,
  width = 10,
  height = 6.2,
  dpi = 320
)

ggsave(
  "outputs/nyc_borough_utility_burden_bars_2009_2019_2023.pdf",
  p,
  width = 10,
  height = 6.2
)
