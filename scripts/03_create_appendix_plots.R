# Write out some correlation matrices and distributions of indicators
library(tidyverse)
library(ggcorrplot)
library(ggridges)
library(urbnthemes)
library(grid)
library(gridExtra)

set_urbn_defaults(style = "print")
final_data <- read_csv("https://ui-covid-housing-risk-indicators.s3.amazonaws.com/housing_index_state_adj.csv")

# Create correlation matrix of indicators for appendix

indicator_corr_matrix <- final_data %>%
  select(starts_with("perc")) %>%
  rename(
    `Cost-burdened households` = perc_cost_burdened_under_35k,
    `Unemployed people` = perc_unemployed_laborforce,
    `Households receiving public assistance` = perc_public_assistance,
    `People living in poverty` = perc_poverty_12mnth,
    `People of color` = perc_person_of_color,
    `Overcrowded households` = perc_overcrowding_renter_1.50_or_more,
    `Adults without health insurance` = perc_no_hinsure,
    `Low-income jobs lost` = perc_low_income_jobs_lost,
    `Foreign-born` = perc_foreign_born,
    `Extremely low–income renters` = perc_30hamfi,
    `Renters` = perc_renters
  ) %>%
  # Need to look into missing data problems
  cor(use = "complete.obs")

#print as pdf


indicator_corr <- ggcorrplot(indicator_corr_matrix,
  type = "lower",
  lab = TRUE,
  colors = c(palette_urbn_cyan[5], "white", palette_urbn_red[5]),
  legend.title = "",
  digits = 2
) +
  theme_urbn_print(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.95),
    legend.key.width = unit(1, "cm"),

  ) +
  labs(
    y = "", x = "",
    fill = "Correlation"
  )


# Paste together legend and plot so legend can be left aligned

x <- grid.arrange(remove_legend(indicator_corr))
legend <- grid.arrange(get_legend(indicator_corr))

dir.create("output/appendix/", showWarnings = FALSE)
ggsave("output/appendix/indicator_correlation.png", x, width = 8, height = 8, units = c("in"))
ggsave("output/appendix/indicator_correlation_legend.png", legend, width = 6, height = 1, units = c("in"))

ggsave(filename = "indicator_correlation.pdf", plot = x, width = 8, height = 8, device = cairo_pdf)
ggsave(filename = "indicator_correlation_legend.pdf", plot = legend, width = 6, height = 1, device = cairo_pdf)


# Create index distribution histograms for appendix

# Get min/max values of each subindex/index for plotting with geom_text


max_value_of_index <- final_data %>%
  pivot_longer(housing_index:total_index, names_to = "index") %>%
  group_by(index) %>%
  summarize(
    max_val = max(value, na.rm = TRUE) %>% round(digits = 3),
    min_val = min(value, na.rm = TRUE) %>% round(digits = 3)
  ) %>%
  mutate(
    index = str_replace_all(index, "_", " "),
    index = str_to_title(index)
  ) %>%
  mutate(index = case_when(
    index == "Covid Index" ~ "COVID-19 Impact subindex",
    index == "Equity Index" ~ "Equity subindex",
    index == "Housing Index" ~ "Housing Instability Risk subindex",
    index == "Total Index" ~ "Total index"
  )) %>%
  mutate(index = factor(index, levels = c(
    "Housing Instability Risk subindex",
    "COVID-19 Impact subindex",
    "Equity subindex",
    "Total index"
  )))

index_dists <- final_data %>%
  pivot_longer(housing_index:total_index, names_to = "index") %>%
  mutate(
    index = str_replace_all(index, "_", " "),
    index = str_to_title(index)
  ) %>%
  mutate(index = case_when(
    index == "Covid Index" ~ "COVID-19 Impact subindex",
    index == "Equity Index" ~ "Equity subindex",
    index == "Housing Index" ~ "Housing Instability Risk subindex",
    index == "Total Index" ~ "Total index"
  )) %>%
  mutate(index = factor(index, levels = c(
    "Housing Instability Risk subindex",
    "COVID-19 Impact subindex",
    "Equity subindex",
    "Total index"
  ))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "white", bins = 100) +
  # Add min and max numbers as text
  geom_text(
    data = max_value_of_index,
    aes(x = max_val, y = 200, label = max_val %>% scales::number(accuracy = 0.1)),
    colour = palette_urbn_main["black"] %>% unname(), 
    fontface = "bold", size = 3
  ) +
  geom_text(
    data = max_value_of_index,
    aes(x = min_val, y = 200, label = min_val %>% scales::number(accuracy = 0.1)),
    colour = palette_urbn_main["black"] %>% unname(), 
    fontface = "bold", size = 3
  ) +
  facet_wrap(~index, ncol = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(strip.text = element_text(face = "plain"),
        plot.title = element_text(face = "plain")) +
  labs(title = "Index Distributions among all US Census Tracts", x = "Index value", y = "") 


ggsave(paste0("output/appendix/index_histograms_plain.png"), dpi = 1000, height = 8, width = 6, units = c("in"))
ggsave(filename = "index_histograms_plain.pdf", plot = index_dists, width = 8, height = 6, device = cairo_pdf)
urbnthemes