library(tidyverse)
library(tidycensus)
library(purrr)
library(sf)
library(readxl)
library(urbnthemes)
library(scales)
library(urbnmapr)
library(hexbin)
library(graphics)
library(ggplot2)
library(reshape2)

set_urbn_defaults(style = "map")
options(scipen=999)
urban_colors <- c("#cfe8f3","#73bfe2","#1696d2","#0a4c6a","#000000","#dfdfdf","#DB2B27",
                  "#ffffff","#0A4C6A")

my_colors <- c(palette_urbn_cyan[3], "#ffffff", palette_urbn_red[3])

my_counties <- get_urbn_map("counties", sf=T)
my_states <- get_urbn_map("states", sf=T)

## BRING OVER DATA FRAMES IN PREVIOUS SCRIPTS ##-----------

# 2018 index values 
ERAP_2018 <- read_csv("data/intermediate-data/housing_index_state_adj_2018.csv") 
ERAP_2018_main <- ERAP_2018 %>%
  select(GEOID, housing_index, covid_index, equity_index, total_index, housing_index_quantile, 
         covid_index_quantile, equity_index_quantile, total_index_quantile) %>%
  rename_all(paste0, "_2018")

# 2019 index values
ERAP_2019 <- read_csv("https://ui-covid-housing-risk-indicators.s3.amazonaws.com/housing_index_state_adj.csv")
ERAP_2019_main <- ERAP_2019 %>%
  select(GEOID, housing_index, covid_index, equity_index, total_index, housing_index_quantile, 
         covid_index_quantile, equity_index_quantile, total_index_quantile, perc_person_of_color,
         perc_low_income_jobs_lost) %>%
  rename_all(paste0, "_2019")


countydirection <- ERAP_2019_main %>%
  left_join(ERAP_2018_main, by = c("GEOID_2019" = "GEOID_2018")) %>%
  filter(!is.na(housing_index_2018) & !is.na(covid_index_2018) & !is.na(equity_index_2018) & !is.na(total_index_2018)) %>%
  mutate(county = substr(GEOID_2019, 1, 5)) %>%
  group_by(county) %>%
  summarize(housing_index_2019_avg = mean(housing_index_2019),
            housing_index_2018_avg = mean(housing_index_2018),
            covid_index_2019_avg = mean(covid_index_2019),
            covid_index_2018_avg = mean(covid_index_2018),
            equity_index_2019_avg = mean(equity_index_2019),
            equity_index_2018_avg = mean(equity_index_2018),
            total_index_2019_avg = mean(total_index_2019),
            total_index_2018_avg = mean(total_index_2018)) %>%
  ungroup() %>%
  mutate(housing_avg_diff = housing_index_2019_avg - housing_index_2018_avg,
         covid_avg_diff = covid_index_2019_avg - covid_index_2018_avg,
         equity_avg_diff = equity_index_2019_avg - equity_index_2018_avg,
         total_avg_diff = total_index_2019_avg - total_index_2018_avg) 

countymap_direction <- left_join(my_counties, countydirection, by = c("county_fips" = "county"))

## ALASKA ##------------

alaskamap <- countymap_direction %>%
  filter(state_abbv == "AK")

my_counties_AK <- my_counties %>%
  filter(state_abbv == "AK")

my_states_AK <- my_states %>%
  filter(state_abbv == "AK")

final_levels_map1_ak <- c("Improvement (0.16 to 0.33)","Improvement (0 to 0.16)","Decline (0 to 0.16)","Decline (0.16 to 0.33)")

# housing map
ggplot() + 
  geom_sf(my_counties_AK, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(alaskamap %>% filter(!is.na(housing_avg_diff)), mapping = aes(fill=housing_avg_diff %>% cut(breaks = c(-0.33,-0.16,0,0.16,0.33), include.lowest = T)), color = "#696969", size = 0.5) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[5],palette_urbn_cyan[3],palette_urbn_red[3],palette_urbn_red[5]), labels = final_levels_map1_ak) + 
  geom_sf(my_states_AK, mapping = aes(), fill = NA, size = 0.2, color = "#000000") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Changes in Housing Sub-Index by County")

ggsave("Alaska Maps/housing_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("Alaska Maps/housing_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)


final_levels_map2_ak <- c("Improvement (0 to 0.5)","Decline (0 to 0.5)","Decline (0.5 to 1)","Decline (1 to 1.5)")

# covid map
ggplot() + 
  geom_sf(my_counties_AK, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(alaskamap %>% filter(!is.na(covid_avg_diff)), mapping = aes(fill=covid_avg_diff %>% cut(breaks = c(-0.5,0,0.5,1,1.5), include.lowest = T)), color = "#696969", size = 0.5) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[3],palette_urbn_red[3],palette_urbn_red[5],palette_urbn_red[7]), labels = final_levels_map2_ak) + 
  geom_sf(my_states_AK, mapping = aes(), fill = NA, size = 0.2, color = "#000000") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Changes in COVID Sub-Index by County")

ggsave("Alaska Maps/covid_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("Alaska Maps/covid_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)



final_levels_map3_ak <- c("Improvement (0.33 to 0.5)","Improvement (0.16 to 0.33)","Improvement (0 to 0.16)","Decline (0 to 0.16)")

# equity map
ggplot() + 
  geom_sf(my_counties_AK, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(alaskamap %>% filter(!is.na(equity_avg_diff)), mapping = aes(fill=equity_avg_diff %>% cut(breaks = c(-0.5,-0.33,-0.16,0,0.16), include.lowest = T)), color = "#696969", size = .5) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[7],palette_urbn_cyan[5],palette_urbn_cyan[3],palette_urbn_red[3]), labels = final_levels_map3_ak) + 
  geom_sf(my_states_AK, mapping = aes(), fill = NA, size = 0.2, color = "#000000") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Changes in Equity Sub-Index by County") 

ggsave("Alaska Maps/equity_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("Alaska Maps/equity_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)



final_levels_map4_ak <- c("Improvement (0.16 to 0.33)","Improvement (0 to 0.16)","Decline (0 to 0.16)","Decline (0.16 to 0.33)")


#total map
ggplot() + 
  geom_sf(my_counties_AK, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(alaskamap %>% filter(!is.na(total_avg_diff)), mapping = aes(fill=total_avg_diff %>% cut(breaks = c(-0.33,-0.16,0,0.16,0.33), include.lowest = T)), color = "#696969", size = .5) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[5],palette_urbn_cyan[3],palette_urbn_red[3],palette_urbn_red[5]), labels = final_levels_map4_ak) + 
  geom_sf(my_states_AK, mapping = aes(), fill = NA, size = 0.22, color = "#000000") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Changes in Total Index by County")

ggsave("Alaska Maps/total_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("Alaska Maps/total_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)


##look at indicators

ERAP_2018_indicators_AK <- ERAP_2018 %>%
  filter(state_name == "Alaska") %>%
  select(GEOID, state_name, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
         perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
         perc_public_assistance, perc_foreign_born,perc_30hamfi,perc_low_income_jobs_lost) %>%
  rename_all(paste0, "_2018")

# 2019 indicator values
ERAP_2019_indicators_AK <- ERAP_2019 %>%
  filter(state_name == "Alaska") %>%
  select(GEOID, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
         perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
         perc_public_assistance, perc_foreign_born, perc_30hamfi, perc_low_income_jobs_lost) %>%
  rename_all(paste0, "_2019") 

indicators_combined_AK <- ERAP_2019_indicators_AK %>%
  left_join(ERAP_2018_indicators_AK, by = c("GEOID_2019" = "GEOID_2018")) %>%
  mutate(cost_burden_diff = perc_cost_burdened_under_35k_2019 - perc_cost_burdened_under_35k_2018,
         overcrowding_diff = perc_overcrowding_renter_1.50_or_more_2019 - perc_overcrowding_renter_1.50_or_more_2018,
         poverty_diff = perc_poverty_12mnth_2019 - perc_poverty_12mnth_2018,
         renters_diff = perc_renters_2019 - perc_renters_2018,
         unemployment_diff = perc_unemployed_laborforce_2019 - perc_unemployed_laborforce_2018,
         jobslost_diff = perc_low_income_jobs_lost_2019 - perc_low_income_jobs_lost_2018,
         nohinsure_diff = perc_no_hinsure_2019 - perc_no_hinsure_2018,
         perc_poc_diff = perc_person_of_color_2019 - perc_person_of_color_2018,
         perc_public_assist_diff = perc_public_assistance_2019 - perc_public_assistance_2018,
         perc_foreign_diff = perc_foreign_born_2019 - perc_foreign_born_2018,
         perc_30hamfi_diff = perc_30hamfi_2019 - perc_30hamfi_2018)

# Box Plots by Index 

set_urbn_defaults(style = "print")


# Housing

indicators_combined_AK %>% 
  select(GEOID_2019, poverty_diff, renters_diff, cost_burden_diff, overcrowding_diff, unemployment_diff) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot() +
  labs(title = "Change in Housing Sub-Index Indicators for Alaska Across Census Tracts",
       y = "Percentage Point Change from 2018 to 2019") + 
  scale_x_discrete(labels=c("poverty_diff" = "% Poverty",
                            "renters_diff" = "% Renters",
                            "cost_burden_diff" = "% Cost-Burdened",
                            "overcrowding_diff" = "% Overcrowded",
                            "unemployment_diff" = "% Unemployed")) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.title.x = element_blank())

ggsave("Alaska Maps/housing_indicator_change.png", width = 8, height = 6, dpi = 1000)

  
# COVID

indicators_combined_AK %>% 
  select(GEOID_2019, nohinsure_diff, jobslost_diff) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot() +
  labs(title = "Change in COVID Sub-Index Indicators for Alaska Across Census Tracts",
       y = "Percentage Point Change from 2018 to 2019") + 
  scale_x_discrete(labels=c("nohinsure_diff" = "% Without Health Insurance",
                            "jobslost_diff" = "% Low-Income Jobs Lost")) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.title.x = element_blank())

ggsave("Alaska Maps/covid_indicator_change.png", width = 8, height = 6, dpi = 1000)

# Equity

indicators_combined_AK %>% 
  select(GEOID_2019, perc_poc_diff, perc_30hamfi_diff, perc_public_assist_diff, perc_foreign_diff) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot() +
  labs(title = "Change in Equity Sub-Index Indicators for Alaska Across Census Tracts",
       y = "Percentage Point Change from 2018 to 2019") + 
  scale_x_discrete(labels=c("perc_poc_diff" = "% People of Color",
                            "perc_30hamfi_diff" = "% Low-Income Renters",
                            "perc_public_assist_diff" = "% Receiving Public Assistance",
                            "perc_foreign_diff" = "% Foreign Born")) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.title.x = element_blank())

ggsave("Alaska Maps/equity_indicator_change.png", width = 8, height = 6, dpi = 1000)















