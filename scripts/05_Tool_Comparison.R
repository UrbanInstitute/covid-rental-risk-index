# Reads in new tool and old tool and compares values

library(tidyverse)
library(tidycensus)
library(purrr)
library(sf)
library(readxl)
library(urbnthemes)
library(scales)
library(urbnmapr)
library(hexbin)

set_urbn_defaults(style = "map")
options(scipen=999)
urban_colors <- c("#cfe8f3","#73bfe2","#1696d2","#0a4c6a","#000000","#dfdfdf","#DB2B27",
                  "#ffffff","#0A4C6A")

my_counties <- get_urbn_map("counties", sf=T)
my_states <- get_urbn_map("states", sf=T)



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

### Summarizing changes in percentiles for census tracts -------

combined <- ERAP_2019_main %>%
  left_join(ERAP_2018_main, by = c("GEOID_2019" = "GEOID_2018")) %>%
  filter(!is.na(housing_index_2018) & !is.na(covid_index_2018) & !is.na(equity_index_2018) & !is.na(total_index_2018)) %>%
  mutate(housing_diff = housing_index_2019 - housing_index_2018,
         covid_diff = covid_index_2019 - covid_index_2018,
         equity_diff = equity_index_2019 - equity_index_2018,
         total_diff = total_index_2019 - total_index_2018,
         housing_diff_quantile = housing_index_quantile_2019 - housing_index_quantile_2018,
         covid_diff_quantile = covid_index_quantile_2019 - covid_index_quantile_2018,
         equity_diff_quantile = equity_index_quantile_2019 - equity_index_quantile_2018,
         total_diff_quantile = total_index_quantile_2019 - total_index_quantile_2018) %>%
  mutate(housing_quantile_change = case_when(housing_diff_quantile == 0 ~"No Change",
                                             housing_diff_quantile > 0 & housing_diff_quantile <=0.25 ~ "0-1 Quartile Increase",
                                             housing_diff_quantile > 0.25 & housing_diff_quantile <= 0.50 ~ "1-2 Quartile Increase",
                                             housing_diff_quantile > 0.50 & housing_diff_quantile <= 0.75 ~ "2-3 Quartile Increase",
                                             housing_diff_quantile > 0.75 & housing_diff_quantile <= 1.00 ~ "3-4 Quartile Increase",
                                             housing_diff_quantile < 0 & housing_diff_quantile >= -0.25 ~ "0-1 Quartile Decrease",
                                             housing_diff_quantile < -0.25 & housing_diff_quantile >= -0.50 ~ "1-2 Quartile Decrease",
                                             housing_diff_quantile < -0.50 & housing_diff_quantile >= -0.75 ~ "2-3 Quartile Decrease",
                                             housing_diff_quantile < -0.75 & housing_diff_quantile >= -1 ~ "3-4 Quartile Decrease"),
         covid_quantile_change = case_when(covid_diff_quantile == 0 ~"No Change",
                                           covid_diff_quantile > 0 & covid_diff_quantile <=0.25 ~ "0-1 Quartile Increase",
                                           covid_diff_quantile > 0.25 & covid_diff_quantile <= 0.50 ~ "1-2 Quartile Increase",
                                           covid_diff_quantile > 0.50 & covid_diff_quantile <= 0.75 ~ "2-3 Quartile Increase",
                                           covid_diff_quantile > 0.75 & covid_diff_quantile <= 1.00 ~ "3-4 Quartile Increase",
                                           covid_diff_quantile < 0 & covid_diff_quantile >= -0.25 ~ "0-1 Quartile Decrease",
                                           covid_diff_quantile < -0.25 & covid_diff_quantile >= -0.50 ~ "1-2 Quartile Decrease",
                                           covid_diff_quantile < -0.50 & covid_diff_quantile >= -0.75 ~ "2-3 Quartile Decrease",
                                           covid_diff_quantile < -0.75 & covid_diff_quantile >= -1 ~ "3-4 Quartile Decrease"),
         equity_quantile_change = case_when(equity_diff_quantile == 0 ~"No Change",
                                            equity_diff_quantile > 0 & equity_diff_quantile <=0.25 ~ "0-1 Quartile Increase",
                                            equity_diff_quantile > 0.25 & equity_diff_quantile <= 0.50 ~ "1-2 Quartile Increase",
                                            equity_diff_quantile > 0.50 & equity_diff_quantile <= 0.75 ~ "2-3 Quartile Increase",
                                            equity_diff_quantile > 0.75 & equity_diff_quantile <= 1.00 ~ "3-4 Quartile Increase",
                                            equity_diff_quantile < 0 & equity_diff_quantile >= -0.25 ~ "0-1 Quartile Decrease",
                                            equity_diff_quantile < -0.25 & equity_diff_quantile >= -0.50 ~ "1-2 Quartile Decrease",
                                            equity_diff_quantile < -0.50 & equity_diff_quantile >= -0.75 ~ "2-3 Quartile Decrease",
                                            equity_diff_quantile < -0.75 & equity_diff_quantile >= -1 ~ "3-4 Quartile Decrease"),
         total_quantile_change = case_when(total_diff_quantile == 0 ~"No Change",
                                           total_diff_quantile > 0 & total_diff_quantile <=0.25 ~ "0-1 Quartile Increase",
                                           total_diff_quantile > 0.25 & total_diff_quantile <= 0.50 ~ "1-2 Quartile Increase",
                                           total_diff_quantile > 0.50 & total_diff_quantile <= 0.75 ~ "2-3 Quartile Increase",
                                           total_diff_quantile > 0.75 & total_diff_quantile <= 1.00 ~ "3-4 Quartile Increase",
                                           total_diff_quantile < 0 & total_diff_quantile >= -0.25 ~ "0-1 Quartile Decrease",
                                           total_diff_quantile < -0.25 & total_diff_quantile >= -0.50 ~ "1-2 Quartile Decrease",
                                           total_diff_quantile < -0.50 & total_diff_quantile >= -0.75 ~ "2-3 Quartile Decrease",
                                           total_diff_quantile < -0.75 & total_diff_quantile >= -1 ~ "3-4 Quartile Decrease")) %>%
  mutate(housing_decile_change = case_when(housing_diff_quantile == 0 ~"No Change",
                                             housing_diff_quantile > 0 & housing_diff_quantile <=0.10 ~ "0-1 Decile Increase",
                                             housing_diff_quantile > 0.10 & housing_diff_quantile <= 0.20 ~ "1-2 Decile Increase",
                                             housing_diff_quantile > 0.20 & housing_diff_quantile <= 0.30 ~ "2-3 Decile Increase",
                                             housing_diff_quantile > 0.30 & housing_diff_quantile <= 0.40 ~ "3-4 Decile Increase",
                                             housing_diff_quantile > 0.40 & housing_diff_quantile <= 0.50 ~ "4-5 Decile Increase",
                                             housing_diff_quantile > 0.50 & housing_diff_quantile <= 0.60 ~ "5-6 Decile Increase",                                           
                                             housing_diff_quantile > 0.60 & housing_diff_quantile <= 0.70 ~ "6-7 Decile Increase",
                                             housing_diff_quantile > 0.70 & housing_diff_quantile <= 0.80 ~ "7-8 Decile Increase",
                                             housing_diff_quantile > 0.80 & housing_diff_quantile <= 0.90 ~ "8-9 Decile Increase",
                                             housing_diff_quantile > 0.90 & housing_diff_quantile <= 1.00 ~ "9-10 Decile Increase",
                                             housing_diff_quantile < 0 & housing_diff_quantile >=-0.10 ~ "0-1 Decile Decrease",
                                             housing_diff_quantile < -0.10 & housing_diff_quantile >= -0.20 ~ "1-2 Decile Decrease",
                                             housing_diff_quantile < -0.20 & housing_diff_quantile >= -0.30 ~ "2-3 Decile Decrease",
                                             housing_diff_quantile < -0.30 & housing_diff_quantile >= -0.40 ~ "3-4 Decile Decrease",
                                             housing_diff_quantile < -0.40 & housing_diff_quantile >= -0.50 ~ "4-5 Decile Decrease",
                                             housing_diff_quantile < -0.50 & housing_diff_quantile >= -0.60 ~ "5-6 Decile Decrease",                                           
                                             housing_diff_quantile < -0.60 & housing_diff_quantile >= -0.70 ~ "6-7 Decile Decrease",
                                             housing_diff_quantile < -0.70 & housing_diff_quantile >= -0.80 ~ "7-8 Decile Decrease",
                                             housing_diff_quantile < -0.80 & housing_diff_quantile >= -0.90 ~ "8-9 Decile Decrease",
                                             housing_diff_quantile < -0.90 & housing_diff_quantile >= -1.00 ~ "9-10 Decile Decrease"),
         covid_decile_change = case_when(covid_diff_quantile == 0 ~"No Change",
                                         covid_diff_quantile > 0 & covid_diff_quantile <=0.10 ~ "0-1 Decile Increase",
                                         covid_diff_quantile > 0.10 & covid_diff_quantile <= 0.20 ~ "1-2 Decile Increase",
                                         covid_diff_quantile > 0.20 & covid_diff_quantile <= 0.30 ~ "2-3 Decile Increase",
                                         covid_diff_quantile > 0.30 & covid_diff_quantile <= 0.40 ~ "3-4 Decile Increase",
                                         covid_diff_quantile > 0.40 & covid_diff_quantile <= 0.50 ~ "4-5 Decile Increase",
                                         covid_diff_quantile > 0.50 & covid_diff_quantile <= 0.60 ~ "5-6 Decile Increase",                                           
                                         covid_diff_quantile > 0.60 & covid_diff_quantile <= 0.70 ~ "6-7 Decile Increase",
                                         covid_diff_quantile > 0.70 & covid_diff_quantile <= 0.80 ~ "7-8 Decile Increase",
                                         covid_diff_quantile > 0.80 & covid_diff_quantile <= 0.90 ~ "8-9 Decile Increase",
                                         covid_diff_quantile > 0.90 & covid_diff_quantile <= 1.00 ~ "9-10 Decile Increase",
                                         covid_diff_quantile < 0 & covid_diff_quantile >=-0.10 ~ "0-1 Decile Decrease",
                                         covid_diff_quantile < -0.10 & covid_diff_quantile >= -0.20 ~ "1-2 Decile Decrease",
                                         covid_diff_quantile < -0.20 & covid_diff_quantile >= -0.30 ~ "2-3 Decile Decrease",
                                         covid_diff_quantile < -0.30 & covid_diff_quantile >= -0.40 ~ "3-4 Decile Decrease",
                                         covid_diff_quantile < -0.40 & covid_diff_quantile >= -0.50 ~ "4-5 Decile Decrease",
                                         covid_diff_quantile < -0.50 & covid_diff_quantile >= -0.60 ~ "5-6 Decile Decrease",                                           
                                         covid_diff_quantile < -0.60 & covid_diff_quantile >= -0.70 ~ "6-7 Decile Decrease",
                                         covid_diff_quantile < -0.70 & covid_diff_quantile >= -0.80 ~ "7-8 Decile Decrease",
                                         covid_diff_quantile < -0.80 & covid_diff_quantile >= -0.90 ~ "8-9 Decile Decrease",
                                         covid_diff_quantile < -0.90 & covid_diff_quantile >= -1.00 ~ "9-10 Decile Decrease"),
         equity_decile_change = case_when(equity_diff_quantile == 0 ~"No Change",
                                          equity_diff_quantile > 0 & equity_diff_quantile <=0.10 ~ "0-1 Decile Increase",
                                          equity_diff_quantile > 0.10 & equity_diff_quantile <= 0.20 ~ "1-2 Decile Increase",
                                          equity_diff_quantile > 0.20 & equity_diff_quantile <= 0.30 ~ "2-3 Decile Increase",
                                          equity_diff_quantile > 0.30 & equity_diff_quantile <= 0.40 ~ "3-4 Decile Increase",
                                          equity_diff_quantile > 0.40 & equity_diff_quantile <= 0.50 ~ "4-5 Decile Increase",
                                          equity_diff_quantile > 0.50 & equity_diff_quantile <= 0.60 ~ "5-6 Decile Increase",                                           
                                          equity_diff_quantile > 0.60 & equity_diff_quantile <= 0.70 ~ "6-7 Decile Increase",
                                          equity_diff_quantile > 0.70 & equity_diff_quantile <= 0.80 ~ "7-8 Decile Increase",
                                          equity_diff_quantile > 0.80 & equity_diff_quantile <= 0.90 ~ "8-9 Decile Increase",
                                          equity_diff_quantile > 0.90 & equity_diff_quantile <= 1.00 ~ "9-10 Decile Increase",
                                          equity_diff_quantile < 0 & equity_diff_quantile >=-0.10 ~ "0-1 Decile Decrease",
                                          equity_diff_quantile < -0.10 & equity_diff_quantile >= -0.20 ~ "1-2 Decile Decrease",
                                          equity_diff_quantile < -0.20 & equity_diff_quantile >= -0.30 ~ "2-3 Decile Decrease",
                                          equity_diff_quantile < -0.30 & equity_diff_quantile >= -0.40 ~ "3-4 Decile Decrease",
                                          equity_diff_quantile < -0.40 & equity_diff_quantile >= -0.50 ~ "4-5 Decile Decrease",
                                          equity_diff_quantile < -0.50 & equity_diff_quantile >= -0.60 ~ "5-6 Decile Decrease",                                           
                                          equity_diff_quantile < -0.60 & equity_diff_quantile >= -0.70 ~ "6-7 Decile Decrease",
                                          equity_diff_quantile < -0.70 & equity_diff_quantile >= -0.80 ~ "7-8 Decile Decrease",
                                          equity_diff_quantile < -0.80 & equity_diff_quantile >= -0.90 ~ "8-9 Decile Decrease",
                                          equity_diff_quantile < -0.90 & equity_diff_quantile >= -1.00 ~ "9-10 Decile Decrease"),
         total_decile_change = case_when(total_diff_quantile == 0 ~"No Change",
                                         total_diff_quantile > 0 & total_diff_quantile <=0.10 ~ "0-1 Decile Increase",
                                         total_diff_quantile > 0.10 & total_diff_quantile <= 0.20 ~ "1-2 Decile Increase",
                                         total_diff_quantile > 0.20 & total_diff_quantile <= 0.30 ~ "2-3 Decile Increase",
                                         total_diff_quantile > 0.30 & total_diff_quantile <= 0.40 ~ "3-4 Decile Increase",
                                         total_diff_quantile > 0.40 & total_diff_quantile <= 0.50 ~ "4-5 Decile Increase",
                                         total_diff_quantile > 0.50 & total_diff_quantile <= 0.60 ~ "5-6 Decile Increase",                                           
                                         total_diff_quantile > 0.60 & total_diff_quantile <= 0.70 ~ "6-7 Decile Increase",
                                         total_diff_quantile > 0.70 & total_diff_quantile <= 0.80 ~ "7-8 Decile Increase",
                                         total_diff_quantile > 0.80 & total_diff_quantile <= 0.90 ~ "8-9 Decile Increase",
                                         total_diff_quantile > 0.90 & total_diff_quantile <= 1.00 ~ "9-10 Decile Increase",
                                         total_diff_quantile < 0 & total_diff_quantile >=-0.10 ~ "0-1 Decile Decrease",
                                         total_diff_quantile < -0.10 & total_diff_quantile >= -0.20 ~ "1-2 Decile Decrease",
                                         total_diff_quantile < -0.20 & total_diff_quantile >= -0.30 ~ "2-3 Decile Decrease",
                                         total_diff_quantile < -0.30 & total_diff_quantile >= -0.40 ~ "3-4 Decile Decrease",
                                         total_diff_quantile < -0.40 & total_diff_quantile >= -0.50 ~ "4-5 Decile Decrease",
                                         total_diff_quantile < -0.50 & total_diff_quantile >= -0.60 ~ "5-6 Decile Decrease",                                           
                                         total_diff_quantile < -0.60 & total_diff_quantile >= -0.70 ~ "6-7 Decile Decrease",
                                         total_diff_quantile < -0.70 & total_diff_quantile >= -0.80 ~ "7-8 Decile Decrease",
                                         total_diff_quantile < -0.80 & total_diff_quantile >= -0.90 ~ "8-9 Decile Decrease",
                                         total_diff_quantile < -0.90 & total_diff_quantile >= -1.00 ~ "9-10 Decile Decrease"))

# write this out

combined %>%
  write_csv("data/intermediate-data/tool comparison.csv")

# using quantiles
table_housing <- combined %>%
  select(housing_quantile_change, covid_quantile_change, equity_quantile_change, total_quantile_change) %>%
  count(housing_quantile_change) %>%
  mutate(percent_housing = n / sum(n)) %>%
  select(housing_quantile_change, percent_housing) %>%
  rename(quantile_change = housing_quantile_change)

table_covid <- combined %>%
  select(housing_quantile_change, covid_quantile_change, equity_quantile_change, total_quantile_change) %>%
  count(covid_quantile_change) %>%
  mutate(percent_covid = n / sum(n)) %>%
  select(covid_quantile_change, percent_covid) %>%
  rename(quantile_change = covid_quantile_change)

table_equity <- combined %>%
  select(housing_quantile_change, covid_quantile_change, equity_quantile_change, total_quantile_change) %>%
  count(equity_quantile_change) %>%
  mutate(percent_equity = n / sum(n)) %>%
  select(equity_quantile_change, percent_equity) %>%
  rename(quantile_change = equity_quantile_change)

table_total <- combined %>%
  select(housing_quantile_change, covid_quantile_change, equity_quantile_change, total_quantile_change) %>%
  count(total_quantile_change) %>%
  mutate(percent_total = n / sum(n)) %>%
  select(total_quantile_change, percent_total) %>%
  rename(quantile_change = total_quantile_change)

summary_table <- table_housing %>%
  left_join(table_covid, by = "quantile_change") %>%
  left_join(table_equity, by = "quantile_change") %>%
  left_join(table_total, by = "quantile_change")

summary_table %>%
  write_csv("data/intermediate-data/quantile change.csv")

# using deciles
table_housing_decile <- combined %>%
  select(housing_decile_change, covid_decile_change, equity_decile_change, total_decile_change) %>%
  count(housing_decile_change) %>%
  mutate(percent_housing = n / sum(n)) %>%
  select(housing_decile_change, percent_housing) %>%
  rename(decile_change = housing_decile_change)

table_covid_decile <- combined %>%
  select(housing_decile_change, covid_decile_change, equity_decile_change, total_decile_change) %>%
  count(covid_decile_change) %>%
  mutate(percent_covid = n / sum(n)) %>%
  select(covid_decile_change, percent_covid) %>%
  rename(decile_change = covid_decile_change)

table_equity_decile <- combined %>%
  select(housing_decile_change, covid_decile_change, equity_decile_change, total_decile_change) %>%
  count(equity_decile_change) %>%
  mutate(percent_equity = n / sum(n)) %>%
  select(equity_decile_change, percent_equity) %>%
  rename(decile_change = equity_decile_change)

table_total_decile <- combined %>%
  select(housing_decile_change, covid_decile_change, equity_decile_change, total_decile_change) %>%
  count(total_decile_change) %>%
  mutate(percent_total = n / sum(n)) %>%
  select(total_decile_change, percent_total) %>%
  rename(decile_change = total_decile_change)

summary_table_decile <- table_housing_decile %>%
  left_join(table_covid_decile, by = "decile_change") %>%
  left_join(table_equity_decile, by = "decile_change") %>%
  left_join(table_total_decile, by = "decile_change")

summary_table_decile %>%
  write_csv("data/intermediate-data/decile change.csv")

### Mapping census tracts that changed by more than +/- 10 percent, at county level -------------

# analysis on census tracts that changed by more than +/- 10 percent
bigchange <- combined %>%
  mutate(bigchangetotal = case_when(total_diff_quantile > 0.10 | total_diff_quantile < -0.10 ~ 1,
                                total_diff_quantile <= 0.10 & total_diff_quantile >= -0.10 ~ 0,
                                T ~ NA_real_),
         bigchangehousing = case_when(housing_diff_quantile > 0.10 | housing_diff_quantile < -0.10 ~ 1,
                                      housing_diff_quantile <= 0.10 & housing_diff_quantile >= -0.10 ~ 0,
                                      T ~ NA_real_),
         bigchangeequity = case_when(equity_diff_quantile > 0.10 | equity_diff_quantile < -0.10 ~ 1,
                                      equity_diff_quantile <= 0.10 & equity_diff_quantile >= -0.10 ~ 0,
                                      T ~ NA_real_),
         bigchangecovid = case_when(covid_diff_quantile > 0.10 | covid_diff_quantile < -0.10 ~ 1,
                                     covid_diff_quantile <= 0.10 & covid_diff_quantile >= -0.10 ~ 0,
                                     T ~ NA_real_),
         counttotal = case_when(bigchangetotal == 0 | bigchangetotal == 1 ~ 1,
                           T ~ NA_real_),
         counthousing = case_when(bigchangehousing == 0 | bigchangehousing == 1 ~ 1,
                                T ~ NA_real_),
         countequity = case_when(bigchangeequity == 0 | bigchangeequity == 1 ~ 1,
                                T ~ NA_real_),
         countcovid = case_when(bigchangecovid == 0 | bigchangecovid == 1 ~ 1,
                                T ~ NA_real_),
         county = substr(GEOID_2019, 1, 5)) %>%
  group_by(county) %>%
  summarize(bigchangetotal = sum(bigchangetotal),
            bigchangehousing = sum(bigchangehousing),
            bigchangeequity = sum(bigchangeequity),
            bigchangecovid = sum(bigchangecovid),
            counttotal = sum(counttotal),
            counthousing = sum(counthousing),
            countequity = sum(countequity),
            countcovid = sum(countcovid)) %>%
  mutate(percent_total = bigchangetotal / counttotal,
         percent_housing = bigchangehousing / counthousing,
         percent_equity = bigchangeequity / countequity,
         percent_covid = bigchangecovid / countcovid)

test <- sum(bigchange$counttotal)

countymap <- left_join(my_counties, bigchange, by = c("county_fips" = "county"))

final_levels <- c("0%","0%-25%","25%-50%","50%-75%","75%-100%")

# total map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap %>% filter(!is.na(percent_total)), mapping = aes(fill=percent_total %>% cut(breaks = c(0,0.00001,0.25,0.5,0.75,1), include.lowest = T)), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = urban_colors, labels = final_levels) +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Total ERAP Index",
       subtitle = "Share of Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("totalshifts.png")

# housing map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap %>% filter(!is.na(percent_housing)), mapping = aes(fill=percent_housing %>% cut(breaks = c(0,0.00001,0.25,0.5,0.75,1), include.lowest = T)), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = urban_colors, labels = final_levels) +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Housing Sub-Index",
       subtitle = "Share of Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("housingshifts.png")

# equity map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap %>% filter(!is.na(percent_equity)), mapping = aes(fill=percent_equity %>% cut(breaks = c(0,0.00001,0.25,0.5,0.75,1), include.lowest = T)), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = urban_colors, labels = final_levels) +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Equity Sub-Index",
       subtitle = "Share of Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("equityshifts.png")

# covid map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap %>% filter(!is.na(percent_covid)), mapping = aes(fill=percent_covid %>% cut(breaks = c(0,0.00001,0.25,0.5,0.75,1), include.lowest = T)), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = urban_colors, labels = final_levels) +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "COVID Sub-Index",
       subtitle = "Share of Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("covidshifts.png")


### Merge in other characteristics ----------

set_urbn_defaults(style = "print")

ruca <- read_excel("data/public-data/ruca2010revised.xlsx", range = "A2:I74004")

ruca_main <- ruca %>%
  rename(GEOID = 4, ruca = 5)

other_char <- combined %>%
  left_join(ruca_main, by = c("GEOID_2019" = "GEOID"))


other_char %>%
  ggplot(mapping = aes(x = total_diff_quantile, y = perc_person_of_color_2019)) + 
  geom_hex(mapping = aes(fill = ..count..)) + 
  scale_x_continuous(limits = c(-1, 1),
                    breaks = -2:2 * 0.5) + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = 0:4 * 0.25) + 
  scale_fill_gradientn(labels = scales::comma) + 
  labs(x = "Change in Total Index Percentile Ranking",
       y = "Percent People of Color") + 
  scatter_grid() + 
  theme(legend.position = "right",
        legend.direction = "vertical")

# % POC
other_char %>%
  filter(covid_diff_quantile > 0.10 | covid_diff_quantile < -.10) %>%
  ggplot(mapping = aes(x = perc_person_of_color_2019, y = covid_diff_quantile)) + 
  geom_hex(mapping = aes(fill = ..count..)) + 
  scale_x_continuous(limits = c(0, 1),
                     breaks = 0:4 * 0.25) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = -2:2 * 0.5) + 
  scale_fill_gradientn(labels = scales::comma) + 
  labs(x = "Percent People of Color",
       y = "Change in COVID Sub-Index Percentile Ranking") + 
  scatter_grid() + 
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("perc_poc_covid.png")

other_char %>%
  filter(housing_diff_quantile > 0.10 | housing_diff_quantile < -.10) %>%
  ggplot(mapping = aes(x = perc_person_of_color_2019, y = housing_diff_quantile)) + 
  geom_hex(mapping = aes(fill = ..count..)) + 
  scale_x_continuous(limits = c(0, 1),
                     breaks = 0:4 * 0.25) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = -2:2 * 0.5) + 
  scale_fill_gradientn(labels = scales::comma) + 
  labs(x = "Percent People of Color",
       y = "Change in Housing Sub-Index Percentile Ranking") + 
  scatter_grid() + 
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("perc_poc_housing.png")

other_char %>%
  filter(covid_diff_quantile > 0.10 | covid_diff_quantile < -.10) %>%
  ggplot(mapping = aes(x = perc_low_income_jobs_lost_2019, y = covid_diff_quantile)) + 
  geom_hex(mapping = aes(fill = ..count..)) + 
  scale_x_continuous(limits = c(0, 0.28),
                     breaks = 0:4 * 0.07) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = -2:2 * 0.5) + 
  scale_fill_gradientn(labels = scales::comma) + 
  labs(x = "Percent Low Income Jobs Lost",
       y = "Change in COVID Sub-Index Percentile Ranking") + 
  scatter_grid() + 
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("perc_low_income_jobs_lost_covid.png")

other_char %>%
  filter(housing_diff_quantile > 0.10 | housing_diff_quantile < -.10) %>%
  ggplot(mapping = aes(x = perc_low_income_jobs_lost_2019, y = housing_diff_quantile)) + 
  geom_hex(mapping = aes(fill = ..count..)) + 
  scale_x_continuous(limits = c(0, 0.28),
                     breaks = 0:4 * 0.07) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = -2:2 * 0.5) + 
  scale_fill_gradientn(labels = scales::comma) + 
  labs(x = "Percent Low Income Jobs Lost",
       y = "Change in Housing Sub-Index Percentile Ranking") + 
  scatter_grid() + 
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("perc_low_income_jobs_lost_housing.png")


# ruca 

bigchangeruca <- combined %>%
  mutate(bigchangetotal = case_when(total_diff_quantile > 0.10 | total_diff_quantile < -0.10 ~ 1,
                                    total_diff_quantile <= 0.10 & total_diff_quantile >= -0.10 ~ 0,
                                    T ~ NA_real_),
         bigchangehousing = case_when(housing_diff_quantile > 0.10 | housing_diff_quantile < -0.10 ~ 1,
                                      housing_diff_quantile <= 0.10 & housing_diff_quantile >= -0.10 ~ 0,
                                      T ~ NA_real_),
         bigchangeequity = case_when(equity_diff_quantile > 0.10 | equity_diff_quantile < -0.10 ~ 1,
                                     equity_diff_quantile <= 0.10 & equity_diff_quantile >= -0.10 ~ 0,
                                     T ~ NA_real_),
         bigchangecovid = case_when(covid_diff_quantile > 0.10 | covid_diff_quantile < -0.10 ~ 1,
                                    covid_diff_quantile <= 0.10 & covid_diff_quantile >= -0.10 ~ 0,
                                    T ~ NA_real_),
         counttotal = case_when(bigchangetotal == 0 | bigchangetotal == 1 ~ 1,
                                T ~ NA_real_),
         counthousing = case_when(bigchangehousing == 0 | bigchangehousing == 1 ~ 1,
                                  T ~ NA_real_),
         countequity = case_when(bigchangeequity == 0 | bigchangeequity == 1 ~ 1,
                                 T ~ NA_real_),
         countcovid = case_when(bigchangecovid == 0 | bigchangecovid == 1 ~ 1,
                                T ~ NA_real_)) %>%
  left_join(ruca_main, by = c("GEOID_2019" = "GEOID")) %>%
  filter(ruca != 99 & !is.na(ruca)) %>%
  group_by(ruca) %>%
  summarize(bigchangetotal = sum(bigchangetotal),
            bigchangehousing = sum(bigchangehousing),
            bigchangeequity = sum(bigchangeequity),
            bigchangecovid = sum(bigchangecovid),
            counttotal = sum(counttotal),
            counthousing = sum(counthousing),
            countequity = sum(countequity),
            countcovid = sum(countcovid)) %>%
  mutate(percent_total = bigchangetotal / counttotal,
         percent_housing = bigchangehousing / counthousing,
         percent_equity = bigchangeequity / countequity,
         percent_covid = bigchangecovid / countcovid) %>%
  mutate(ruca_names = case_when(ruca == 1 ~ "Metro area: Level 1",
                                ruca == 2 ~ "Metro area: Level 2",
                                ruca == 3 ~ "Metro area: Level 3",
                                ruca == 4 ~ "Micro area: Level 1",
                                ruca == 5 ~ "Micro area: Level 2",
                                ruca == 6 ~ "Micro area: Level 3",
                                ruca == 7 ~ "Small town core: Level 1",
                                ruca == 8 ~ "Small town core: Level 2",
                                ruca == 9 ~ "Small town core: Level 3",
                                ruca == 10 ~ "Rural areas"))

library(tidyverse)

bigchangeruca %>% 
  ggplot(mapping = aes(x = factor(ruca_names,
                                  levels = c("Metro area: Level 1", "Metro area: Level 2",
                                             "Metro area: Level 3", "Micro area: Level 1",
                                             "Micro area: Level 2", "Micro area: Level 3",
                                             "Small town core: Level 1", "Small town core: Level 2",
                                             "Small town core: Level 3", "Rural areas")), y = percent_total)) + 
  geom_col() + 
  labs(x = "Share of Census Tracts",
       y = NULL) + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  coord_flip() + 
  labs(title = "Total Index",
       subtitle = "Share of All Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("ruca_total.png")

bigchangeruca %>% 
  ggplot(mapping = aes(x = factor(ruca_names,
                                  levels = c("Metro area: Level 1", "Metro area: Level 2",
                                             "Metro area: Level 3", "Micro area: Level 1",
                                             "Micro area: Level 2", "Micro area: Level 3",
                                             "Small town core: Level 1", "Small town core: Level 2",
                                             "Small town core: Level 3", "Rural areas")), y = percent_housing)) + 
  geom_col() + 
  labs(x = "Share of Census Tracts",
       y = NULL) + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  coord_flip() + 
  labs(title = "Housing Sub-Index",
       subtitle = "Share of All Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("ruca_housing.png")

bigchangeruca %>% 
  ggplot(mapping = aes(x = factor(ruca_names,
                                  levels = c("Metro area: Level 1", "Metro area: Level 2",
                                             "Metro area: Level 3", "Micro area: Level 1",
                                             "Micro area: Level 2", "Micro area: Level 3",
                                             "Small town core: Level 1", "Small town core: Level 2",
                                             "Small town core: Level 3", "Rural areas")), y = percent_equity)) + 
  geom_col() + 
  labs(x = "Share of Census Tracts",
       y = NULL) + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  coord_flip() + 
  labs(title = "Equity Sub-Index",
       subtitle = "Share of All Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("ruca_equity.png")

bigchangeruca %>% 
  ggplot(mapping = aes(x = factor(ruca_names,
                                  levels = c("Metro area: Level 1", "Metro area: Level 2",
                                             "Metro area: Level 3", "Micro area: Level 1",
                                             "Micro area: Level 2", "Micro area: Level 3",
                                             "Small town core: Level 1", "Small town core: Level 2",
                                             "Small town core: Level 3", "Rural areas")), y = percent_covid)) + 
  geom_col() + 
  labs(x = "Share of Census Tracts",
       y = NULL) + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  coord_flip() + 
  labs(title = "COVID Sub-Index",
       subtitle = "Share of All Census Tracts that Changed by More than +/- 10 Percentile Points")

ggsave("ruca_covid.png")


### Comparing improvements and worsening conditions  -------

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
         total_avg_diff = total_index_2019_avg - total_index_2018_avg,
         groupingvar_housing = case_when(housing_avg_diff > 0 ~ "Improvement",
                                         housing_avg_diff == 0 ~ "No Change",
                                         housing_avg_diff < 0 ~ "Worsening",
                                         T ~ NA_character_),
         groupingvar_covid = case_when(covid_avg_diff > 0 ~ "Improvement",
                                       covid_avg_diff == 0 ~ "No Change",
                                       covid_avg_diff < 0 ~ "Worsening",
                                         T ~ NA_character_),
         groupingvar_equity = case_when(equity_avg_diff > 0 ~ "Improvement",
                                        equity_avg_diff == 0 ~ "No Change",
                                        equity_avg_diff < 0 ~ "Worsening",
                                         T ~ NA_character_),
         groupingvar_total = case_when(total_avg_diff > 0 ~ "Improvement",
                                       total_avg_diff == 0 ~ "No Change",
                                       total_avg_diff < 0 ~ "Worsening",
                                         T ~ NA_character_)) 



countymap_direction <- left_join(my_counties, countydirection, by = c("county_fips" = "county"))

#final_levels <- c("0%","0%-25%","25%-50%","50%-75%","75%-100%")

# housing map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(housing_avg_diff)), mapping = aes(fill=housing_avg_diff), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_color_gradient2(low = "#DB2B27",
                        mid = "#ffffff",
                        high = "#0A4C6A",
                        midpoint = 0,
                        aesthetics = "colour") +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Housing Sub-Index",
       subtitle = "Counties with Improvements (+) or Worsening Conditions (-)")

# covid map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(covid_avg_diff)), mapping = aes(fill=covid_avg_diff), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_color_gradient2(low = "#DB2B27",
                        mid = "#ffffff",
                        high = "#0A4C6A",
                        midpoint = 0,
                        aesthetics = "colour") +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Covid Sub-Index",
       subtitle = "Counties with Improvements (+) or Worsening Conditions (-)")


# equity map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(equity_avg_diff)), mapping = aes(fill=equity_avg_diff), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_color_gradient2(low = "#DB2B27",
                        mid = "#ffffff",
                        high = "#0A4C6A",
                        midpoint = 0,
                        aesthetics = "colour") +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Equity Sub-Index",
       subtitle = "Counties with Improvements (+) or Worsening Conditions (-)")

#total map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(total_avg_diff)), mapping = aes(fill=total_avg_diff), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_color_gradient2(low = "#DB2B27",
                        mid = "#ffffff",
                        high = "#0A4C6A",
                        midpoint = 0,
                        aesthetics = "colour") +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Total Sub-Index",
       subtitle = "Counties with Improvements (+) or Worsening Conditions (-)")









  