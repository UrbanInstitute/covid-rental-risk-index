# Reads in new tool and old tool and compares values

library(tidyverse)
library(tidycensus)
library(purrr)
library(sf)
library(readxl)
library(urbnthemes)
library(scales)
library(urbnmapr)

set_urbn_defaults(style = "map")
options(scipen=999)
urban_colors <- c("#cfe8f3","#73bfe2","#1696d2","#0a4c6a","#000000","#dfdfdf")

my_counties <- get_urbn_map("counties", sf=T)
my_states <- get_urbn_map("states", sf=T)



# 2018 index values 
ERAP_2018 <- read_csv("data/intermediate-data/housing_index_state_adj_2018.csv") 
ERAP_2018_main <- ERAP_2018 %>%
  select(GEOID, housing_index, covid_index, equity_index, total_index, housing_index_quantile, 
         covid_index_quantile, equity_index_quantile, total_index_quantile) %>%
  rename_all(paste0, "_2018")

# 2019 index values
ERAP_2019 <- read_csv("data/intermediate-data/housing_index_state_adj_2019.csv")
ERAP_2019_main <- ERAP_2019 %>%
  select(GEOID, housing_index, covid_index, equity_index, total_index, housing_index_quantile, 
         covid_index_quantile, equity_index_quantile, total_index_quantile) %>%
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
  mutate(morethan10 = case_when(total_diff_quantile > 0.10 | total_diff_quantile < -0.10 ~ 1,
                                total_diff_quantile <= 0.10 & total_diff_quantile >= -0.10 ~ 0,
                                T ~ NA_real_),
         count = case_when(morethan10 == 0 | morethan10 == 1 ~ 1,
                           T ~ NA_real_),
         county = substr(GEOID_2019, 1, 5)) %>%
  group_by(county) %>%
  summarize(morethan10 = sum(morethan10),
            count = sum(count)) %>%
  mutate(percent = morethan10 / count)

test <- sum(bigchange$count)

countymap <- left_join(my_counties, bigchange, by = c("county_fips" = "county"))

final_levels <- c("0%","0%-25%","25%-50%","50%-75%","75%-100%")


ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap %>% filter(!is.na(percent)), mapping = aes(fill=percent %>% cut(breaks = c(0,0.00001,0.25,0.5,0.75,1), include.lowest = T)), color = "#ffffff", size = .05) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = urban_colors, labels = final_levels) +
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1, color = "white") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("shareshifts.png")


### Merge in other characteristics ----------

ruca <- read_excel("data/public-data/ruca2010revised.xlsx", range = "A2:I74004")

ruca_main <- ruca %>%
  rename(GEOID = 4, ruca = 5)






