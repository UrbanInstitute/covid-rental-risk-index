# Analysis of changes in interactive 

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


# 2018 indicator values 
ERAP_2018 <- read_csv("data/intermediate-data/housing_index_state_adj_2018.csv") 

ERAP_2018_index <- ERAP_2018 %>%
  select(GEOID, housing_index_quantile, covid_index_quantile, equity_index_quantile, total_index_quantile) %>%
  rename_all(paste0, "_2018")

# 2019 indicator values
ERAP_2019 <- read_csv("data/intermediate-data/housing_index_state_adj_2019.csv")

ERAP_2019_index <- ERAP_2019 %>%
  select(GEOID, housing_index_quantile, covid_index_quantile, equity_index_quantile, total_index_quantile) %>%
  rename_all(paste0, "_2019")

changes_in_interactive <- ERAP_2019_index %>%
  left_join(ERAP_2018_index, by = c("GEOID_2019" = "GEOID_2018")) %>%
  filter(!is.na(housing_index_quantile_2018) & !is.na(covid_index_quantile_2018) & !is.na(equity_index_quantile_2018) & !is.na(total_index_quantile_2018)) %>%
  select(GEOID_2019, total_index_quantile_2019, total_index_quantile_2018) %>%
  mutate(cat_2019 = case_when(total_index_quantile_2019 < 0.50 ~ "Cat 1",
                              total_index_quantile_2019 >= 0.50 & total_index_quantile_2019 < 0.75 ~ "Cat 2",
                              total_index_quantile_2019 >= 0.75 & total_index_quantile_2019 < 0.85 ~ "Cat 3",
                              total_index_quantile_2019 >= 0.85 & total_index_quantile_2019 < 0.90 ~ "Cat 4",
                              total_index_quantile_2019 >= 0.90 & total_index_quantile_2019 < 0.95 ~ "Cat 5",
                              total_index_quantile_2019 >= 0.95 ~ "Cat 6"),
         cat_2018 = case_when(total_index_quantile_2018 < 0.50 ~ "Cat 1",
                              total_index_quantile_2018 >= 0.50 & total_index_quantile_2018 < 0.75 ~ "Cat 2",
                              total_index_quantile_2018 >= 0.75 & total_index_quantile_2018 < 0.85 ~ "Cat 3",
                              total_index_quantile_2018 >= 0.85 & total_index_quantile_2018 < 0.90 ~ "Cat 4",
                              total_index_quantile_2018 >= 0.90 & total_index_quantile_2018 < 0.95 ~ "Cat 5",
                              total_index_quantile_2018 >= 0.95 ~ "Cat 6"),
         change = case_when(cat_2019 != cat_2018 ~ 1,
                            cat_2019 == cat_2018 ~ 0))


table(changes_in_interactive$change)









