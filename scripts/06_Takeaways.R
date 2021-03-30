# 2018 index values 
ERAP_2018 <- read_csv("data/intermediate-data/housing_index_state_adj_2018.csv") 

ERAP_2018_indicators <- ERAP_2018 %>%
  select(GEOID, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
        perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
        perc_public_assistance, perc_foreign_born,perc_30hamfi,perc_low_income_jobs_lost            
        ) %>%
        rename_all(paste0, "_2018")

# 2019 index values
ERAP_2019 <- read_csv("data/intermediate-data/housing_index_state_adj_2019_ajjit.csv")
ERAP_2019_indicators <- ERAP_2019 %>%
  select(GEOID, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
         perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
         perc_public_assistance, perc_foreign_born,perc_30hamfi,perc_low_income_jobs_lost            
        ) %>%
         rename_all(paste0, "_2019")


combined <- ERAP_2019_indicators %>%
  left_join(ERAP_2018_indicators, by = c("GEOID_2019" = "GEOID_2018")) %>%
  filter(!is.na(housing_index_2018) & !is.na(covid_index_2018) & !is.na(equity_index_2018) & !is.na(total_index_2018)) %>%
  mutate(housing_diff = housing_index_2019 - housing_index_2018,
         covid_diff = covid_index_2019 - covid_index_2018,
         equity_diff = equity_index_2019 - equity_index_2018,
         total_diff = total_index_2019 - total_index_2018,
         housing_diff_quantile = housing_index_quantile_2019 - housing_index_quantile_2018,
         covid_diff_quantile = covid_index_quantile_2019 - covid_index_quantile_2018,
         equity_diff_quantile = equity_index_quantile_2019 - equity_index_quantile_2018,
         total_diff_quantile = total_index_quantile_2019 - total_index_quantile_2018)