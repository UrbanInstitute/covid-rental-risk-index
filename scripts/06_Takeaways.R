# 2018 indicator values 
ERAP_2018 <- read_csv("data/intermediate-data/housing_index_state_adj_2018.csv") 

ERAP_2018_indicators <- ERAP_2018 %>%
  select(GEOID, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
        perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
        perc_public_assistance, perc_foreign_born,perc_30hamfi,perc_low_income_jobs_lost            
        ) %>%
        rename_all(paste0, "_2018")

# 2019 indicator values
ERAP_2019 <- read_csv("data/intermediate-data/housing_index_state_adj_2019.csv")
ERAP_2019_indicators <- ERAP_2019 %>%
  select(GEOID, perc_cost_burdened_under_35k, perc_overcrowding_renter_1.50_or_more, perc_unemployed_laborforce, 
         perc_renters, perc_poverty_12mnth, perc_no_hinsure, perc_person_of_color, 
         perc_public_assistance, perc_foreign_born, perc_30hamfi, perc_low_income_jobs_lost            
        ) %>%
         rename_all(paste0, "_2019")

##combine 2018 and 2019 indicators into one data frame, generate difference by indicator
indicators_combined <- ERAP_2019_indicators %>%
  left_join(ERAP_2018_indicators, by = c("GEOID_2019" = "GEOID_2018")) %>%
  mutate(cost_burden_diff = perc_cost_burdened_under_35k_2019 - perc_cost_burdened_under_35k_2018,
         overcrowding_diff = perc_overcrowding_renter_1.50_or_more_2019 - perc_overcrowding_renter_1.50_or_more_2018,
         poverty_diff = perc_poverty_12mnth_2019 - perc_poverty_12mnth_2018,
         renters_diff = perc_renters_2019 - perc_renters_2018,
         unemployment_diff = perc_unemployed_laborforce_2019 - perc_unemployed_laborforce_2018,
         jobslost_diff = perc_low_income_jobs_lost_2019 - perc_low_income_jobs_lost_2018,
         nohinsure_diff = perc_no_hinsure_2019 - perc_no_hinsure_2018,
        )
  

##absolute values of difference
indicators_combined$cost_burden_abs <-abs(indicators_combined$cost_burden_diff) 
indicators_combined$overcrowding_abs <- abs(indicators_combined$overcrowding_diff)
indicators_combined$poverty_abs <- abs(indicators_combined$poverty_diff)
indicators_combined$renters_abs <- abs(indicators_combined$renters_diff)
indicators_combined$unemployment_abs <- abs(indicators_combined$unemployment_diff)
indicators_combined$jobslost_abs <- abs(indicators_combined$jobslost_diff)
indicators_combined$nohinsure_abs <- abs(indicators_combined$nohinsure_diff)

##sum by index 
indicators_diff <- indicators_combined %>%
                   select(poverty_abs, renters_abs, cost_burden_abs,
                           overcrowding_abs, unemployment_abs, jobslost_abs, nohinsure_abs)%>%
                   mutate (housing_total_change = poverty_abs + renters_abs + cost_burden_abs +
                           overcrowding_abs + unemployment_abs) %>%
                   mutate (covid_total_change = nohinsure_abs + `jobslost_abs`)
                      
##write out
indicators_diff %>%
                  write_csv("data/intermediate-data/housing_covid_indicators_change.csv")