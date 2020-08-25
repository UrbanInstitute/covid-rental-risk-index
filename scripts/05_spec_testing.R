library(tidyverse)

## Specification 2

# Assuming equal weights for other equity indicators, this sets the person of
# color weight and the other weights are automatically calculated in the df
poc_weight_in_index = 0.5

# Create df with configurable weights for each indicator 
indicator_weights = tribble(
  ~indicator,                 ~housing_index_weight, ~covid_index_weight, ~equity_index_weight,
  "perc_cost_burdened_under_35k",                 0.2,         NA_integer_,         NA_integer_,
  "perc_overcrowding_renter_1.50_or_more",        0.2,         NA_integer_,         NA_integer_,
  "perc_unemployed_laborforce",                   0.2,         NA_integer_,         NA_integer_,
  "perc_renters",                                 0.2,         NA_integer_,         NA_integer_,
  "perc_poverty_12mnth",                          0.2,         NA_integer_,         NA_integer_,
  "perc_person_of_color",                  NA_integer_,        NA_integer_,  poc_weight_in_index,   
  "perc_foreign_born",                     NA_integer_,        NA_integer_,(1-poc_weight_in_index)/3,
  "perc_public_assistance",                NA_integer_,        NA_integer_,(1-poc_weight_in_index)/3,
  "perc_30hamfi",                          NA_integer_,        NA_integer_,(1-poc_weight_in_index)/3,
  "perc_low_income_jobs_lost",             NA_integer_,                0.5,         NA_integer_,
  "perc_no_hinsure",                       NA_integer_,                0.5,         NA_integer_,
  
) 

# Weights for three composite indicators (housing, equity, covid) in that order
index_weights = c(0.5, 0.4, 0.1)



## Specification 3
