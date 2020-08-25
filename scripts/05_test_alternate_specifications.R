library(tidyverse)
library(sf)
# Helper functions
z_scored_by_state = read_csv("data/intermediate-data/z-scored-by-state.csv")

index_orig = st_read("data/intermediate-data/housing_index_state_adj.geojson")
index_orig = index_orig %>% 
  select(GEOID:total_index)



###-----Set up helper functions---------

# Helper function for weighted sum across columns (used for total index score)
row_sum_weighted = function(df, weight_vec){
  # Function to create weighted rowwise sum. Used only for total index score
  # INPUT:
  #   df: A datframe of just the columne you would like rowwise sums of
  #   weight_vec: A vector of weights. Must be same length as ncol(df)
  # OUPU:
  #   result: a single vector that is the weighted sum across the columns. Will
  #   have same number of rows as df
  
  df_m = df %>% as.matrix()
  sum_matrix = (drop(df_m %*% weight_vec))
  sum_matrix %>% as.vector()
}

# Helper function to perform calculations and generate indices 
generate_index = function(df, 
                          indicator_weights_df,
                          index_weights_vec, 
                          tracts_18 = NA,
                          rescale_indices = FALSE){
  # Function to generate covid, housing, equity, and total index given weights. 
  # INPUT:
  #   df: df of z scores of all indicators
  #   indicator_weights_df: df of weights for all the indicators. See'
  #     indicator_weights object for an example
  #   index_weights_vec: vector of length 3, which contains weights fo housing, 
  #     equity, and covid index (in that order)
  #   tracts_18: An sf dataframe of 2018 tracts with a GEOID column. If NA, this
  #     function will download in the data from the Job Loss tool data files
  # OUTPUT:
  #   result: An sf dataframe with 72k+ rows each row representing a tract
  
  indexed_data = df %>% 
    pivot_longer(cols = starts_with("perc"),
                 names_to = "indicator") %>% 
    left_join(indicator_weights_df, by = "indicator") %>% 
    group_by(GEOID) %>% 
    # Since this is a long dataframe, the product across 2 columns, 
    # and summed across all values in a tract will return the index score
    summarize(housing_index = sum(value*housing_index_weight, na.rm = T),
              covid_index = sum(value*covid_index_weight, na.rm = T),
              equity_index= sum(value*equity_index_weight, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(
      total_index = 
        row_sum_weighted(select(., housing_index, 
                                equity_index,
                                covid_index),
                         weight_vec = index_weights_vec)) 
  
  
  # Check that nrows of indexes line up with nrows of initial dataframe
  assert(nrow(indexed_data) == nrow(df))
  
  # Check that manual sums of covid, housing, and equity indices add up
  assert(dplyr::all_equal(
    df %>% slice(1:10) %>% 
      select(GEOID, perc_low_income_jobs_lost, perc_no_hinsure) %>% 
      mutate(covid_index = row_sum_weighted(select(.,
                                                   perc_low_income_jobs_lost, 
                                                   perc_no_hinsure ),
                                            weight_vec = indicator_weights %>%
                                              pull(covid_index_weight) %>%
                                              na.omit())),
    indexed_data %>% slice(1:10)
  ))
  
  
  if(rescale_indices){
    # rescale_indices to be between 0 and 100 if option is set
    
    indexed_data = indexed_data %>% 
      mutate(across(contains("index"), ~scales::rescale(.x, to = c(0,100))))
  }
  
  
  
  ntile_100 = function(x){
    ntile(x, n = 99)
  }
  
  # Left join indexes to Z scores 
  result = df %>% 
    left_join(indexed_data, by = "GEOID") %>% 
    # append state quantiles for all indicators and indices
    group_by(state_name) %>%
    # Append many addnl columsn with percent ranking of the indicators and indices
    mutate(across(starts_with("perc")|ends_with("index"), list(quantile = ntile_100))) %>% 
    mutate(across(ends_with("quantile"), ~.x/100))
  
  
  # Attach geometries for each tract using data files from Job Loss Tool
  
  # If tracts isn't given in fxn call, then download in from Job Loss Tool data
  if (is.na(tracts_18)){
    tracts_18_job_loss = 
      st_read("https://ui-lodes-job-change-public.s3.amazonaws.com/job_loss_by_tract.geojson")
    tracts_18 = tracts_18_job_loss %>% 
      select(GEOID, geometry)
  } 
  
  result_with_geoms = result %>% 
    right_join(tracts_18, by = "GEOID") %>% 
    st_as_sf()
  
  return(result_with_geoms)
  
  
}

## Specification 2

# Assuming equal weights for other equity indicators, this sets the person of
# color weight and the other weights are automatically calculated in the df
poc_weight_in_index = 0.5

# Create df with configurable weights for each indicator 
indicator_weights_1 = tribble(
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
index_weights_1 = c(0.55, 0.45, 0)


## Specification 3


# Assuming equal weights for other equity indicators, this sets the person of
# color weight and the other weights are automatically calculated in the df
poc_weight_in_index = 0.5

# Create df with configurable weights for each indicator 
indicator_weights_2 = tribble(
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
  "perc_low_income_jobs_lost",             NA_integer_,                0.75,         NA_integer_,
  "perc_no_hinsure",                       NA_integer_,                0.25,         NA_integer_,
  
) 

# Weights for three composite indicators (housing, equity, covid) in that order
index_weights_2 = c(0.45, 0.35, 0.2)

index_1 = generate_index(df = z_scored_by_state, indicator_weights_df = indicator_weights_1, index_weights_vec = index_weights_1)
index_2 = generate_index(df = z_scored_by_state, indicator_weights_df = indicator_weights_2, index_weights_vec = index_weights_2)


index_1 = index_1 %>% 
  select(GEOID, contains("state"), contains("county"), ends_with("index")) %>%
  st_drop_geometry() %>% 
  ungroup()
index_2 = index_2 %>% 
  select(GEOID, contains("state"), contains("county"), ends_with("index")) %>% 
  st_drop_geometry() %>% 
  ungroup()


diff_total_indices = index_orig %>% 
  left_join(index_1 %>% select(GEOID, ends_with("index")), by = "GEOID", suffix = c("", "_1")) %>% 
  left_join(index_2 %>% select(GEOID, ends_with("index")), by = "GEOID", suffix = c("", "_2")) %>% 
  mutate(diff_orig_1 = total_index - total_index_1,
         diff_orig_2 = total_index - total_index_2) %>% 
  select(GEOID:grayed_out, total_index, starts_with("diff"))


difference_hists = diff_total_indices %>% 
  rename(no_covid_weight = diff_orig_1,
         increased_covid_weight = diff_orig_2) %>% 
  pivot_longer(cols = ends_with("weight"), names_to = "diff", values_to = "value") %>% 
  mutate(diff = fct_rev(diff)) %>% 
  select(GEOID, state_name, state_fips, county_name, diff, value) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(col = "white", bins= 90) +
  facet_wrap(~diff) +
  labs(x = "Total Index Difference", y = "Count of Census tracts")


### Look at the top 10% of tracts in our current index and see whether they remain
# in the top 10% under our alternate specifications

top_10_perc_orign = index_orig %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())


top_10_perc_1 = index_1 %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())

top_10_perc_2 = index_2 %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())

# To easily see the percent of rows that are filtered out
library(tidylog)
geoids_top_10_percent = top_10_perc_orign %>% pull(GEOID)

# 10% of rows filtered out
top_10_perc_1 %>% 
  filter(GEOID %in% geoids_top_10_percent)

# 12% of rows filtered out
top_10_perc_2 %>% 
  filter(GEOID %in% geoids_top_10_percent)




### By state, look at the top 10% of tracts in our current index and see whether they remain
# in the top 10% under our alternate specifications
top_10_perc_orign = index_orig %>% 
  group_by(state_fips) %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())


top_10_perc_1 = index_1 %>% 
  group_by(state_fips) %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())

top_10_perc_2 = index_2 %>% 
  group_by(state_fips) %>% 
  slice_max(order_by = total_index, prop = 0.1) %>% 
  arrange(desc(total_index)) %>% 
  select(total_index, everything())

geoids_top_10_percent = top_10_perc_orign %>% pull(GEOID)

# 6% of rows filtered out
top_10_perc_1 %>% 
  filter(GEOID %in% geoids_top_10_percent)

# 9% of rows filtered out
top_10_perc_2 %>% 
  filter(GEOID %in% geoids_top_10_percent)


