library(tidyverse)
library(tidycensus)
library(purrr)
library(sf)
library(readxl)
# effectsize is for standardizing vars with z scores
library(effectsize)
library(testit)
# remotes::install_github("UrbanInstitute/urbnthemes")
library(urbnthemes)

set_urbn_defaults()

# Load variable metadata from ACS 5 yr
vars_list <- load_variables(year = 2018, dataset = "acs5", cache = TRUE)

# View table of all ACS variables in another tab
#View(vars_list)

# get list of states to pull data for
my_states <- fips_codes %>%
  # filter out territories
  filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) %>%
  pull(state) %>%
  unique()

### ----Pull ACS Indicators---------

# Cost Burdened Indicator
cb_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "B25074",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
# Calculate percent of housholds making under 35k who pay more than 50% of their
# income on rent
cb_stats <- cb_vars %>%
  # select ACS table variables w/ attached GEOID
  select(
    # These are all the peolpe making under 35k (denominator)
    B25074_002E, B25074_011E, B25074_020E,
    # These are all the people making under 35k who pay more than 50% of thier income on rent (numerator)
    B25074_009E, B25074_018E, B25074_027E, 
    # These are the people making under 35k for whom this metric wasn't computed
    # and they therefore need to be subtracted from the denominator
    B25074_010E, B25074_019E, B25074_028E,
    GEOID
  ) %>%
  # create cost burden variable w/ calculation
  mutate(perc_cost_burdened_under_35k = (B25074_009E + B25074_018E + B25074_027E) /
         (B25074_002E + B25074_011E + B25074_020E - B25074_010E - B25074_019E -
            B25074_028E),
         #cd add
         perc_cost_burdened_under_35k =
          if_else(B25074_002E + B25074_011E + B25074_020E == 0, 0, 
                  perc_cost_burdened_under_35k))

# Overcrowding Indicator
oc_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "B25014",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
# pipe function to calculate percentage renter overcrowding
# rename to oc_stats
oc_stats <- oc_vars %>%
  # select ACS table variables w/ attached GEOID
  select(B25014_013E, B25014_012E, B25014_008E, GEOID) %>%
  # create cost burden variable w/ calculation
  mutate(perc_overcrowding_renter_1.50_or_more = ((B25014_012E + B25014_013E) / B25014_008E),
         perc_overcrowding_renter_1.50_or_more = 
           if_else(B25014_008E == 0, 0, perc_overcrowding_renter_1.50_or_more))


# Unemployment Indicator Calculations
# rename to un_vars
un_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    variables = NULL,
    table = "B12006",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
# pipe function to calculate percentage unemployed
# rename to un_stats
un_stats <- un_vars %>%
  # select ACS table variables w/ attached GEOID
  select(B12006_055E, B12006_050E, B12006_011E, B12006_006E, B12006_022E, B12006_017E, B12006_033E, B12006_028E, B12006_044E, B12006_039E, B12006_053E, B12006_048E, B12006_009E, B12006_004E, B12006_020E, B12006_015E, B12006_031E, B12006_026E, B12006_042E, B12006_037E, GEOID) %>%
  # create cost burden variable w/ calculation
  mutate(perc_unemployed_laborforce = ((B12006_055E + B12006_050E + B12006_011E + B12006_006E + B12006_022E + B12006_017E + B12006_033E + B12006_028E + B12006_044E + B12006_039E) / (B12006_053E + B12006_048E + B12006_009E + B12006_004E + B12006_020E + B12006_015E + B12006_031E + B12006_026E + B12006_042E + B12006_037E)),
         perc_unemployed_laborforce = 
           if_else(B12006_053E + B12006_048E + B12006_009E + B12006_004E + B12006_020E + B12006_015E + B12006_031E + B12006_026E + B12006_042E + B12006_037E == 0, 0,
                   perc_unemployed_laborforce))

# Share Renter Indicator Calculations
# rename to sr_vars
sr_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    variables = NULL,
    table = "B25003",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
# pipe function to calculate percentage of renter occupied housing units
# rename to sr_stats
sr_stats <- sr_vars %>%
  # select ACS table variables w/ attached GEOID
  select(B25003_003E, B25003_001E, GEOID) %>%
  # create Share renters variable w/ calculation
  mutate(perc_renters = (B25003_003E / B25003_001E),
         perc_renters = if_else(B25003_001E == 0, 0, perc_renters),
         num_renters = B25003_003E)

num_renters <-  sr_stats %>% select(GEOID, num_renters)


# Poverty Indicator Calculations
# rename to pv_vars
pv_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    variables = NULL,
    table = "C17002",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
# pipe function to calculate percentage of population in poverty in the last 12 months
# rename to pv_stats
pv_stats <- pv_vars %>%
  # select ACS table variables w/ attached GEOID
  select(C17002_002E, C17002_003E, C17002_001E, GEOID) %>%
  # create Share renters variable w/ calculation
  mutate(perc_poverty_12mnth = ((C17002_002E + C17002_003E) / C17002_001E),
         perc_poverty_12mnth = 
           if_else(C17002_001E == 0, 0, perc_poverty_12mnth))

# does not have health insurance indicator
hinsure_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "C27012",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)

hinsure_stats <- hinsure_vars %>%
  select(
    # These are populations without health insurance by work status 
    # (Full Time, Less than Full Time, Not Working)
    FT_nHI = C27012_008E, 
    LFT_nHI = C27012_015E, 
    NW_nHI = C27012_022E,
    # These are total populations by work status
    FT_total = C27012_002E, 
    LFT_total = C27012_009E, 
    NW_total = C27012_016E, GEOID) %>%
  mutate(perc_no_hinsure = (FT_nHI + LFT_nHI + NW_nHI) / (FT_total + LFT_total + NW_total),
          perc_no_hinsure = 
           if_else(FT_total + LFT_total + NW_total == 0, 0, perc_no_hinsure))


# race indicator
race_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "B03002",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
race_stats <- race_vars %>%
  select(
    total = B03002_001E, NHBlack = B03002_004E, NHWhite = B03002_003E, Hispanic = B03002_012E, NHAmerIn = B03002_005E,
    NHAsian = B03002_006E, NHPacificIsl = B03002_007E, NHOther = B03002_008E, NHmorethanone = B03002_009E, GEOID
  ) %>%
  mutate(perc_NHBlack = NHBlack / total,
         perc_NHBlack = 
          if_else(total == 0, 0, perc_NHBlack)) %>%
  mutate(perc_Hispanic = Hispanic / total,
         perc_Hispanic = 
           if_else(total == 0, 0, perc_Hispanic)) %>%
  mutate(perc_otherPOC = (NHAmerIn + NHAsian + NHPacificIsl + NHOther + NHmorethanone) / total,
         perc_otherPOC = 
           if_else(total == 0, 0, perc_otherPOC)) %>%
  # mutate (perc_white = NHWhite / total) %>% PART OF CHECK TO SEE IF CALCULATIONS WORKED
  mutate(perc_person_of_color = (perc_NHBlack + perc_Hispanic + perc_otherPOC))
# mutate (check = (perc_white + perc_person_of_color)) CHECKING TO SEE IF CALCULATIONS WORKED

# public assistance indicator
pa_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "B19057",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
pa_stats <- pa_vars %>%
  select(total = B19057_001E, total_public_assistance = B19057_002E, GEOID) %>%
  mutate(perc_public_assistance = total_public_assistance / total,
         perc_public_assistance = 
           if_else(total == 0, 0, perc_public_assistance))

# foreign born indicator
fb_vars <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "tract",
    state = .,
    table = "B05002",
    year = 2018,
    survey = "acs5",
    output = "wide"
  )
)
fb_stats <- fb_vars %>%
  select(total = B05002_001E, total_foreign_born = B05002_013E, GEOID) %>%
  mutate(perc_foreign_born = total_foreign_born / total,
         perc_foreign_born =
           if_else(total == 0, 0, perc_foreign_born))


### ---Pull HUD CHAS data------

# Income Indicator
# Download in Zip file from HUD website, unzip and rename
download.file("https://www.huduser.gov/portal/datasets/cp/2012thru2016-140-csv.zip", 
              destfile = "data/raw-data/hud_files.zip",
              method = "libcurl")
unzip("data/raw-data/hud_files.zip", 
      files = "2012thru2016-140-csv/2012thru2016-140-csv/140/Table8.csv",
      exdir = "data/raw-data")

file.rename("data/raw-data/2012thru2016-140-csv/2012thru2016-140-csv/140/Table8.csv",
            "data/raw-data/Table8.csv")

income_vars <- read_csv("data/raw-data/Table8.csv")
income_stats <- income_vars %>%
  select(old_geoid = geoid, total_ro = T8_est68, ro_30hamfi = T8_est69) %>%
  mutate(
    perc_30hamfi = ro_30hamfi / total_ro,
    perc_30hamfi = 
      if_else(total_ro == 0, 0, perc_30hamfi),
    num_ELI = ro_30hamfi,
    GEOID = str_replace_all(old_geoid, "14000US", "")
  )

num_ELI <- income_stats %>% select(GEOID, num_ELI)

### ----Pull Estimated Job Loss Data-------

# low income job loss indicator
# Read in data on low income jobs lost per tract from Urban Data Catalog
covid_low_income_job_vars <- read_csv("https://ui-lodes-job-change-public.s3.amazonaws.com/job_loss_by_tract.csv")
covid_low_income_job_stats <- covid_low_income_job_vars %>%
  select(GEOID, state_name, county_name, state_fips, county_fips,
    perc_low_income_jobs_lost = low_income_worker_job_loss_rate
  )

###------Data Cleaning---------

# Compile into one dataframe
indicator_data <- cb_stats %>%
  left_join(oc_stats, by = "GEOID") %>%
  left_join(un_stats, by = "GEOID") %>%
  left_join(sr_stats, by = "GEOID") %>%
  left_join(pv_stats, by = "GEOID") %>%
  left_join(hinsure_stats, by = "GEOID") %>%
  left_join(race_stats, by = "GEOID") %>%
  left_join(pa_stats, by = "GEOID") %>%
  left_join(fb_stats, by = "GEOID") %>%
  left_join(income_stats, by = "GEOID") %>%
  left_join(covid_low_income_job_stats, by = "GEOID") %>%
  # Note this drops all the individual ACS variables, the extra CHAS columns 
  # (like the num_ELI variable) and the num_renters column
  select(GEOID, state_name, county_name, state_fips, county_fips, starts_with("perc"))



# Do some data cleaning/imputation
indicator_data = indicator_data %>% 
  # There are 319 tracts where perc_low_incomes_jobs_lost is NA. 
  # I believe these are all water tracts as all the other indicators are also 0 in
  # these 319 rows. We filter these out
  # TODO: Problem with Covid data where one tract has value of NA
  filter(!is.na(perc_low_income_jobs_lost)) %>% 
  # Deselect unused race indicators

  select(-perc_NHBlack, -perc_Hispanic, -perc_otherPOC) %>% 
  # 9 tracts in New Mexico have NA values for a few metrics, perhaps due to Census
  # suppression. So we replace those NA values with the national mean
  mutate(perc_cost_burdened_under_35k = 
           if_else(is.na(perc_cost_burdened_under_35k), 
                   mean(perc_cost_burdened_under_35k, na.rm = T),
                   perc_cost_burdened_under_35k),
         perc_poverty_12mnth = 
           if_else(is.na(perc_poverty_12mnth), 
                   mean(perc_poverty_12mnth, na.rm = T),
                   perc_poverty_12mnth),
         perc_no_hinsure = 
           if_else(is.na(perc_no_hinsure), 
                   mean(perc_no_hinsure, na.rm = T),
                   perc_no_hinsure),
         perc_public_assistance =
           if_else(is.na(perc_public_assistance), 
                   mean(perc_public_assistance, na.rm = T),
                   perc_public_assistance)
           )

###---Convert to Z scores---------------------------

# Convert all numeric columns (ie the perc* columns) into Z-scores

# Z scores for the nation
z_scored_us = indicator_data %>% 
  # standardizes all numeric columns
  effectsize::standardize()

# Z scores by state
z_scored_by_state = indicator_data %>% 
  group_by(state_name) %>% 
  # standardizes all numeric columns
  effectsize::standardize() %>% 
  ungroup() 

# Create plot comparing Z score (for perc_public_assistance) by state and by US
diff_z_score_state_natl = z_scored_by_state %>% 
  select(z_score_state = perc_public_assistance, GEOID, state_name) %>% 
  left_join(z_scored_us %>% select(z_score_nat = perc_public_assistance, GEOID), by = "GEOID") %>% 
  ggplot(aes(x = z_score_state, y = z_score_nat)) +
  geom_point(alpha = 0.1, size = 0.6) +
  geom_abline(aes(intercept = 0, slope = 1), col = palette_urbn_magenta[4]) +
  labs(x = "Z Score by State",
       y = "Z Score (National)",
       title = "Z score comparison for % Public Assistance Variable")


# Write out plot
ggsave("output/z-score-comparison-state-natl.png")



###----Set up Weights---------


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

###---Create Indexes---------

tracts_18_job_loss = 
  st_read("https://ui-lodes-job-change-public.s3.amazonaws.com/job_loss_by_tract.geojson")
tracts_18 = tracts_18_job_loss %>% 
  select(GEOID, geometry)

indexed_data_us = generate_index(z_scored_us,
                                 indicator_weights,
                                 index_weights,
                                 tracts_18)

indexed_data_us <- indexed_data_us %>%
  left_join(num_ELI, by = "GEOID") %>% 
  left_join(num_renters, by = "GEOID") %>% 
  ungroup() %>% 
  # Remember the indexed_data perc values are z scores. Rename columns to make
  # that clear
  rename_with(~ paste0("z_score_", .x), starts_with("perc_"))

indexed_data_by_state = generate_index(z_scored_by_state, 
                                       indicator_weights, 
                                       index_weights,
                                       tracts_18,
                                      rescale_indices = F)

indexed_data_by_state <- indexed_data_by_state %>%
  left_join(num_ELI, by = "GEOID") %>% 
  left_join(num_renters, by = "GEOID") %>% 
  # For some reason when you use rename_with on a grouped sf dataframe, 
  # it becomes a tibble? So we ungroup
  ungroup() %>% 
  rename_with(~ paste0("z_score_", .x), starts_with("perc_")) %>% 
  # Add flag for graying out tracts with less than 50 renters for Comms
  mutate(grayed_out = ifelse(num_ELI <= 0, 1, 0)) %>% 
  relocate(num_renters, num_ELI, grayed_out, ends_with("index"), ends_with("index_quantile"), 
           .after = county_fips)


# Look at one weird tract in CA which highlights problems with rescaled index values
# indexed_data_by_state %>% filter(GEOID == "06067007101") %>% select(GEOID, contains("index")) %>%  mapview()

full_data_state_indices = indicator_data %>% 
  left_join(indexed_data_by_state %>% 
              dplyr::select(GEOID, contains("index") |
                              (starts_with("z_score_perc")),
                           num_ELI, grayed_out),
            by = "GEOID") %>% 
  st_as_sf()

full_data_natl_indices = indicator_data %>% 
  left_join(indexed_data_us %>% 
              select(contains("index"), GEOID |
                       starts_with("z_score_perc")),
            suffix = c("", "_national"),
            by = "GEOID") %>% 
  st_as_sf()

# Join indexes to non z scored (ie the indicator) data
indicator_data_us = indexed_data_us %>% 
  select(contains("index"), GEOID|
           (starts_with("z_score_perc"))) %>% 
  dplyr::rename_with(~paste0(.x, "_natl"), .cols = contains("index")) %>% 
  right_join(indicator_data, by = "GEOID") %>% 
    # move indices to the right
  select(-contains("index"), contains("index")) %>% 
  # Also append by state indices for comparison. May need to remove later
  left_join(indexed_data_by_state %>% 
              dplyr::select(GEOID, contains("index") |
                    (starts_with("z_score_perc"))) %>% 
              st_drop_geometry(),
            by = "GEOID",
            suffix = c("", "_state_adj")) %>%
  left_join(num_ELI, by = "GEOID") %>% 
  left_join(num_renters, by = "GEOID")  %>% 
  dplyr::relocate(ends_with("_natl"), .after = last_col()) %>% 
  dplyr::relocate(starts_with("perc_")) %>% 
  dplyr::relocate(GEOID, state_name, county_name, state_fips, county_fips,num_ELI, num_renters)
  

# Round indices to two decimal points
full_data_state_indices


###---Write out data-------------
# Create dir in case it doesn't exist
dir.create("data/intermediate-data/", recursive = T, showWarnings = F)

###---Write out data-------------
# Create dir in case it doesn't exist
dir.create("data/intermediate-data/", recursive = T, showWarnings = F)

## Write out full state adjusted data
# CSV for Data Catalog
full_data_state_indices %>% 
  st_drop_geometry() %>% 
  write_csv("data/intermediate-data/housing_index_state_adj.csv")

# Geojson for data catalog
full_data_state_indices %>% 
  st_write("data/intermediate-data/housing_index_state_adj.geojson", delete_dsn = TRUE)

# Geojson for Comms
full_data_state_indices %>% 
  select(GEOID:county_fips, contains("quantile"), contains("index"), 
         num_ELI, grayed_out) %>% 
  rename_with(~str_replace_all(.x, "z_score_", ""), starts_with("z_score_")) %>% 
  # Alice wanted GEOID to be numeric bc of Mapbox weirdness
  mutate(GEOID = as.numeric(GEOID)) %>% 
  st_write("data/intermediate-data/housing_index_state_adj_feature.geojson", delete_dsn = TRUE)
  
  

## Write out full national unadjusted data
# CSV for Data Catalog
full_data_natl_indices %>% 
  st_drop_geometry() %>% 
  write_csv("data/intermediate-data/housing_index_state_natl.csv")

# Geojson for data catalog
full_data_natl_indices %>% 
  st_write("data/intermediate-data/housing_index_natl.geojson")

  
  
  
### Old data writeouts, kept for correlation/eda scripts
# Write out non z scored indicators (and indexes)
write_csv(indicator_data_us %>% st_drop_geometry(), "data/intermediate-data/housing_data_indicators.csv")

# Write out z scored indicators (and indexes)
write_csv(indexed_data_us %>% st_drop_geometry(), "data/intermediate-data/housing_data_index.csv")
write_csv(indexed_data_by_state %>% st_drop_geometry(), "data/intermediate-data/housing_data_index_by_state.csv")

# Write out num renters csv for data investigation
num_renters %>% write_csv("data/intermediate-data/num_renters_per_tract.csv")


# Alice requested a numeric ID column that is a version of GEOID for Mapbox geojsons
indexed_data_us = indexed_data_us %>% 
  mutate(id = as.numeric(GEOID)) 
  
indexed_data_by_state = indexed_data_by_state %>% 
  mutate(id = as.numeric(GEOID)) 

## Write out geojsons
st_write(indexed_data_us, "data/intermediate-data/housing_data_index.geojson", delete_dsn = T)
st_write(indexed_data_by_state, "data/intermediate-data/housing_data_index_by_state.geojson", delete_dsn = T)



#Add: reproducability check
# local <- st_read("data/intermediate-data/housing_index_state_adj_feature.geojson")
# online <- st_read("https://ui-covid-housing-risk-indicators.s3.amazonaws.com/housing_index_state_adj_feature.geojson")
# 
# all.equal(local, online)

