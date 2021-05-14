library(tidyverse)
library(sf)
library(urbnthemes)

urbnthemes::set_urbn_defaults()

housing_index_state_adj_feature = st_read("data/intermediate-data/housing_index_state_adj_feature.geojson") %>% 
  mutate(GEOID = as.character(GEOID))

housing_index_state_adj_feature_updated_2020 = st_read("/Users/anarayanan/Downloads/housing_index_state_adj_feature.geojson") %>% 
  mutate(GEOID = as.character(GEOID))

housing_index_state_adj = st_read("data/intermediate-data/housing_index_state_adj.geojson") %>% 
  mutate(GEOID = as.character(GEOID))



housing_index_state_adj_feature %>% 
  mutate(GEOID = as.character(GEOID), 
         GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")) %>% 
  filter(GEOID == "01073011600") %>% 
  select(GEOID, state_name, county_name, total_index_quantile, housing_index_quantile) %>% 
  st_drop_geometry()
housing_index_state_adj %>% 
  mutate(GEOID = as.character(GEOID), 
         GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")) %>% 
  filter(GEOID == "01073011600") %>% 
  select(GEOID, state_name, county_name, total_index_quantile, housing_index_quantile) %>% 
  st_drop_geometry()
housing_index_state_adj_feature_updated_2020 %>% 
  mutate(GEOID = as.character(GEOID), 
         GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")) %>% 
  filter(GEOID == "01073011600") %>% 
  select(GEOID, state_name, county_name, total_index_quantile, housing_index_quantile) %>% 
  st_drop_geometry()

old = housing_index_state_adj_feature_updated_2020 %>% select(GEOID, state_name, county_name, total_index_quantile)
new = housing_index_state_adj_feature %>% select(GEOID, state_name, county_name, total_index_quantile)


comparison = new %>% left_join(old %>% 
                                 select(GEOID, total_index_quantile_old = total_index_quantile) %>%
                                 st_drop_geometry()) %>% 
  mutate(diff = total_index_quantile - total_index_quantile) %>% 
  mutate(bin_old = case_when(
    (total_index_quantile_old >= 0  & total_index_quantile_old <= .50) ~ 1,
    (total_index_quantile_old > .50 & total_index_quantile_old <= .75) ~ 2,
    (total_index_quantile_old > .75 & total_index_quantile_old <= .85) ~ 3,
    (total_index_quantile_old > .85 & total_index_quantile_old <= .90) ~ 4,
    (total_index_quantile_old > .90 & total_index_quantile_old <= .95) ~ 5,
    (total_index_quantile_old > .95 & total_index_quantile_old <= 1) ~ 6,
  )) %>% 
  mutate(bin = case_when(
    (total_index_quantile >= 0  & total_index_quantile <= .50) ~ 1,
    (total_index_quantile > .50 & total_index_quantile <= .75) ~ 2,
    (total_index_quantile > .75 & total_index_quantile <= .85) ~ 3,
    (total_index_quantile > .85 & total_index_quantile <= .90) ~ 4,
    (total_index_quantile > .90 & total_index_quantile <= .95) ~ 5,
    (total_index_quantile > .95 & total_index_quantile <= 1) ~ 6,
  )) 

comparison = comparison %>% st_drop_geometry()

comparison %>% st_drop_geometry() %>%  as_tibble() %>% count(bin)
comparison_df = comparison %>% mutate(
  is_same = bin_old == bin,
  diff = bin - bin_old) %>% 
  st_drop_geometry() %>% 
  rowwise() %>% 
  mutate(col_value = case_when(
    !(is_same) ~ palette_urbn_cyan[4],
    TRUE ~ palette_urbn_gray[2]
  ))


comparison_df %>% 
  ggplot(aes(x = total_index_quantile_old, y = total_index_quantile, col = I(col_value))) +
  geom_point(alpha = 0.15, stroke = FALSE) +
  xlab("Total Index Quantile (Old)") +
  ylab("Total Index Quantile (New)")


comparison_df %>% filter(is.na(total_index_quantile_old))

