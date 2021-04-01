
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

my_colors <- c(palette_urbn_cyan[3], "#ffffff", palette_urbn_red[3])

my_counties <- get_urbn_map("counties", sf=T)
my_states <- get_urbn_map("states", sf=T)

countymap_direction <- left_join(my_counties, countydirection, by = c("county_fips" = "county"))
final_levels_map1 <- c("","","declined 0 to 0.25","declined 0.25 to 0.50", "declined 0.50 to 0.75", "declined 0.75 to 1")

# housing map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(housing_avg_diff)), mapping = aes(fill=housing_avg_diff %>% cut(breaks = c(-1.5,0.0001,0,0.25,0.50,0.75,1), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c("#ffffff","#ffffff",palette_urbn_red[2],palette_urbn_red[3],palette_urbn_red[4],palette_urbn_red[5]), labels = final_levels_map1) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 0.5, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Housing Sub-Index",
       subtitle = "Counties with Worsening Conditions")

ggsave("housing_improv_decline_2.png")



final_levels_map2 <- c("Improvement (1+)","Improvement (0.5 to 1)","Improvement (0 to 0.5)","No Change","Worse (0 to 0.5)","Worse (0.5 to 1)","Worse (1+)")

# covid map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(covid_avg_diff)), mapping = aes(fill=covid_avg_diff %>% cut(breaks = c(-2,-1,-0.5,0,0.0001,0.5,1,5), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[7], palette_urbn_cyan[5], palette_urbn_cyan[3], "#ffffff",palette_urbn_red[3],palette_urbn_red[5],palette_urbn_red[7]), labels = final_levels_map2) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1.5, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "COVID Sub-Index",
       subtitle = "Counties with Improvements or Worsening Conditions")


ggsave("covid_improv_worse.png")


final_levels_map3 <- c("Improvement (0 to 0.5)","No Change","Worse (0 to 0.5)","Worse (0.5 to 1)")


# equity map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(equity_avg_diff)), mapping = aes(fill=equity_avg_diff %>% cut(breaks = c(-0.5,0,0.0001,0.5,1), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[3], "#ffffff", palette_urbn_red[3],palette_urbn_red[5]), labels = final_levels_map3) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1.5, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Equity Sub-Index",
       subtitle = "Counties with Improvements or Worsening Conditions")

ggsave("equity_improv_worse.png")


final_levels_map4 <- c("Improvement (0.5 to 1)","Improvement (0 to 0.5)","No Change","Worse (0 to 0.5)")


#total map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(total_avg_diff)), mapping = aes(fill=total_avg_diff %>% cut(breaks = c(-1,-0.5,0,0.0001,0.5), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c(palette_urbn_cyan[5], palette_urbn_cyan[3], "#ffffff", palette_urbn_red[3]), labels = final_levels_map4) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 1.5, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Total Index",
       subtitle = "Counties with Improvements or Worsening Conditions")

ggsave("total_improv_worse.png")

