
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
final_levels_map1 <- c("Improvement or No Change","Decline (0 to 0.16)","Decline (0.16 to 0.33)","Decline (0.33 to 0.50)", "Decline (0.50 to 0.66)", "Decline (0.66 to 0.83)", "Decline (0.83 to 1)")

# housing map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(housing_avg_diff)), mapping = aes(fill=housing_avg_diff %>% cut(breaks = c(-1.5,0.0001,0,0.16,0.33,0.50,0.66,0.83,1), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c("#ffffff",palette_urbn_red[3],palette_urbn_red[4],palette_urbn_red[5],palette_urbn_red[6],palette_urbn_red[7],palette_urbn_red[8]), labels = final_levels_map1) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 0.33, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Housing Sub-Index",
       subtitle = "Counties with Worsening Conditions")

ggsave("housing_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("housing_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)


final_levels_map2 <- c("Improvement or No Change","Decline (0 to 0.16)","Decline (0.16 to 0.33)","Decline (0.33 to 0.50)", "Decline (0.50 to 0.66)", "Decline (0.66 to 0.83)", "Decline (0.83 to 1)", "Decline (+1)")

# covid map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(covid_avg_diff)), mapping = aes(fill=covid_avg_diff %>% cut(breaks = c(-2,0.0001,0.16,0.33,0.50,0.66,0.83,1,5), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c("#ffffff",palette_urbn_red[2],palette_urbn_red[3],palette_urbn_red[4],palette_urbn_red[5],palette_urbn_red[6], palette_urbn_red[7], palette_urbn_red[8]), labels = final_levels_map2) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 0.33, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "COVID Sub-Index",
       subtitle = "Counties with Worsening Conditions")

ggsave("covid_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("covid_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)



final_levels_map3 <- c("Improvement or No Change","Decline (0 to 0.16)","Decline (0.16 to 0.33)","Decline (0.33 to 0.50)", "Decline (0.50 to 0.66)", "Decline (0.66 to 0.83)", "Decline (0.83 to 1)")

# equity map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(equity_avg_diff)), mapping = aes(fill=equity_avg_diff %>% cut(breaks = c(-1.5,0.0001,0.16,0.33,0.50,0.66,0.83,1), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c("#ffffff",palette_urbn_red[3],palette_urbn_red[4],palette_urbn_red[5],palette_urbn_red[6],palette_urbn_red[7],palette_urbn_red[8]), labels = final_levels_map3) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 0.33, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Equity Sub-Index",
       subtitle = "Counties with Worsening Conditions")

ggsave("equity_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("equity_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)



final_levels_map4 <- c("Improvement or No Change","Decline (0 to 0.16)","Decline (0.16 to 0.33)","Decline (0.33 to 0.50)", "Decline (0.50 to 0.66)", "Decline (0.66 to 0.83)", "Decline (0.83 to 1)")


#total map
ggplot() + 
  geom_sf(my_counties, mapping = aes(), fill = "#dfdfdf", color = "#ffffff", size = 1) + 
  coord_sf(datum = NA) + 
  geom_sf(countymap_direction %>% filter(!is.na(total_avg_diff)), mapping = aes(fill=total_avg_diff %>% cut(breaks = c(-1.5,0.0001,0.16,0.33,0.50,0.66,0.83,1), include.lowest = T)), color = "#696969", size = .01) + 
  coord_sf(datum = NA) + 
  scale_fill_manual(values = c("#ffffff",palette_urbn_red[3],palette_urbn_red[4],palette_urbn_red[5],palette_urbn_red[6],palette_urbn_red[7],palette_urbn_red[8]), labels = final_levels_map4) + 
  geom_sf(my_states, mapping = aes(), fill = NA, size = 0.33, color = "#ffffff") + 
  theme(plot.margin = margin(t=0, b=0, l=0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "Total Index",
       subtitle = "Counties with Worsening Conditions")

ggsave("total_decline_test.png", width = 8, height = 6, dpi = 1000)
ggsave("total_decline_test.pdf", device = cairo_pdf, width = 8, height = 6, dpi = 1000)

