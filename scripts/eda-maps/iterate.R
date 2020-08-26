library(rmarkdown)
library(stringr)
library(tidyverse)
library(tigris)
library(sf)
library(mapview)

all_counties = tigris::counties(cb = TRUE, year = 2018, class = "sf") %>% 
  mutate(GEOID = as.character(GEOID),
         GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0"))
# View all counties in the US in interactive map to get GEOID
# mapview(all_counties)

# create an index
county_geoid_list <- c("35015", "06085")

county_geoid_list <- sort(county_geoid_list, decreasing = FALSE)
fnames = all_counties %>% 
  filter(GEOID %in% county_geoid_list) %>% 
  arrange(desc(NAME)) %>% 
  pull(NAME)
  

# create a data frame with parameters and output file names
runs <- tibble(
  filename = str_c(fnames, ".html"),             # creates a string with output file names in the form <index>.pdf
  params = map(county_geoid_list, ~list(county_geoid = .)))  # creates a nest list of parameters for each object in the index

dir.create("scripts/eda-maps/maps/", showWarnings = FALSE)

# iterate render() along the tibble of parameters and file names, which outputs
# files into the factsheets folder
runs %>%
  select(output_file = filename, params) %>%
  pwalk(rmarkdown::render, input = "scripts/eda-maps/county_maps.rmd", output_dir = "scripts/eda-maps/maps/")
