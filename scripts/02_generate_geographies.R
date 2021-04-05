library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)

options(tigris_use_cache=FALSE)

state_2019 = tigris::states(year = 2019, class = "sf") %>% 
  janitor::clean_names() %>% 
  select(statefp, abbv = stusps, name)

us_counties_2019 = tigris::counties(year = 2019, class = "sf") %>%
  janitor::clean_names() %>% 
  select(statefp, countyfp, name, namelsad) %>% 
  left_join(state_2019 %>%
              select(statefp, state_abbv = abbv, state_name = name) %>% 
              st_drop_geometry()) %>% 
  mutate(unique_id = row_number()) %>% 
  mutate(full_name = paste0(namelsad, ", ", state_abbv)) %>% 
  select(unique_id, statefp, countyfp, namelsad, state_abbv, state_name)


us_counties_2019_cb = tigris::counties(year = 2019, class = "sf", cb = TRUE) %>%
  janitor::clean_names() %>% 
  select(statefp, countyfp) %>% 
  left_join(us_counties_2019 %>% 
              select(statefp, countyfp, namelsad, state_abbv, state_name) %>% 
              st_drop_geometry(),
            by = c("statefp", "countyfp")) %>% 
  mutate(unique_id = row_number()) %>% 
  select(unique_id, statefp, countyfp, namelsad, state_abbv, state_name)



# Download in zip file of COC geographies and unzip
download.file("https://www.hudexchange.info/resources/documents/CoC_GIS_National_Boundary_2019.zip",
              destfile = "data/raw-data/coc_geographies.zip")
dir.create("data/raw-data/coc-geographies", showWarnings = F)

unzip("coc_geographies.zip",
      exdir = "data/raw-data/coc-geographies"
      )

# coc_gdb = st_read("data/raw-data/coc-geographies/FY19_CoC_National_Bnd.gdb")


# Write out CoC and state data for Rob to do spatial intersection
# coc_gdb %>% st_write("data/raw-data/coc-geographies/cocs.shp)
# state_2018 %>% st_write("data/raw-data/coc-geographies/states.shp)


# Asked Rob Pitingolo to perform state to CoC spatial join. The join was taking
# forever in R (5+ hours) possibly due to malformed geometries. Not sure why this
# was happening, need to investigate

coc_state_ints = st_read("data/raw-data/coc-geographies/coc_state_join/coc_state_join.shp")

coc_state_ints = coc_state_ints %>% 
  mutate(area_int = st_area(.)) %>%
  # Filter out intersections less thatn 40k meters to filter out long thin state
  # boundaries that were matched due to slightly mismatched coc and state boundaries 
  filter(area_int > units::set_units(40000, m^2)) %>% 
  add_count(COCNAME) %>% 
  arrange(desc(n)) 


split_cocs = coc_state_ints %>% 
  # Pull out just the CoC's we are splitting into two across state borders
  filter(COCNUM %in% c("GA-505", "MO-604", "NE-501", "IA-500")) %>% 
  arrange(COCNUM, desc(area_int)) %>% 
  # Put the state name in the COCNAME so all COCNAMES are unique
  mutate(COCNAME = paste0(COCNAME, " (", name, " part)"))


cocs_in_single_state= coc_state_ints %>% 
  # Filter to counties located in two states, but not in the list of state we are splitting across state_boundaries
  filter(!COCNUM %in% c("GA-505", "MO-604", "NE-501", "IA-500")) %>% 
  arrange(COCNUM, desc(area_int)) %>% 
  group_by(COCNUM) %>% 
  # Manually confirmed that for the 6 other CoCs that are located in two states,
  # the second state polygon is just a long thin boundary polygon (ususally 
  # along rivers) where the boundaries don't exact match up. So we get the
  # largest CoCs
  slice(1)
  

all_cocs_modified = rbind(split_cocs, cocs_in_single_state) %>% 
  select(coc_num = COCNUM,
         coc_name = COCNAME, 
         state_abbv = abbv, 
         state_name = name) %>% 
  # Lee's Summit/Jackson county shows up as Lee?s Summit/Jackson, probably as a 
  # result of transferring to shp file for Rob's spatial join and casting backs
  mutate(coc_name = str_replace_all(coc_name, "\\?", "'")) %>% 
  mutate(unique_id = row_number())

# Write out as geojson
all_cocs_modified %>% st_write("data/intermediate-data/coc_geographies_states_split.geojson", delete_dsn = TRUE)

us_counties_2019 %>% st_write("data/intermediate-data/counties.geojson", delete_dsn = TRUE)

us_counties_2019_cb %>% st_write("data/intermediate-data/counties_cb.geojson", delete_dsn = TRUE)

