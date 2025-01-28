# This script finds all ZCTAs exposed to fires (within 20 km of a fire
# boundary) during our study period in California, and writes a list of these
# ZCTAs along with the fire ignition date to the data_proximity_analyses
# folder.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)

# Read --------------------------------------------------------------------

# shapefile of all fire boundaries and ignition dates
load(
    here(
      "data_processing",
      "data_proximity_analyses",
      "calfire_disasters_2016_2020.rdata"
    )
  )

# shapefile of zctas
zctas <-
  read_sf(here(
    "data_processing",
    "raw_data",
    "zcta_shapefile",
    "tl_2018_us_zcta510.shp"
  )) %>%
  st_transform(2227)

# kaiser data
k <-
  read_csv(
    here(
      "data_processing",
      "raw_data",
      "DMEdatasets20200929172326",
      "dme_anydisease_A_09282020.csv"
    )
  )


# Find exposed controls ---------------------------------------------------

# find list of zctas in the kaiser area
k <- k %>%
  select(zcta) %>%
  filter(!is.na(zcta)) %>%
  distinct()

# filter zcta shapefile to CA Kaiser area
zctas <- zctas %>% filter(ZCTA5CE10 %in% k$zcta)

# reconcile coordinate systems
all_fires <- st_transform(calfire_kaiser_disaster, 2227)

# add buffer to fires to evaluate fire exposure - 20 km
all_fires_buffer <- st_buffer(x = all_fires, dist = 20000)

# add fire data to zctas that intersect with a fire exposure zone
exposed_controls <-
  st_join(zctas, all_fires_buffer, join = st_intersects) %>%
  st_drop_geometry() %>%
  mutate(date = as.Date(ALARM_DATE))

# remove zctas from the list that don't intersect to get a list of
# zctas that are exposed to fire and the relevant dates
exposed_controls <- exposed_controls %>%
  group_by(ZCTA5CE10) %>%
  summarise(earliest = min(date, na.rm = TRUE)) %>%
  mutate(earliest = case_when(is.infinite(earliest) ~ as.Date(NA),
                              TRUE ~ earliest)) %>%
  filter(!is.na(earliest)) %>% # to make joining easier later on
  mutate(zcta = as.factor(ZCTA5CE10)) %>%
  select(earliest, zcta)


# Write -------------------------------------------------------------------

write_csv(
  exposed_controls,
  here(
    "data_processing",
    "data_proximity_analyses",
    "exposed_controls.csv"
  )
)




