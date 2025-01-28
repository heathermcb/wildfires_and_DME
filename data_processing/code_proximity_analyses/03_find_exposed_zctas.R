# This script will take wildfire evacuation boundary data, Kaiser data on
# wildfire boundary proximity, and the census California ZCTA shapefile,
# and mark certain ZCTAs as exposed to wildfire if they are within 10km
# of an evacuation zone or within 20km of a wildfire boundary.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)

# Read --------------------------------------------------------------------

# ZCTAs
zctas <-
  read_sf(here(
    "data_processing",
    "raw_data",
    "zcta_shapefile",
    "tl_2018_us_zcta510.shp"
  )) %>%
  st_transform(2227)

# wf evacuation boundaries
evac_woolsey <-
  read_sf(
    here(
      "data_processing",
      "data_proximity_analyses",
      "woolsey_evac_boundary",
      "woolsey_clean.shp"
    )
  ) %>%
  st_transform(crs = st_crs(zctas))

# getty evacuation boundaries
evac_getty <-
  read_sf(
    here(
      "data_processing",
      "data_proximity_analyses",
      "getty_evac_boundary",
      "getty_clean.shp"
    )
  ) %>%
  st_transform(crs = st_crs(zctas))

# Kaiser
k <-
  read_csv(
    here(
      "data_processing",
      "raw_data",
      "DMEdatasets20200929172326",
      "dme_anydisease_A_09282020.csv"
    )
  )
k <- k %>%
  select(zcta, getty_disaster_20km, woolsey_disaster_20km) %>%
  filter(!is.na(zcta)) %>%
  distinct()
# filter zcta file to CA Kaiser area
zctas <- zctas %>% filter(ZCTA5CE10 %in% k$zcta)


# Do ----------------------------------------------------------------------

# pad evacuation zones and do a spatial intersection to identify evacuated
# or partially evacuated ZCTAs

# buffer
evac_getty_b <- st_buffer(x = evac_getty, dist = 10000)
evac_woolsey_b <-
  st_buffer(x = evac_woolsey, dist = 10000) %>% st_union()
# intersect
exposed_evac_getty <- st_intersection(zctas, evac_getty_b)
exposed_evac_woolsey <- st_intersection(zctas, evac_woolsey_b)
# save
zctas_exposed_getty <- exposed_evac_getty$ZCTA5CE10
zctas_exposed_woolsey <- exposed_evac_woolsey$ZCTA5CE10

# make final exposure measurement
k <-
  k %>% mutate(
    getty_evac = case_when(zcta %in% zctas_exposed_getty ~ 1,
                           TRUE ~ 0),
    woolsey_evac = case_when(zcta %in% zctas_exposed_woolsey ~ 1,
                             TRUE ~ 0)
  )

# Write -------------------------------------------------------------------

write_csv(k,
          here(
            "data_processing",
            "data_proximity_analyses",
            "proximity_exposure.csv"
          ))
