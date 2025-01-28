# Create map of evacuation areas and fire boundaries as figure for final paper.
# Heather, April 4th

# Libraries and read ------------------------------------------------------
library(tidyverse)
library(sf)
library(here)
library(magrittr)
library(lubridate)

# constant - counties of the study area
kaiser_counties = c("Los Angeles",
                    "Ventura",
                    "Kern",
                    "Riverside",
                    "San Bernardino",
                    "San Diego",
                    "Orange")

# getty boundary data
gets <-
  read_sf(
    here(
      "writing",
      "figures",
      "map_figure_1",
      "ca_getty_20191029_0019_dd83",
      "ca_getty_20191029_0019_dd83.shp"
    )
  ) %>%
  st_transform(2227)

# woolsey boundary data
wools <-
  read_sf(
    here(
      "writing",
      "figures",
      "map_figure_1",
      "Woolsey_Fire_Perimeter_20181118",
      "FirePerimeter_20181118.shp"
    )
  ) %>%
  st_transform(2227)

# getty evacuation boundary
getty_evac <-
  read_sf(
    here(
      "data_processing",
      "data_proximity_analyses",
      "getty_evac_boundary",
      "getty_clean.shp"
    )
  ) %>%
  st_transform(2227) %>%
  st_buffer(dist = 10000)

# woolsey evacuation boundary
woolsey_evac <-
  read_sf(
    here(
      "data_processing",
      "data_proximity_analyses",
      "woolsey_evac_boundary",
      "woolsey_clean.shp"
    )
  ) %>%
  st_transform(2227) %>%
  st_buffer(dist = 10000)

# county shapefile
county <-
  read_sf(here(
    "writing",
    "figures",
    "map_figure_1",
    "CA_Counties",
    "CA_Counties_TIGER2016.shp"
  )) %>%
  mutate(County = case_when(NAME %in% kaiser_counties ~ 'Study area',
                            TRUE ~ 'Not in study area')) %>%
  mutate(County = ordered(County,
                          levels = c("Not in study area",
                                     "Study area"))) %>%
  st_transform(2227)


# Do ----------------------------------------------------------------------
cols  <- c(
  "Not in study area" = "#FFFFFF",
  "Study area" = '#778899'
)

map1 <- ggplot() + 
  geom_sf(data = county, alpha = 0.2, aes(fill = County)) + 
  scale_fill_manual(name = "County", values = cols) +
  geom_sf(data = getty_evac, fill = 'light blue', color = alpha('light blue', 0.2)) +
  geom_sf(data = woolsey_evac, fill = 'light green', color = alpha('light green', 0.2)) +
  geom_sf(data = wools, fill = 'black', color = 'black') +
  geom_sf(data = gets, fill = 'black', color = 'black') +
  theme_minimal(base_size = 8) + 
  coord_sf(crs = 4326, xlim = c(-121, -117), ylim = c(32, 36)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  ) + 
  theme(legend.title = element_blank(), legend.text = element_blank()) 


map <- ggplot() + 
  geom_sf(data = county, alpha = 0.2, aes(fill = County)) + 
  scale_fill_manual(name = "County", values = cols) +
  geom_sf(data = getty_evac, fill = 'light blue', color = alpha('light blue', 0.2)) +
  geom_sf(data = woolsey_evac, fill = 'light blue', color = alpha('light blue', 0.2)) +
  geom_sf(data = wools, fill = 'black', color = 'black') +
  geom_sf(data = gets, fill = 'black', color = 'black') +
  theme_minimal(base_size = 8) + 
  coord_sf(crs = 4326, xlim = c(-119.5, -118), ylim = c(33.5, 34.5)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  ) + 
  theme(legend.title = element_blank()) 


# Save --------------------------------------------------------------------

ggsave(filename = here(
  "writing",
  "figures",
  "map_figure_1",
  "base_map_larger.pdf"),
  plot = map1,
  device = 'pdf')

ggsave(filename = here(
  "writing",
  "figures",
  "map_figure_1",
  "base_map_zoom.pdf"),
  plot = map,
  device = 'pdf')


