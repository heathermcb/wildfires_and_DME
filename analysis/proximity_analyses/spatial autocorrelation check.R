# check spatial autocorrelation

library(tidyverse)
library(viridis)
library(hglm)
library(sf)
library(spdep)

an_dat_getty_prox2 <- an_dat_getty_prox %>% 
  filter(!is.na(patients) & !is.na(n) & !is.na(wkmntp) & !is.na(person_time_exp))
         
         
# read shapefile
zcta_shp <- read_sf("/Users/heathermcbrien/Documents/wildfires_and_DME/data_processing/raw_data/zcta_shapefile/tl_2018_us_zcta510.shp")
zcta_shp <- zcta_shp %>% 
  filter(ZCTA5CE10 %in% an_dat_getty_prox2$zcta) 
# run analyses here

# then add checks

an_dat_getty_prox2$residuals <- m1f$residuals
results <- an_dat_getty_prox2 %>% group_by(week_year, zcta)  %>%
  mutate(zcta = as.character(zcta))

zcta_shp <- zcta_shp %>% left_join(results, by = c("ZCTA5CE10" = 'zcta'))

zcta_shp <- zcta_shp %>% filter(!is.na(mean_res) & (ZCTA5CE10 %in% an_dat_getty_prox2$zcta))

s <- zcta_shp %>% ggplot() + geom_sf(aes(fill = mean_res), size = 0.1) +
  labs(fill = 'Mean residual value') + xlab("Map of mean residual values in negative biomial model
  of the relationship between residence <20 km from
the Getty Fire and all-cause oupatient visits, in ZCTA codes
in the study area in California") +
  theme_minimal()

#ggsave(filename = here("residuals_nov_22"), device = 'pdf')



#Formally test Moran's I
#Create neighbor matrix (basically saying which polygons are neighbors of other polygons)
nb <- poly2nb(zcta_shp, queen=FALSE)

#Assigning spatial weights based on neighbors
nb_lw <- nb2listw(nb, style="C", zero.policy = TRUE)

#Moran's I, if p-value < 0.05 there is evidene of spatial autocorrelation but you'll already know this from looking visually

moran.test(zcta_shp$mean_res, nb_lw, zero.policy = TRUE) #p-value = 1 indicating no support non-randomly distributed residuals



