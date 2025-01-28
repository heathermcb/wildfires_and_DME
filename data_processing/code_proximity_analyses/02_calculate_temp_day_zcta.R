# This script will:

# - load raster file for particular year, month and day
# - load shapefile of ZCTAs
# - isolate each zcta's temperature data in raster via the shapefile and
#   then find area-weighted mean over the ZCTA
# - output mean temperature data by day and ZCTA for that year

# from Robbie M Parks 2020 GitHub; modified for our purposes


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(graticule)
library(zoo)

# Function ----------------------------------------------------------------

# this function calculates and writes temperature data for a given year
# using PRISM data and a ZCTA shapefile
calculate_and_write_temp_data <- function(year) {
  # constants
  dir.output <- here("data_processing", "data_proximity_analyses")
  dname <- "tmean"
  time.res <- "daily"
  space.res <- "zip"

  # ticker
  print("running grid_county_intersection_raster_prism.R")
  print(year)

  # load data for given year
  # load national ZCTA shapefile
  us.national <- readOGR(dsn = here(
    "data_processing",
    "raw_data",
    "zcta_shapefile",
    "tl_2019_us_zcta510.shp"
  ))
  # load California ZCTAs
  zips <-
    sort(unique(read_csv(
      here("data_processing", "raw_data", "zip_zcta_xwalk.csv")
    )$zcta))
  # filter national file to CA
  us.national <- us.national[us.national$ZCTA5CE10 %in% c(zips), ]
  # remove PO Box ZCTAs
  zips <- zips[zips %in% c(us.national$ZCTA5CE10)]
  us.main <- us.national
  # save projection of shapefile
  original.proj <- proj4string(us.national)

  # create summary dataframe
  # loop through each raster file for each day and then through
  # each ZCTA, summarizing over ZCTAs
  dates <-
    seq(as.Date(paste0("0101", year), format = "%d%m%Y"),
      as.Date(paste0("3112", year), format = "%d%m%Y"),
      by = 1
    )
  
  # init empty dataframe to load summarised daily values into
  weighted.area.national <- data.frame()
  # loop through each day of the year and perform analysis
  print(paste0("Processing dates in ", year))

  for (date in dates) {
    # get date components to load file
    print(format(as.Date(date, origin = "1970-01-01"), "%d/%m/%Y"))
    day <- format(as.Date(date, origin = "1970-01-01"), "%d")
    month <- format(as.Date(date, origin = "1970-01-01"), "%m")
    day.month <- paste0(month, day)
    print(day.month)
    # load raster for relevant date
    raster.full <- raster(
      paste0(
        here::here("data_processing", "raw_data"),
        "/PRISM",
        year,
        "/PRISM_",
        dname,
        "_stable_4kmD2_",
        year,
        day.month,
        "_bil.bil"
      )
    )
    raster.full <- projectRaster(raster.full, crs = original.proj)
    # create empty dataframe to fill with zip code summary information
    weighted.area <- data.frame()
    # loop through zip codes and save summarized information
    for (zip in zips) {
      # process zip preamble
      zip <- as.character(zip)
      # isolate zip to highlight
      us.zip <- us.main[us.main$ZCTA5CE10 %in% zip, ]
      # calculate mean temp over zip
      current.value <- raster::extract(
        x = raster.full,
        weights = TRUE,
        normalizeWeights = TRUE,
        y = us.zip,
        fun = mean,
        df = TRUE,
        na.rm = TRUE
      )
      to.add <- data.frame(zip, value = current.value[1, 2])
      weighted.area <- rbind(weighted.area, to.add)
    }
    names(weighted.area) <- c("zip", dname)
    # add date information to this particular zip's dataframe
    analysis.dummy <- weighted.area
    analysis.dummy$date <- format(as.Date(date), "%d/%m/%Y")
    analysis.dummy$day <- day
    analysis.dummy$month <- month
    analysis.dummy$year <- year
    # save in larger dataframe
    weighted.area.national <- rbind(weighted.area.national, analysis.dummy)
  }

  # write 
  saveRDS(
    weighted.area.national,
    paste0(
      dir.output,
      "/weighted_area_raster_zip",
      "_",
      dname,
      "_",
      time.res,
      "_",
      as.character(year),
      ".rds"
    )
  )
}

# Complete calculation and writing for all years ---------------------------

calculate_and_write_temp_data(2016)
calculate_and_write_temp_data(2017)
calculate_and_write_temp_data(2018)
calculate_and_write_temp_data(2019)

