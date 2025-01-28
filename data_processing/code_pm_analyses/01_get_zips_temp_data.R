#### Get a list of the ZCTAs in the Kaiser study area.

#### This script uses a file from Kaiser, in the raw_data folder, to find
#### a list of ZCTAs in the study area. It then writes a csv containing
#### that list of ZCTAs to the data_pm_analyses folder.

# Libraries ----
library(here)
library(tidyverse)

# Data ----
# load Kaiser data -
# all zips are the same for all files therefore only need zips for one
k <-
  read_csv(here(
    "data_processing",
    "raw_data",
    "DMEdatasets20200929172326",
    "dme_anydisease_A_09282020.csv"
  ))

# Write ----
# get unique zctas and write
zctas <-
  as.data.frame(sort(unique(k$zcta))) %>%
  rename(zcta = "sort(unique(k$zcta))")
write_csv(zctas, here("data_processing", "data_pm_analyses", "zcta.csv"))
