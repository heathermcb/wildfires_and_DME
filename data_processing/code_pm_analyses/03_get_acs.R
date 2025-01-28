#### Get ACS data.

#### This script will use tidycensus to get ACS data from the 2015-2019 ACS on
#### the following covariates:
#### - population < 5 years old, 5-18 years, 18-64
#### - population density
#### - % male
#### - home ownership

#### Other variables that will be included in the analysis are already in the
#### data provided by Kaiser.

# Load packages ----
library(tidycensus)
library(tidyverse)
library(here)

# Get ACS variables ----
covars <- get_acs(
  geography = "zcta",
  year = 2019,
  survey = "acs5",
  variables = c(
    total_male_pop = "B01001_002",
    total_pop = "B01003_001",
    homeowner_occupied = "B07013_002"
  )
)

# get second set of variables
covars2 <- get_acs(
  geography = "zcta",
  year = 2019,
  survey = "acs5",
  variables = c(
    under_5 = "S0101_C02_002E",
    p_5_9 = "S0101_C02_003E",
    p_10_14 = "S0101_C02_004E",
    p_15_19 = "S0101_C02_005E",
    p_20_24 = "S0101_C02_006E",
    p_25_29 = "S0101_C02_007E",
    p_30_34 = "S0101_C02_008E",
    p_35_39 = "S0101_C02_009E",
    p_40_44 = "S0101_C02_010E",
    p_45_49 = "S0101_C02_011E",
    p_50_54 = "S0101_C02_012E",
    p_55_59 = "S0101_C02_013E",
    p_60_64 = "S0101_C02_014E",
    p_65_69 = "S0101_C02_015E",
    p_70_74 = "S0101_C02_016E",
    p_75_79 = "S0101_C02_017E",
    p_80_85 = "S0101_C02_018E",
    p_85_older = "S0101_C02_019E"
  )
)

# Clean vars ----
# change first set of vars to tidy format
covars1 <- covars %>%
  dplyr::select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# second set tidy format
covars3 <- covars2 %>%
  mutate(GEOID = substr(NAME, 7, 11)) %>%
  dplyr::select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# fix column names
colnames(covars3) <- c(
  "GEOID",
  "under_5",
  "p_5_9",
  "p_10_14",
  "p_15_19",
  "p_20_24",
  "p_25_29",
  "p_30_34",
  "p_35_39",
  "p_40_44",
  "p_45_49",
  "p_50_54",
  "p_55_59",
  "p_60_64",
  "p_65_69",
  "p_70_74",
  "p_75_79",
  "p_80_85",
  "p_85_older"
)

# create one data frame
covars_all <- left_join(covars3, covars1)

# filter down to only relevant ZCTAs
zctas <-
  read_csv(here("data_processing", "data_pm_analyses", "zcta.csv"))
covars_all <- covars_all %>% filter(GEOID %in% zctas$zcta)

# add cols together to get data we want
covars_all <- covars_all %>%
  mutate(
    p_5_19 = p_5_9 +
      p_10_14 +
      p_15_19,
    p_20_64 = p_20_24 +
      p_25_29 +
      p_30_34 +
      p_35_39 +
      p_40_44 +
      p_45_49 +
      p_50_54 +
      p_55_59 +
      p_60_64,
    p_65_more = p_65_69 +
      p_70_74 +
      p_75_79 +
      p_80_85 +
      p_85_older
  ) %>%
  select(
    GEOID,
    under_5,
    p_5_19,
    p_20_64,
    p_50_54,
    p_65_more,
    total_male_pop,
    total_pop,
    homeowner_occupied
  )


# Write ----
write_csv(covars_all,
          here("data_processing", "data_pm_analyses", "acs_covars.csv"))
