# Create analytic dataset for proximity to wildfires models.

# This script is designed to merge all previous data into four analytic
# datasets enabling a negative binomial analysis with 'wildfire proximity
# and evacuation as the exposures. It will:
# - merge the 5 Kaiser outcome files into one dataset containing all
#   outcomes (from raw_data)
# - add temperature data for all zips to outcome data
#   (from data_proximity_analyses)
# - create and add a variable called weekyears to help with time series
# - add wildfire exposure information, both for exposed regions and
#   for removing exposed regions from controls, from data_proximity_analyses
# - create an offset variable for population exposed in each week using
#   population counts of each ZCTA from Kaiser
# Then, it will save the analytic dataset to data_proximity_analyses.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

# Read --------------------------------------------------------------------

# Kaiser data
fls <-
  list.files(path = here::here("data_processing",
                               "raw_data",
                               "DMEdatasets20200929172326"))
setwd(here::here("data_processing",
                 "raw_data",
                 "DMEdatasets20200929172326"))
dt <- lapply(fls, read.csv)
# combine files - we know that each file is identical save the patient
# visits column therefore, just add subsequent columns to the first dataframe
dat <-
  cbind(
    dt[[1]],
    dt[[2]]$daily_byzip_ct_I_anydisease,
    dt[[3]]$daily_byzip_ct_R_anydisease,
    dt[[4]]$daily_byzip_ct_I_circulatory,
    dt[[5]]$daily_byzip_ct_R_circulatory
  ) %>% rename(
    visitsA = daily_byzip_ct_A_anydisease,
    visitsI = `dt[[2]]$daily_byzip_ct_I_anydisease`,
    visitsR = `dt[[3]]$daily_byzip_ct_R_anydisease`,
    visitsIC = `dt[[4]]$daily_byzip_ct_I_circulatory`,
    visitsRC = `dt[[5]]$daily_byzip_ct_R_circulatory`
  ) %>% # remove empty rows (outside study area)
  filter((is.na(zcta) == FALSE) &
           (is.na(getty) == FALSE)) %>% # dates to date class
  mutate(
    date = as.Date(date, format = "%d%b%Y"),
    admitdate = as.Date(date, format = "%d%b%Y")
  )

# temperature data
tps <-
  list.files(
    path = here("data_processing", "data_proximity_analyses"),
    pattern = "*.rds"
  )
setwd(here("data_processing", "data_proximity_analyses"))
tp <- lapply(tps, readRDS)
# create temp data frame
tp <- do.call("rbind", tp) %>%
  rename(zcta = zip) %>%
  select(date, zcta, tmean) %>%
  mutate(date = as.Date(as.character(date), format = "%d/%m/%Y")) # date class

# wildfire exposure
exposure <-
  read_csv(here(
    "data_processing",
    "data_proximity_analyses",
    "proximity_exposure.csv"
  )) %>%
  mutate(zcta = as.factor(zcta))

# control exposure
exposed_controls <-
  read_csv(here(
    "data_processing",
    "data_proximity_analyses",
    "exposed_controls.csv"
  ))

# zip zcta xwalk for pop counts
zipzcta <-
  read_csv(here("data_processing", "raw_data", "zip_zcta_xwalk.csv")) %>%
  select(zip_code, zcta)

# ZCTA population counts
np <-
  read_csv(here("data_processing", "raw_data", "dme_counts_by_zip.csv"))
colnames(np) <- c("zip_code", "patients")
np <- np %>%
  left_join(zipzcta) %>%
  filter(zcta %in% unique(dat$zcta)) %>%
  select(zcta, patients) %>%
  mutate(zcta = as.factor(zcta))

# Create an data ----------------------------------------------------------

# add temperature
dat <- dat %>%
  mutate(zcta = as.factor(zcta)) %>%
  left_join(tp, by = c("zcta", "date"))

# add indicator for any fire exposure to use to filter out potential exposed
# controls
exposed_controls <- exposed_controls %>% 
  mutate(zcta = as.factor(zcta))
dat <- dat %>%
  left_join(exposed_controls) %>%
  mutate(valid_control = case_when(is.na(earliest) |
                                     (!is.na(earliest) &
                                        (admitdate < earliest)) ~ 1,
                                   TRUE ~ 0)) %>%
  select(-c(earliest))

# add exposure for analyses
dat <- dat %>%
  left_join(exposure)


# Aggregate to weekly level for neg bin -----------------------------------

# aggregate both visits (by type of visit) and temperature
dat <- dat %>%
  mutate(year = year(date), week = week(date)) %>%
  mutate(week = formatC(
    as.numeric(week),
    width = 2,
    format = "d",
    flag = "0"
  )) %>%
  mutate(
    week_year = paste(year, week, sep = "")
  ) %>%
  group_by(week_year, zipid, zcta) %>%
  summarise(
    visitsA = sum(visitsA),
    visitsI = sum(visitsI),
    visitsR = sum(visitsR),
    visitsIC = sum(visitsIC),
    visitsRC = sum(visitsRC),
    wkmntp = mean(tmean),
    n = n(),
    getty_disaster_20km = max(getty_disaster_20km),
    woolsey_disaster_20km = max(woolsey_disaster_20km),
    getty = max(getty),
    woolsey = max(woolsey),
    woolsey_evac = max(woolsey_evac),
    getty_evac = max(getty_evac),
    valid_control = min(valid_control)
  )

# create population offsets
dat <- dat %>%
  left_join(np) %>%
  mutate(person_time_exp = n * patients)

# add numbering for modelling
# weekyears
s <- sort(unique(dat$week_year))
wkyrseq <- seq(1, length(s), 1)
s <- data.frame(s, wkyrseq) %>% rename(week_year = s)
dat <- dat %>% left_join(s)



# Write -------------------------------------------------------------------

write_csv(dat,
          here::here("data_processing",
                     "data_proximity_analyses",
                     "an_dat.csv"))
