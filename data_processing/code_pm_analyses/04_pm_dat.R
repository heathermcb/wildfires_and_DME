##### Create comprehensive dataset for PM 2.5 models.

####  This script is designed to merge all previous data into a full
####  dataset enabling a negative binomial analysis with 'wildfire PM'
####  as the exposure. It will:
#### - merge the 5 Kaiser outcome files into one dataset containing all
####   outcomes (from raw_data)
#### - add temperature data for all zips to outcome data
####   (from data_pm_analyses)
#### - add PM exposure information from corrected PM files (INFO IN KAISER IS
####   WRONG.)
#### - create an offset variable for population exposed in each ZCTA grouping
####   using population counts of each ZCTA from Kaiser
#### - add ACS covariates by ZCTA.
#### Then, it will save the dataset to data_pm_analyses.

# Libraries ----
library(tidyverse)
library(here)

# Read and set up all data sources ----
# Kaiser data
fls <-
  list.files(path = here("data_processing",
                         "raw_data",
                         "DMEdatasets20200929172326"))
setwd(here("data_processing", "raw_data", "DMEdatasets20200929172326"))
dt <- lapply(fls, read.csv)
# we know that each file is identical save the patient visits column
# therefore, just add subsequent columns to the first dataframe
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

# read in temperature data
tps <- list.files(path = here("data_processing", "data_pm_analyses"), pattern = "*.rds")
setwd(here("data_processing", "data_pm_analyses"))
tp <- lapply(tps, readRDS)
# create temp data frame
tp <- do.call("rbind", tp) %>%
  rename(zcta = zip) %>%
  select(date, zcta, tmean) %>%
  mutate(date = as.Date(as.character(date), format = "%d/%m/%Y")) # date class

# zip zcta xwalk for PM data
zipzcta <-
  read_csv(here("data_processing", "raw_data", "zip_zcta_xwalk.csv")) %>%
  select(zip_code, zcta)

# read PM 2.5 data
wf <-
  read_csv(here(
    "data_processing",
    "raw_data",
    "wf_imp_IDW_intersect_SoCal_20Apr2021.csv"
  )) %>%
  group_by(county, date, zip) %>%
  summarise(
    wf_pm25_idw_intrsct = mean(wf_pm25_idw_intrsct, na.rm = TRUE),
    wf_pm25_imp_intrsct = mean(wf_pm25_imp_intrsct, na.rm = TRUE),
    mean_pm25 = mean(mean_pm25, na.rm = TRUE)
  )
# collapse to zcta
wf <- wf %>%
  left_join(zipzcta, by = c("zip" = "zip_code")) %>%
  group_by(date, zcta) %>%
  summarise(
    mean_pm25 = mean(mean_pm25, na.rm = TRUE),
    wf_pm25_idw_intrsct = mean(wf_pm25_idw_intrsct, na.rm = TRUE),
    wf_pm25_imp_intrsct = mean(wf_pm25_imp_intrsct, na.rm = TRUE),
  ) %>%
  mutate(zcta = as.factor(zcta))

# Kaiser population counts by ZCTA
np <- read_csv(here("data_processing", "raw_data", "dme_counts_by_zip.csv"))
colnames(np) <- c("zip_code", "patients")
np <- np %>%
  left_join(zipzcta) %>%
  filter(zcta %in% unique(dat$zcta)) %>%
  select(zcta, patients) %>%
  mutate(zcta = as.factor(zcta))

# ACS covariates
acs <-
  read_csv(here("data_processing", "data_pm_analyses", "acs_covars.csv")) %>%
  mutate(zcta = as.factor(GEOID)) %>%
  select(-c(GEOID))

# Add all data sources ----
# join Kaiser data with ACS covariates and filter relevant cols
dat <- dat %>%
  mutate(zcta = as.factor(zcta)) %>%
  left_join(acs) %>%
  select(
    date = admitdate,
    zipid,
    zcta,
    visitsA,
    visitsI,
    visitsIC,
    visitsR,
    visitsRC,
    pov_p,
    edu_lt_hs_p,
    med_inc,
    tot_pop,
    black_p,
    hispan_p,
    white_p,
    under_5,
    p_5_19,
    p_20_64,
    p_65_more,
    total_male_pop,
    homeowner_occupied
  )

# join Kaiser outcomes with temp data
dat <- dat %>%
  left_join(tp, by = c("zcta", "date"))

# join Kaiser with PM data
dat <- dat %>%
  left_join(wf, by = c("date", "zcta"))

# add offset
dat <- dat %>% left_join(np)

# Write ----
write_csv(
  dat,
  here::here(
    "data_processing",
    "data_pm_analyses",
    "daily_with_acs_pm_temp.csv"
  )
)
