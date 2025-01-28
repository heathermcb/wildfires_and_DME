#### This script creates mean weekly PM measurements by ZCTA grouping.
#### It takes in the grouped PM dataset, and outputs mean weekly PM measurements
#### so that we can analyze lags up to three weeks later in the analyses.

# Load libraries ----
library(tidyverse)
library(sf)
library(lubridate)
library(here)

# Read ----
an_dat <-
  read_csv(here("data_processing", "data_pm_analyses", "an_dat_daily.csv"))

# Summarize ----
an_dat_weekly <- an_dat %>% 
  mutate(week_year = paste0(year(an_dat$date), "-", week(an_dat$date))) %>% 
  select(-c('date')) %>%
  group_by(week_year, grouping) %>%
  summarise_all(
    .funs = list(
      ~ mean(., na.rm = TRUE),
      ~ sum(., na.rm = TRUE)
    )) %>%
  select(week_year, 
         grouping,
          visitsA = visitsA_sum,
          visitsI = visitsI_sum,
          visitsIC = visitsIC_sum,
          visitsR = visitsR_sum,
          visitsRC = visitsRC_sum,
          pov_p = pov_p_mean,
          edu_lt_hs_p = edu_lt_hs_p_mean,
          med_inc = med_inc_mean,
          tot_pop = tot_pop_mean,
          black_p = black_p_mean,
          hispan_p = hispan_p_mean,
          white_p = white_p_mean,
          under_5 = under_5_mean,
          p_5_19 = p_5_19_mean,
          p_20_64 = p_20_64_mean,
          p_65_more = p_65_more_mean,
          total_male_pop = total_male_pop_mean,
          homeowner_occupied = homeowner_occupied_mean,
          tempmean = temp_lag0_mean,
          nonwf_pm25 = non_wf_pm_lag0_mean,
          wf_pm_week_1 = wf_pm_Lag0_mean,
          wf_pm_week_2 = wf_pm_Lag7_mean,
          wf_pm_week_3 = wf_pm_Lag14_mean,
          patients = patients_sum
  ) 

# add week year sequence to use as a temporal covariate
week_year <- sort(unique(an_dat_weekly$week_year))
week_year_seq <- seq_along(week_year)
s <- data.frame(week_year = week_year, week_year_seq = week_year_seq)
an_dat_weekly <- an_dat_weekly %>% left_join(s)


# Write ----
write_csv(an_dat_weekly,
          here("data_processing", "data_pm_analyses", "an_dat_weekly.csv"))
