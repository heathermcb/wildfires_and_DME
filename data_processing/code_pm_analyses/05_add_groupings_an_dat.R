#### This script adds groupings to the ZCTAs in the analytic dataset
#### so that we have some geographic units which are a bit bigger than the zctas
#### themselves, and summarize existing variables over those groupings. It
#### saves the resulting analytic dataset in the data_pm_analyses folder.

# Load libraries ----
library(tidyverse)
library(sf)
library(lubridate)
library(here)

# Load data ----
an_dat <-
  read_csv(here(
    "data_processing",
    "data_pm_analyses",
    "daily_with_acs_pm_temp.csv"
  ))
s <-
  read_sf(here(
    "data_processing",
    "raw_data",
    "zcta_shapefile",
    "tl_2018_us_zcta510.shp"
  ))

# Functions ----
# add groupings: add groupings to a list of ZCTAs by grouping two together
# if they are numerically 1 digit apart
add_grouping <- function(dataframez) {
  grouping <- rep(0, length(dataframez$ZCTA5CE10))
  group <- 1
  grouping[[1]] <- 1
  for (i in 1:(length(dataframez$ZCTA5CE10) - 1)) {
    if ((dataframez$ZCTA5CE10)[[i + 1]] == (dataframez$ZCTA5CE10[[i]] + 1)) {
      grouping[[i + 1]] <- group
    } else {
      group <- group + 1
      grouping[[i + 1]] <- group
    }
  }
  dataframez$grouping <- as.factor(grouping)
  return(dataframez)
}

# add lags: add lags to a Kaiser dataframe
add_lags <- function(a_df) {
  a_df <- a_df %>% arrange(date)
  a_df <- a_df %>%
    mutate(
      wf_pm_Lag0 = wf_pm25,
      wf_pm_Lag1 = lag(wf_pm25, 1),
      wf_pm_Lag2 = lag(wf_pm25, 2),
      wf_pm_Lag3 = lag(wf_pm25, 3),
      wf_pm_Lag4 = lag(wf_pm25, 4),
      wf_pm_Lag5 = lag(wf_pm25, 5),
      wf_pm_Lag6 = lag(wf_pm25, 6),
      wf_pm_Lag7 = lag(wf_pm25, 7),
      wf_pm_Lag8 = lag(wf_pm25, 8),
      wf_pm_Lag9 = lag(wf_pm25, 9),
      wf_pm_Lag10 = lag(wf_pm25, 10),
      wf_pm_Lag11 = lag(wf_pm25, 11),
      wf_pm_Lag12 = lag(wf_pm25, 12),
      wf_pm_Lag13 = lag(wf_pm25, 13),
      wf_pm_Lag14 = lag(wf_pm25, 14),
      wf_pm_Lag15 = lag(wf_pm25, 15),
      wf_pm_Lag16 = lag(wf_pm25, 16),
      wf_pm_Lag17 = lag(wf_pm25, 17),
      wf_pm_Lag18 = lag(wf_pm25, 18),
      wf_pm_Lag19 = lag(wf_pm25, 19),
      wf_pm_Lag20 = lag(wf_pm25, 20),
      wf_pm_Lag21 = lag(wf_pm25, 21),
      wf_pm_Lag22 = lag(wf_pm25, 22),
      wf_pm_Lag23 = lag(wf_pm25, 23),
      wf_pm_Lag24 = lag(wf_pm25, 24)
    )

  a_df <- a_df %>%
    mutate(
      temp_lag0 = tmean,
      temp_lag1 = lag(tmean, 1),
      temp_lag2 = lag(tmean, 2),
      temp_lag3 = lag(tmean, 3),
      temp_lag4 = lag(tmean, 4),
      temp_lag5 = lag(tmean, 5),
      temp_lag6 = lag(tmean, 6),
      temp_lag7 = lag(tmean, 7),
      temp_lag8 = lag(tmean, 8),
      temp_lag9 = lag(tmean, 9),
      temp_lag10 = lag(tmean, 10),
      temp_lag11 = lag(tmean, 11),
      temp_lag12 = lag(tmean, 12),
      temp_lag13 = lag(tmean, 13),
      temp_lag14 = lag(tmean, 14),
      temp_lag15 = lag(tmean, 15),
      temp_lag16 = lag(tmean, 16),
      temp_lag17 = lag(tmean, 17),
      temp_lag18 = lag(tmean, 18),
      temp_lag19 = lag(tmean, 19),
      temp_lag20 = lag(tmean, 20),
      temp_lag21 = lag(tmean, 21),
      temp_lag22 = lag(tmean, 22),
      temp_lag23 = lag(tmean, 23),
      temp_lag24 = lag(tmean, 24)
    )

  a_df <- a_df %>% mutate(
    non_wf_pm_lag0 = mean_pm25,
    non_wf_pm_lag1 = lag(mean_pm25, 1),
    non_wf_pm_lag2 = lag(mean_pm25, 2),
    non_wf_pm_lag3 = lag(mean_pm25, 3),
    non_wf_pm_lag4 = lag(mean_pm25, 4),
    non_wf_pm_lag5 = lag(mean_pm25, 5),
    non_wf_pm_lag6 = lag(mean_pm25, 6),
    non_wf_pm_lag7 = lag(mean_pm25, 7),
    non_wf_pm_lag8 = lag(mean_pm25, 8),
    non_wf_pm_lag9 = lag(mean_pm25, 9),
    non_wf_pm_lag10 = lag(mean_pm25, 10),
    non_wf_pm_lag11 = lag(mean_pm25, 11),
    non_wf_pm_lag12 = lag(mean_pm25, 12),
    non_wf_pm_lag13 = lag(mean_pm25, 13),
    non_wf_pm_lag14 = lag(mean_pm25, 14),
    non_wf_pm_lag15 = lag(mean_pm25, 15),
    non_wf_pm_lag16 = lag(mean_pm25, 16),
    non_wf_pm_lag17 = lag(mean_pm25, 17),
    non_wf_pm_lag18 = lag(mean_pm25, 18),
    non_wf_pm_lag19 = lag(mean_pm25, 19),
    non_wf_pm_lag20 = lag(mean_pm25, 20),
    non_wf_pm_lag21 = lag(mean_pm25, 21),
    non_wf_pm_lag22 = lag(mean_pm25, 22),
    non_wf_pm_lag23 = lag(mean_pm25, 23),
    non_wf_pm_lag24 = lag(mean_pm25, 24)
    
  )
  # get mean exposures over lag period and mean pm:
  a_df$wk_mean_non_wf_pm <- rowMeans(a_df[, 27:33], na.rm = TRUE)
  a_df$wk_mean_temp <- rowMeans(a_df[, 75:81], na.rm = TRUE)
  return(a_df)
}

# Create groupings ----
# filter zctas to California study area only
zctas <- unique(an_dat$zcta)
s <- s %>% filter(ZCTA5CE10 %in% zctas)

# arrange in numerical order
s <- s %>% arrange(ZCTA5CE10)
s$ZCTA5CE10 <- as.numeric(s$ZCTA5CE10)

# create groupings
t <- add_grouping(s) %>%
  st_drop_geometry() %>%
  mutate(zcta = as.factor(ZCTA5CE10)) %>%
  select(zcta, grouping)

# Summarize data over groupings ----
an_dat1 <- an_dat %>%
  mutate(zcta = as.factor(zcta))
an_dat1 <- an_dat1 %>%
  left_join(t) 

an_dat1 <- an_dat1 %>%
  group_by(date, grouping) %>%
  summarise_at(
    .vars = vars(
      "visitsA",
      "visitsI",
      "visitsR",
      "visitsIC",
      "visitsRC",
      "tmean",
      "pov_p",
      "edu_lt_hs_p",
      "med_inc",
      "tot_pop",
      "black_p",
      "hispan_p",
      "white_p",
      "under_5",
      "p_5_19",
      "p_20_64",
      "p_65_more",
      "total_male_pop",
      "homeowner_occupied",
      "mean_pm25",
      "wf_pm25_idw_intrsct",
      "patients"
    ),
    .funs = list(
      ~ sum(., na.rm = TRUE),
      ~ weighted.mean(., w = tot_pop)
    )
  )

an_dat1 <- an_dat1 %>%
  select(date,
    grouping,
    visitsA = visitsA_sum,
    visitsI = visitsI_sum,
    visitsIC = visitsIC_sum,
    visitsR = visitsR_sum,
    visitsRC = visitsRC_sum,
    pov_p = pov_p_weighted.mean,
    edu_lt_hs_p = edu_lt_hs_p_weighted.mean,
    med_inc = med_inc_weighted.mean,
    tot_pop = tot_pop_sum,
    black_p = black_p_weighted.mean,
    hispan_p = hispan_p_weighted.mean,
    white_p = white_p_weighted.mean,
    under_5 = under_5_weighted.mean,
    p_5_19 = p_5_19_weighted.mean,
    p_20_64 = p_20_64_weighted.mean,
    p_65_more = p_65_more_weighted.mean,
    total_male_pop = total_male_pop_sum,
    homeowner_occupied = homeowner_occupied_weighted.mean,
    tmean = tmean_weighted.mean,
    mean_pm25 = mean_pm25_weighted.mean,
    wf_pm25 = wf_pm25_idw_intrsct_weighted.mean,
    patients = patients_sum
  )

# Lag all exposures ----
# group split to keep lags from bleeding into next grouping
an_dat2 <- an_dat1 %>%
  group_by(grouping) %>%
  group_split()
# add them to each dataframe
an_dat2 <- lapply(an_dat2, add_lags)
# return to one dataframe
an_dat2 <- bind_rows(an_dat2)

# Add temporal variables ----
# add date sequence to use as a temporal covariate
date <- sort(unique(an_dat2$date))
day <- wday(date)
day_seq <- seq(from = 1, to = 766, by = 0.5)
# add weekend day indicator
days <-
  data.frame(date, day_seq, day) %>%
  mutate(weekend = case_when(
    day == 6 ~ 1,
    day == 7 ~ 1,
    TRUE ~ 0
  ))
an_dat2 <- an_dat2 %>% left_join(days)

# add proportion male 
an_dat2 <- an_dat2 %>% 
  mutate(prop_male = total_male_pop/tot_pop)

# Write ----
write_csv(an_dat2,
          here("data_processing", "data_pm_analyses", "an_dat_daily.csv"))
