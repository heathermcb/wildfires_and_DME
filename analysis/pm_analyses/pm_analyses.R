# Models testing the association between wildfire-generated PM 2.5 and 
# healthcare visits in Kaiser Southern California
# Heather, April 1st, 2022.

# Libraries and read ------------------------------------------------------
library(tidyverse)
library(splines)
library(dlnm)
library(mgcv)
library(lubridate)
library(here)

an_dat <-
  read_csv(here("data_processing", "data_pm_analyses", "an_dat_daily.csv"))
an_dat_weekly <-
  read_csv(here("data_processing", "data_pm_analyses", "an_dat_weekly.csv"))

# Primary models ----------------------------------------------------------
# 1
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1 <-
  gam(
    data = an_dat,
    visitsA ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      weekend +
      s(wk_mean_non_wf_pm) + 
      s(wk_mean_temp) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2 <-
  gam(
    data = an_dat,
    visitsI ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(wk_mean_non_wf_pm) + 
      s(wk_mean_temp) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3 <-
  gam(
    data = an_dat,
    visitsIC ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(wk_mean_non_wf_pm) + 
      s(wk_mean_temp) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4 <-
  gam(
    data = an_dat,
    visitsR ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(wk_mean_non_wf_pm) + 
      s(wk_mean_temp) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5 <-
  gam(
    data = an_dat,
    visitsRC ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(wk_mean_non_wf_pm) + 
      s(wk_mean_temp) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5))

# Save 
saveRDS(m1,
        here("analysis", "pm_analyses", "results", "outpatient.RDS"))
saveRDS(m2,
        here("analysis", "pm_analyses", "results", "inpatient.RDS"))
saveRDS(m3,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "inpatient_cardioresp.RDS"
        ))
saveRDS(m4,
        here("analysis", "pm_analyses", "results", "emergency.RDS"))
saveRDS(m5,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "emergency_cardioresp.RDS"
        ))


# Sensitivity analyses ----------------------------------------------------
# Section 1: weekly analysis, with lags up to 3 weeks, and keeping penalized 
# splines
# 1
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1_week_lag <-
  gam(
    data = an_dat_weekly,
    visitsA ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(nonwf_pm25) + 
      s(tempmean) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1_week_lag))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2_week_lag <-
  gam(
    data = an_dat_weekly,
    visitsI ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(nonwf_pm25) + 
      s(tempmean) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2_week_lag))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3_week_lag <-
  gam(
    data = an_dat_weekly,
    visitsIC ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(nonwf_pm25) + 
      s(tempmean) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3_week_lag))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4_week_lag <-
  gam(
    data = an_dat_weekly,
    visitsR ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(nonwf_pm25) + 
      s(tempmean) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4_week_lag))

# 5
print("Running model 5 - emergency visits for cardiorespiratory concerns")
start <- Sys.time()
m5_week_lag <-
  gam(
    data = an_dat_weekly,
    visitsRC ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      s(nonwf_pm25) + 
      s(tempmean) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5_week_lag))

# Save
saveRDS(m1_week_lag,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "outpatient_3_week_lags.RDS"
        ))
saveRDS(m2_week_lag,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "inpatient_3_week_lags.RDS"
        ))
saveRDS(
  m3_week_lag,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "inpatient_cardioresp_3_week_lags.RDS"
  )
)
saveRDS(m4_week_lag,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "emergency_3_week_lags.RDS"
        ))
saveRDS(
  m5_week_lag,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "emergency_cardioresp_3_week_lags.RDS"
  )
)

# Section 2: sensitivity to natural vs. penalized spline
# 1
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1_ns <-
  gam(
    data = an_dat,
    visitsA ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      weekend +
      ns(wk_mean_non_wf_pm, df = 4) + 
      ns(wk_mean_temp, df = 4) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1_ns))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2_ns <-
  gam(
    data = an_dat,
    visitsI ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(wk_mean_non_wf_pm, df = 4) + 
      ns(wk_mean_temp, df = 4) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2_ns))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3_ns <-
  gam(
    data = an_dat,
    visitsIC ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(wk_mean_non_wf_pm, df = 4) + 
      ns(wk_mean_temp, df = 4) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3_ns))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4_ns <-
  gam(
    data = an_dat,
    visitsR ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(wk_mean_non_wf_pm, df = 4) + 
      ns(wk_mean_temp, df = 4) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4_ns))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5_ns <-
  gam(
    data = an_dat,
    visitsRC ~ wf_pm_Lag0 + 
      wf_pm_Lag1 + 
      wf_pm_Lag2 + 
      wf_pm_Lag3 + 
      wf_pm_Lag4 + 
      wf_pm_Lag5 + 
      wf_pm_Lag6 + 
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(wk_mean_non_wf_pm, df = 4) + 
      ns(wk_mean_temp, df = 4) +
      ns(day_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5_ns))


# Save 
saveRDS(m1_ns,
        here("analysis", "pm_analyses", "results", "outpatient_ns.RDS"))
saveRDS(m2_ns,
        here("analysis", "pm_analyses", "results", "inpatient_ns.RDS"))
saveRDS(m3_ns,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "inpatient_cardioresp_ns.RDS"
        ))
saveRDS(m4_ns,
        here("analysis", "pm_analyses", "results", "emergency_ns.RDS"))
saveRDS(m5_ns,
        here(
          "analysis",
          "pm_analyses",
          "results",
          "emergency_cardioresp_ns.RDS"
        ))

# Section 3: sensitivity of weekly models to natural vs. penalized splines
# 1
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1_week_ns <-
  gam(
    data = an_dat_weekly,
    visitsA ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(nonwf_pm25, df = 4) + 
      ns(tempmean, df = 4) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1_week_ns))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2_week_ns <-
  gam(
    data = an_dat_weekly,
    visitsI ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(nonwf_pm25, df = 4) + 
      ns(tempmean, df = 4) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2_week_ns))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3_week_ns <-
  gam(
    data = an_dat_weekly,
    visitsIC ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(nonwf_pm25, df = 4) + 
      ns(tempmean, df = 4) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3_week_ns))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4_week_ns <-
  gam(
    data = an_dat_weekly,
    visitsR ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(nonwf_pm25, df = 4) + 
      ns(tempmean, df = 4) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4_week_ns))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5_week_ns <-
  gam(
    data = an_dat_weekly,
    visitsRC ~ wf_pm_week_1 +
      wf_pm_week_2 +
      wf_pm_week_3 +
      med_inc +
      pov_p +
      black_p +
      hispan_p +
      p_65_more +
      ns(nonwf_pm25, df = 4) + 
      ns(tempmean, df = 4) +
      ns(week_year_seq, df = 12),
    offset = log(patients),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5_week_ns))


# Save
saveRDS(
  m1_week_ns,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "outpatient_3_week_lags_ns.RDS"
  )
)
saveRDS(
  m2_week_ns,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "inpatient_3_week_lags_ns.RDS"
  )
)
saveRDS(
  m3_week_ns,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "inpatient_cardioresp_3_week_lags_ns.RDS"
  )
)
saveRDS(
  m4_week_ns,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "emergency_3_week_lags_ns.RDS"
  )
)
saveRDS(
  m5_week_ns,
  here(
    "analysis",
    "pm_analyses",
    "results",
    "emergency_cardioresp_3_week_lags_ns.RDS"
  )
)





