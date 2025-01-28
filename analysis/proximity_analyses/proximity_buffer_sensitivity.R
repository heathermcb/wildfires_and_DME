# Proximity analyses evaluating the association between being close to a fire
# boundary or being evacuated for a fire
# Heather - April 2nd.


# Libraries and read ------------------------------------------------------

library(tidyverse)
library(here)
library(mgcv)
library(splines)


an_dat <-
  read_csv(here(
    "data_processing",
    "data_proximity_analyses",
    "an_dat_sensitivity.csv"
  ))

unique_id <- seq(1, 582)
zcta <- unique(an_dat$zcta)
zcta_ids <- data.frame(unique_id, zcta)

an_dat <- an_dat %>%
  drop_na() %>% 
  left_join(zcta_ids) %>% arrange(unique_id)

an_dat_getty_evac <- 
  an_dat %>% filter(getty_evac == 1 | valid_control == 1)
an_dat_woolsey_evac <- 
  an_dat %>% filter(woolsey_evac == 1 | valid_control == 1)
an_dat_getty_prox <- 
  an_dat %>% filter(getty_disaster_20km == 1 | valid_control == 1)
an_dat_woolsey_prox <- 
  an_dat %>% filter(woolsey_disaster_20km == 1 | valid_control == 1)

# Evacuation and fire models ----------------------------------------------
# Exposure: evacuation only -----------------------------------------------
# Getty: exposure is evacuation only
# 1
print("Running Getty")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1e <-
  gam(
    visitsA ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1e))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2e <-
  gam(
    visitsI ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2e))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3e <-
  gam(
    visitsIC ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3e))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4e <-
  gam(
    visitsR ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4e))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5e <-
  gam(
    visitsRC ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5e))

# Save all
saveRDS(m1e,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "getty_outpatient_evac_new_sensitivity.RDS"
        ))
saveRDS(m2e,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "getty_inpatient_evac_new_sensitivity.RDS"
        ))
saveRDS(
  m3e,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "getty_inpatient_cardioresp_evac_new_sensitivity.RDS"
  )
)
saveRDS(m4e,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "getty_emergency_evac_new_sensitivity.RDS"
        ))
saveRDS(
  m5e,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "getty_emergency_cardioresp_evac_new_sensitivity.RDS"
  )
)

# Woolsey: exposure is evacuation only
# 1
print("Running Woolsey")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1we <-
  gam(
    visitsA ~ woolsey * woolsey_evac + s(wkmntp) +
      s(wkyrseq),
    data = an_dat_woolsey_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1we))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2we <-
  gam(
    visitsI ~ woolsey * woolsey_evac + s(wkmntp) +
      s(wkyrseq),
    data = an_dat_woolsey_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2we))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3we <-
  gam(
    visitsIC ~ woolsey * woolsey_evac + s(wkmntp) +
      s(wkyrseq),
    data = an_dat_woolsey_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3we))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4we <-
  gam(
    visitsR ~ woolsey * woolsey_evac + s(wkmntp) +
      s(wkyrseq),
    data = an_dat_woolsey_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4we))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5we <-
  gam(
    visitsRC ~ woolsey * woolsey_evac + s(wkmntp) +
      s(wkyrseq),
    data = an_dat_woolsey_evac,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5we))


# Save
saveRDS(m1we,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_outpatient_evac_new_sensitivity.RDS"
        ))
saveRDS(m2we,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_inpatient_evac_new_sensitivity.RDS"
        ))
saveRDS(
  m3we,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "woolsey_inpatient_cardioresp_evac_new_sensitivity.RDS"
  )
)
saveRDS(m4we,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_emergency_evac_new_sensitivity.RDS"
        ))
saveRDS(
  m5we,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "woolsey_emergency_cardioresp_evac_new_sensitivity.RDS"
  )
)




# Exposure: fire only -----------------------------------------------------
# Getty: exposure is fire only
# 1
print("Running Getty")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1f <-
  gam(
    visitsA ~ getty * getty_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1f))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2f <-
  gam(
    visitsI ~ getty * getty_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2f))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3f <-
  gam(
    visitsIC ~ getty * getty_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3f))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4f <-
  gam(
    visitsR ~ getty * getty_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4f))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5f <-
  gam(
    visitsRC ~ getty * getty_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_getty_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5f))

# Save
saveRDS(
  m1f,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "getty_outpatient_fire_new_sensitivity.RDS"
  )
)
saveRDS(m2f,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "getty_inpatient_fire_new_sensitivity.RDS"
        ))
saveRDS(
  m3f,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "getty_inpatient_cardioresp_fire_new_sensitivity.RDS"
  )
)
saveRDS(m4f,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "getty_emergency_fire_new_sensitivity.RDS"
        ))
saveRDS(
  m5f,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "getty_emergency_cardioresp_fire_new_sensitivity.RDS"
  )
)

# Woolsey: exposure is fire only
# 1
print("Running Woolsey")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1wf <-
  gam(
    visitsA ~ woolsey * woolsey_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_woolsey_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1wf))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2wf <-
  gam(
    visitsI ~ woolsey * woolsey_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_woolsey_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2wf))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3wf <-
  gam(
    visitsIC ~ woolsey * woolsey_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_woolsey_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3wf))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4wf <-
  gam(
    visitsR ~ woolsey * woolsey_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_woolsey_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4wf))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5wf <-
  gam(
    visitsRC ~ woolsey * woolsey_disaster_20km + s(wkmntp) + s(wkyrseq),
    data = an_dat_woolsey_prox,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5wf))


# Save
saveRDS(m1wf,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_outpatient_fire_new_sensitivity.RDS"
        ))
saveRDS(m2wf,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_inpatient_fire_new_sensitivity.RDS"
        ))
saveRDS(
  m3wf,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "woolsey_inpatient_cardioresp_fire_new_sensitivity.RDS"
  )
)
saveRDS(m4wf,
        here(
          "analysis",
          "proximity_analyses",
          "sensitivity_results",
          "woolsey_emergency_fire_new_sensitivity.RDS"
        ))
saveRDS(
  m5wf,
  here(
    "analysis",
    "proximity_analyses",
    "sensitivity_results",
    "woolsey_emergency_cardioresp_fire_new_sensitivity.RDS"
  )
)
