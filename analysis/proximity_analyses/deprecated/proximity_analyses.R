# Proximity analyses evaluating the association between being close to a fire
# boundary or being evacuated for a fire
# Heather - April 2nd.


# Libraries and read ------------------------------------------------------

library(tidyverse)
library(here)
library(mgcv)
library(splines)

an_dat <-
  read_csv(here("data_processing", "data_proximity_analyses", "an_dat.csv"))

an_dat_getty_evac <- 
  an_dat %>% filter(getty_evac == 1 | valid_control == 1)
an_dat_woolsey_evac <- 
  an_dat %>% filter(woolsey_evac == 1 | valid_control == 1)
an_dat_getty_prox <- 
  an_dat %>% filter(getty_disaster_20km == 1 | valid_control == 1)
an_dat_woolsey_prox <- 
  an_dat %>% filter(woolsey_disaster_20km == 1 | valid_control == 1)

# Evacuation and fire models ----------------------------------------------
# Exposure: fire or evacuation --------------------------------------------
# Getty: exposure is evacuation or fire proximity
# 1
print("Running Getty")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1 <-
  gam(
    visitsA ~ getty * getty_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsI ~ getty * getty_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsIC ~ getty * getty_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsR ~ getty * getty_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsRC ~ getty * getty_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5))

# Save all
saveRDS(m1,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_outpatient_both.RDS"
        ))
saveRDS(m2,
        here("analysis", "proximity_analyses", "results",
             "getty_inpatient_both.RDS"))
saveRDS(m3,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_inpatient_cardioresp_both.RDS"
        ))
saveRDS(m4,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_emergency_both.RDS"
        ))
saveRDS(m5,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_emergency_cardioresp_both.RDS"
        ))

# Woolsey: exposure is evacuation or fire proximity
# 1
print("Running Woolsey")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1w <-
  gam(
    visitsA ~ woolsey * woolsey_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1w))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2w <-
  gam(
    visitsI ~ woolsey * woolsey_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2w))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3w <-
  gam(
    visitsIC ~ woolsey * woolsey_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3w))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4w <-
  gam(
    visitsR ~ woolsey * woolsey_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4w))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5w <-
  gam(
    visitsRC ~ woolsey * woolsey_exposed + s(wkmntp) + s(wkyrseq),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5w))


# Save all
saveRDS(m1w,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_outpatient_all.RDS"
        ))
saveRDS(m2w,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_inpatient_all.RDS"
        ))
saveRDS(
  m3w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_inpatient_cardioresp_all.RDS"
  )
)
saveRDS(m4w,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_emergency_all.RDS"
        ))
saveRDS(
  m5w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_emergency_cardioresp_all.RDS"
  )
)




# Exposure: evacuation only -----------------------------------------------
# Getty: exposure is evacuation only
# 1
print("Running Getty")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1e <-
  gam(
    visitsA ~ getty * getty_evac + s(wkmntp) + s(wkyrseq),
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
          "results",
          "getty_outpatient_evac.RDS"
        ))
saveRDS(m2e,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_inpatient_evac.RDS"
        ))
saveRDS(
  m3e,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_inpatient_cardioresp_evac.RDS"
  )
)
saveRDS(m4e,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_emergency_evac.RDS"
        ))
saveRDS(
  m5e,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_emergency_cardioresp_evac.RDS"
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
          "results",
          "woolsey_outpatient_evac.RDS"
        ))
saveRDS(m2we,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_inpatient_evac.RDS"
        ))
saveRDS(
  m3we,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_inpatient_cardioresp_evac.RDS"
  )
)
saveRDS(m4we,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_emergency_evac.RDS"
        ))
saveRDS(
  m5we,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_emergency_cardioresp_evac.RDS"
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    "results",
    "getty_outpatient_fire.RDS"
  )
)
saveRDS(m2f,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_inpatient_fire.RDS"
        ))
saveRDS(
  m3f,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_inpatient_cardioresp_fire.RDS"
  )
)
saveRDS(m4f,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "getty_emergency_fire.RDS"
        ))
saveRDS(
  m5f,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_emergency_cardioresp_fire.RDS"
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
    data = an_dat,
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
          "results",
          "woolsey_outpatient_fire.RDS"
        ))
saveRDS(m2wf,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_inpatient_fire.RDS"
        ))
saveRDS(
  m3wf,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_inpatient_cardioresp_fire.RDS"
  )
)
saveRDS(m4wf,
        here(
          "analysis",
          "proximity_analyses",
          "results",
          "woolsey_emergency_fire.RDS"
        ))
saveRDS(
  m5wf,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_emergency_cardioresp_fire.RDS"
  )
)

# Sensitivity to spline parameterization
# Getty: exposure is evacuation or fire proximity, ns 4 dfs
# 1
print("Running Getty")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1 <-
  gam(
    visitsA ~ getty * getty_exposed + ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsI ~ getty * getty_exposed + ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsIC ~ getty * getty_exposed  +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsR ~ getty * getty_exposed + ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
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
    visitsRC ~ getty * getty_exposed ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5))

# Save all
saveRDS(
  m1,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_outpatient_both_ns.RDS"
  )
)
saveRDS(
  m2,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_inpatient_both_ns.RDS"
  )
)
saveRDS(
  m3,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_inpatient_cardioresp_both_ns.RDS"
  )
)
saveRDS(
  m4,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_emergency_both_ns.RDS"
  )
)
saveRDS(
  m5,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "getty_emergency_cardioresp_both_ns.RDS"
  )
)

# Woolsey: exposure is evacuation or fire proximity with natural splines df = 4
# 1
print("Running Woolsey")
print("Running model 1 - outpatient visits")
start <- Sys.time()
m1w <-
  gam(
    visitsA ~ woolsey * woolsey_exposed +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length1 <- end - start
print("Finished model 1")
print(length1)
print(summary(m1w))

# 2
print("Running model 2 - inpatient visits")
start <- Sys.time()
m2w <-
  gam(
    visitsI ~ woolsey * woolsey_exposed +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length2 <- end - start
print("Finished model 2")
print(length2)
print(summary(m2w))

# 3
print("Running model 3 - inpatient visits for cardiorespiratory concerns")
start <- Sys.time()
m3w <-
  gam(
    visitsIC ~ woolsey * woolsey_exposed +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length3 <- end - start
print("Finished model 3")
print(length3)
print(summary(m3w))

# 4
print("Running model 4 - emergency visits")
start <- Sys.time()
m4w <-
  gam(
    visitsR ~ woolsey * woolsey_exposed +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length4 <- end - start
print("Finished model 4")
print(length4)
print(summary(m4w))

# 5
print("Running model 5 - emergency visits for cardiorepiratory concerns")
start <- Sys.time()
m5w <-
  gam(
    visitsRC ~ woolsey * woolsey_exposed +
      ns(wkmntp, df = 4) + ns(wkyrseq, df = 4),
    data = an_dat,
    offset = log(person_time_exp),
    family = nb(link = "log")
  )
end <- Sys.time()
length5 <- end - start
print("Finished model 5")
print(length5)
print(summary(m5w))


# Save all
saveRDS(
  m1w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_outpatient_all_ns.RDS"
  )
)
saveRDS(
  m2w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_inpatient_all_ns.RDS"
  )
)
saveRDS(
  m3w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_inpatient_cardioresp_all_ns.RDS"
  )
)
saveRDS(
  m4w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_emergency_all_ns.RDS"
  )
)
saveRDS(
  m5w,
  here(
    "analysis",
    "proximity_analyses",
    "results",
    "woolsey_emergency_cardioresp_all_ns.RDS"
  )
)
