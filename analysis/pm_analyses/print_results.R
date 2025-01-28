# Print results from models testing the association between wildfire-generated
# PM 2.5 and healthcare visits in Kaiser Southern California

# Libraries and read ------------------------------------------------------
library(tidyverse)
library(here)

filenames <- list.files(
  here("analysis", "pm_analyses", "results"),
  pattern = "*.RDS",
  full.names = TRUE
)
s <- lapply(X = filenames, FUN = readRDS)

# Print relevant results --------------------------------------------------
# EMERGENCY VISITS - no significant results
# emergency visits 3 week lags natural spline
summary(s[[1]])
# emergency visits 3 week lags penalized spline
summary(s[[2]])
# emergency visits 6 day lags natural spline
summary(s[[7]])
# emergency visits 6 day lag penalized spline
summary(s[[8]])

# INPATIENT VISITS - no significant results
# inpatient visits 3 week lag natural spline
summary(s[[9]])
# inpatient visits 3 week lag penalized spline
summary(s[[10]])
# inpatient visits 6 day lag natural spline
summary(s[[15]])
# inpatient visits 6 day lag penalized spline
summary(s[[16]])

# OUTPATIENT VISITS - there are some significant results which are sensitive to
# spline parameterization
# outpatient visits 3 week lag natural spline
summary(s[[17]])
# outpatient visits 3 week lag penalized spline
summary(s[[18]])
# outpatient visits 6 day lag natural spline
summary(s[[19]])
# outpatient visits 6 day lag penalized spline
summary(s[[20]])


# CARDIORESPIRATORY EMERGENCY VISITS - no significant results
# 3 week lag natural spline
summary(s[[3]])
# 3 week lag penalized spline
summary(s[[4]])
# 6 day lag natural spline
summary(s[[5]])
# 6 day lag penalized spline
summary(s[[6]])

# CARDIORESPIRATORY INPATIENT VISITS - no significant results
# natural spline 3 week lag
summary(s[[11]])
# penalized spline 3 week lag
summary(s[[12]])
# 6 day lag natural spline
summary(s[[13]])
# 6 day lag penalized spline
summary(s[[14]])


# RRs and CIs -------------------------------------------------------------
# for paper tables - copied into Table 1 word document.
# outpatient association RR estimates and CIs for 10 ug/m3 increase in
# wildfire-generated PM2.5:

# NATURAL SPLINE WEEKLY LAGS
# outpatient natural spline 3 week lag - lag 1 week estimate
up <- exp(1.866e-02 + 2.662e-03 * 1.96) ^ 10
lw <- exp(1.866e-02 - 2.662e-03 * 1.96) ^ 10
RR <- exp(1.866e-02) ^ 10
# lag 2 week estimate
up <- exp(1.796e-03 + 2.037e-03 * 1.96) ^ 10
lw <- exp(1.796e-03 - 2.037e-03 * 1.96) ^ 10
RR <- exp(1.796e-03) ^ 10
#  lag 3 week estimate
up <- exp(6.216e-03  + 1.847e-03 * 1.96) ^ 10
lw <- exp(6.216e-03  - 1.847e-03 * 1.96) ^ 10
RR <- exp(6.216e-03) ^ 10

# PENALIZED SPLINE WEEK LAGS
# outpatient penalized spline 3 week lag - lag 1 week estimate
up <- exp(1.178e-02 + 3.026e-03 * 1.96) ^ 10
lw <- exp(1.178e-02 - 3.026e-03 * 1.96) ^ 10
RR <- exp(1.178e-02) ^ 10
# lag 2 week estimate
up <- exp(4.401e-03 + 2.114e-03 * 1.96) ^ 10
lw <- exp(4.401e-03 - 2.114e-03 * 1.96) ^ 10
RR <- exp(4.401e-03) ^ 10
#  lag 3 week estimate
up <- exp(5.638e-03  + 1.852e-03 * 1.96) ^ 10
lw <- exp(5.638 * 10 ^ (-03)  - 1.852 * 10 ^ (-03) * 1.96) ^ 10
RR <- exp(5.638 * 10 ^ (-03)) ^ 10

# NATURAL SPLINE DAY LAGS
# outpatient natural spline day lags 0-6
# lag 0
up0 <- exp(-7.244e-04 + 1.195e-03) ^ 10
lw0 <- exp(-7.244e-04 - 1.195e-03) ^ 10
RR0 <- exp(-7.244e-04) ^ 10
# lag 1
up1 <- exp(-2.790 * 10 ^ (-03) + 1.292 * 10 ^ (-03)) ^ 10
lw1 <- exp(-2.790 * 10 ^ (-03) - 1.292 * 10 ^ (-03)) ^ 10
RR1 <- exp(-2.790 * 10 ^ (-03)) ^ 10
# lag 2
up2 <- exp(3.322e-03 + 1.387e-03) ^ 10
lw2 <- exp(3.322e-03 - 1.387e-03) ^ 10
RR2 <- exp(3.322e-03) ^ 10
# lag 3
up3 <- exp(8.479e-03  + 1.359e-03) ^ 10
lw3 <- exp(8.479e-03  - 1.359e-03) ^ 10
RR3 <- exp(8.479e-03) ^ 10
# lag 4
up4 <- exp(-7.156e-04  + 1.507e-03) ^ 10
lw4 <- exp(-7.156e-04  - 1.507e-03) ^ 10
RR4 <- exp(-7.156e-04) ^ 10
# lag 5
up5 <- exp(6.928e-03  + 1.265e-03) ^ 10
lw5 <- exp(6.928e-03  - 1.265e-03) ^ 10
RR5 <- exp(6.928e-03) ^ 10
# lag 6
up6 <- exp(1.125e-02 + 1.470e-03) ^ 10
lw6 <- exp(1.125e-02 - 1.470e-03) ^ 10
RR6 <- exp(1.125e-02) ^ 10

# PENALIZED SPLINE DAY LAGS
# outpatient natural spline day lags 0-6
# lag 0
up0 <- exp(-1.552e-03 + 1.193e-03) ^ 10
lw0 <- exp(-1.552e-03 - 1.193e-03) ^ 10
RR0 <- exp(-1.552e-03) ^ 10
# lag 1
up1 <- exp(-3.868e-03 + 1.284e-03) ^ 10
lw1 <- exp(-3.868e-03 - 1.284e-03) ^ 10
RR1 <- exp(-3.868e-03) ^ 10
# lag 2
up2 <- exp(2.698e-03 + 1.468e-03) ^ 10
lw2 <- exp(2.698e-03 - 1.468e-03) ^ 10
RR2 <- exp(2.698e-03) ^ 10
# lag 3
up3 <- exp(7.708e-03  + 1.490e-03) ^ 10
lw3 <- exp(7.708e-03  - 1.490e-03) ^ 10
RR3 <- exp(7.708e-03) ^ 10
# lag 4
up4 <- exp(-1.489e-03  + 1.639e-03) ^ 10
lw4 <- exp(-1.489e-03  - 1.639e-03) ^ 10
RR4 <- exp(-1.489e-03) ^ 10
# lag 5
up5 <- exp(6.652e-03  + 1.389e-03) ^ 10
lw5 <- exp(6.652e-03  - 1.389e-03) ^ 10
RR5 <- exp(6.652e-03) ^ 10
# lag 6
up6 <- exp(1.172e-02   + 1.654e-03) ^ 10
lw6 <- exp(1.172e-02   - 1.654e-03) ^ 10
RR6 <- exp(1.172e-02) ^ 10

