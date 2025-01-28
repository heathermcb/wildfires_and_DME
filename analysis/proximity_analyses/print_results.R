# Print results from models testing the association between being close to a
# wildfire and healthcare visits

# Libraries and read ------------------------------------------------------
library(tidyverse)
library(here)

filenames <- list.files(
  here("analysis", "proximity_analyses", "sensitivity_results"),
  pattern = "*.RDS",
  full.names = TRUE
)
s <- lapply(X = filenames, FUN = readRDS)

# Print relevant results --------------------------------------------------

# Getty evac + fire DID estimators and CIs
# outpatient DID
up <- exp(-0.025461 +  0.049589 * 1.96)
lw <- exp(-0.025461 -  0.049589 * 1.96)
RR <- exp(-0.025461)

# inpatient DID
up <- exp(-0.257790 +  0.213044 * 1.96)
lw <- exp(-0.257790  - 0.213044 * 1.96)
RR <- exp(-0.257790)

# emergency DID
up <- exp(-0.104451 +  0.134469 * 1.96)
lw <- exp(-0.104451 -  0.134469 * 1.96)
RR <- exp(-0.104451)

# inpatient cardiorespiratory DID
up <- exp(-0.211231 +  0.218459 * 1.96)
lw <- exp(-0.211231 -  0.218459 * 1.96)
RR <- exp(-0.211231)

# cardiorespiratory emergency DID
up <- exp(-0.166604 +  0.152795 * 1.96)
lw <- exp(-0.166604 -  0.152795 * 1.96)
RR <- exp(-0.166604)

# Woolsey evac + fire DID estimators and CIs
# outpatient DID
up <- exp(-0.132416 +  0.059152 * 1.96)
lw <- exp(-0.132416  - 0.059152 * 1.96)
RR <- exp(-0.132416)

# inpatient DID
up <- exp(0.304069 +  0.183851 * 1.96)
lw <- exp(0.304069 -  0.183851 * 1.96)
RR <- exp(0.304069)

# emergency DID
up <- exp(0.163087  + 0.141416 * 1.96)
lw <- exp(0.163087 -  0.141416 * 1.96)
RR <- exp(0.163087)

# inpatient cardiorespiratory DID
up <- exp(0.378354  + 0.187984 * 1.96)
lw <- exp(0.378354  - 0.187984 * 1.96)
RR <- exp(0.378354)

# emergency cardiorespiratory DID
up <- exp(0.12262   + 0.15417 * 1.96)
lw <- exp(0.12262  -  0.15417 * 1.96)
RR <- exp(0.12262)

# Woolsey fire exposure only (no evac) estimators and CIs
# outpatient
up1 <- exp(-0.132416 + 0.059152 * 1.96)
lw1 <- exp(-0.132416 - 0.059152 * 1.96)
RR1 <- exp(-0.132416)

# inpatient
up2 <- exp(0.304069 + 0.183851 * 1.96)
lw2 <- exp(0.304069 - 0.183851 * 1.96)
RR2 <- exp(0.304069)

# emergency
up3 <- exp(0.163087 + 0.141416 * 1.96)
lw3 <- exp(0.163087 - 0.141416 * 1.96)
RR3 <- exp(0.163087)

# inpatient cardiorespiratory DID
up4 <- exp(0.378354 + 0.187984  * 1.96)
lw4 <- exp(0.378354 - 0.187984  * 1.96)
RR4 <- exp(0.378354)

# emergency cardiorespiratory DID
up5 <- exp(0.12262 + 0.15417  * 1.96)
lw5 <- exp(0.12262 - 0.15417  * 1.96)
RR5 <- exp(0.12262)

# Woolsey evac only (no fire) estimators and CIs
# outpatient
up1 <- exp(-0.154366 + 1.96 * 0.088449)
lw1 <- exp(-0.154366 - 1.96 * 0.088449)
RR1 <- exp(-0.154366)

# inpatient
up2 <- exp(0.407379 + 1.96 * 0.267081)
lw2 <- exp(0.407379 - 1.96 * 0.267081)
RR2 <- exp(0.40737)

# emergency
up3 <- exp(0.196943 + 1.96 * 0.210639)
lw3 <- exp(0.196943 - 1.96 * 0.210639)
RR3 <- exp(0.196943)

# inpatient cardiorespiratory
up4 <- exp(0.521622 + 1.96 * 0.271998)
lw4 <- exp(0.521622 - 1.96 * 0.271998)
RR4 <- exp(0.521622)

# emergency cardiorespiratory
up5 <- exp(0.168280 + 1.96 *  0.228768)
lw5 <- exp(0.168280 - 1.96 *  0.228768)
RR5 <- exp(0.168280)
