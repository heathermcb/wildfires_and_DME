library(here)
library(tidyverse)


dat <- read_csv(here("data_proximity_analyses", "an_dat.csv"))
datA <- dat %>% group_by(wkyrseq, getty_exposed) %>% summarize(meanvisitsA = mean(visitsA))

dat1 <- datA %>% filter(getty_exposed == 1)
dat2 <- datA %>% filter(getty_exposed == 0)

s1 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsA)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsA), linetype = 'solid', color = 'red') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
outpatient visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s1.pdf"), width = 5, height = 5)


datI <- dat %>% group_by(wkyrseq, getty_exposed) %>% summarize(meanvisitsI = mean(visitsI))

dat1 <- datI %>% filter(getty_exposed == 1)
dat2 <- datI %>% filter(getty_exposed == 0)

s2 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsI)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsI), linetype = 'solid', color = 'red') +
theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
inpatient visits')+
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s2.pdf"), width = 5, height = 5) 


datIC <- dat %>% group_by(wkyrseq, getty_exposed) %>% summarize(meanvisitsIC = mean(visitsIC))

dat1 <- datIC %>% filter(getty_exposed == 1)
dat2 <- datIC %>% filter(getty_exposed == 0)

s3 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsIC)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsIC), linetype = 'solid', color = 'red') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly cardiorespiratory
inpatient visits')  +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s3.pdf"), width = 5, height = 5)

datR <- dat %>% group_by(wkyrseq, getty_exposed) %>% summarize(meanvisitsR = mean(visitsR))

dat1 <- datR %>% filter(getty_exposed == 1)
dat2 <- datR %>% filter(getty_exposed == 0)

s4 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsR)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsR), linetype = 'solid', color = 'red') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
ED visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s4.pdf"), width = 5, height = 5) 
  
datRC <- dat %>% group_by(wkyrseq, getty_exposed) %>% summarize(meanvisitsRC = mean(visitsRC))

dat1 <- datRC %>% filter(getty_exposed == 1)
dat2 <- datRC %>% filter(getty_exposed == 0)
s5 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsRC)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsRC), linetype = 'solid', color = 'red') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly cardiorespiratory 
ED visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s5.pdf"), width = 5, height = 5)

dat <- read_csv(here("data_proximity_analyses", "an_dat.csv"))
datA <- dat %>% group_by(wkyrseq, woolsey_exposed) %>% summarize(meanvisitsA = mean(visitsA))

dat1 <- datA %>% filter(woolsey_exposed == 1)
dat2 <- datA %>% filter(woolsey_exposed == 0)
s6 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsA)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsA), linetype = 'solid', color = 'blue') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
outpatient visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s6.pdf"), width = 5, height = 5)
datI <- dat %>% group_by(wkyrseq, woolsey_exposed) %>% summarize(meanvisitsI = mean(visitsI))

dat1 <- datI %>% filter(woolsey_exposed == 1)
dat2 <- datI %>% filter(woolsey_exposed == 0)
s7 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsI)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsI), linetype = 'solid', color = 'blue') +
theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
inpatient visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 

ggsave(here("rmd_data_checks", "s7.pdf"), width = 5, height = 5)
datIC <- dat %>% group_by(wkyrseq, woolsey_exposed) %>% summarize(meanvisitsIC = mean(visitsIC))

dat1 <- datIC %>% filter(woolsey_exposed == 1)
dat2 <- datIC %>% filter(woolsey_exposed == 0)
s8 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsIC)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsIC), linetype = 'solid', color = 'blue') +
theme_minimal() + xlab('Week of study period') + ylab('Mean weekly cardiorespiratory 
inpatient visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 
ggsave(here("rmd_data_checks", "s8.pdf"), width = 5, height = 5)
datR <- dat %>% group_by(wkyrseq, woolsey_exposed) %>% summarize(meanvisitsR = mean(visitsR))

dat1 <- datR %>% filter(woolsey_exposed == 1)
dat2 <- datR %>% filter(woolsey_exposed == 0)
s9 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsR)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsR), linetype = 'solid', color = 'blue') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly all-cause 
ED visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 
ggsave(here("rmd_data_checks", "s9.pdf"), width = 5, height = 5)

datRC <- dat %>% group_by(wkyrseq, woolsey_exposed) %>% summarize(meanvisitsRC = mean(visitsRC))

dat1 <- datRC %>% filter(woolsey_exposed == 1)
dat2 <- datRC %>% filter(woolsey_exposed == 0)
s10 <- ggplot() + geom_line(aes(x = dat1$wkyrseq, y = dat1$meanvisitsRC)) + 
  geom_line(aes(x = dat2$wkyrseq, y = dat2$meanvisitsRC), linetype = 'solid', color = 'blue') +
  theme_minimal() + xlab('Week of study period') + ylab('Mean weekly cardiorespiratory 
ED visits') +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)) 
ggsave(here("rmd_data_checks", "s10.pdf"), width = 5, height = 5)

