# Create dataset for plotting study period wildfire and non-wildfire PM 2.5
# by county, and then save final wildfire PM figures.

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)

# Read and format ---------------------------------------------------------
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
  ) %>%
  mutate(zcta = as.factor(zcta))

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

# get data with counties, zctas, dates, and PM measurements
pm <- dat %>% select(date = admitdate, county, zcta) %>%
  left_join(wf) %>%
  mutate(all_pm25 = mean_pm25,
         nonwf_pm25 = mean_pm25 - wf_pm25_idw_intrsct,
         wf_pm25 = wf_pm25_idw_intrsct) %>%
  select(date, county, zcta, all_pm25, nonwf_pm25, wf_pm25)

# add fire dates
pm <-
  pm %>%
  mutate(
    Thomas = case_when(
      date > as.Date('2017-12-04', origin = '1970-01-01') &
        date < as.Date('2018-03-22', origin = '1970-01-01') ~ 1,
      TRUE ~ 0
    ),
    Woolsey = case_when(
      date > as.Date('2018-11-08', origin = '1970-01-01') &
        date < as.Date('2018-11-21', origin = '1970-01-01') ~ 1,
      TRUE ~ 0
    ),
    Getty = case_when(
      date > as.Date('2019-10-28', origin = '1970-01-01') &
        date < as.Date('2019-11-05', origin = '1970-01-01') ~ 1,
      TRUE ~ 0
    )
  )
# add indicator for any fire burning
pm <- pm %>%
  mutate(Fire = case_when(Getty == 1 ~ 1,
                          Woolsey == 1 ~ 2,
                          Thomas == 1 ~ 3,
                          TRUE ~ 0))


# Plot --------------------------------------------------------------------
# create data for plots of all pm, non-wf pm, and wf pm
# all pm
pm_plot <- pm %>%
  group_by(county, date) %>%
  summarise_at(
    .funs = c(mean),
    .vars = c("all_pm25", "Fire"),
    na.rm = TRUE
  ) %>%
  mutate(Fire = as.factor(Fire)) %>%
  mutate(Fire = case_when(
    Fire == 1 ~ 'Getty',
    Fire == 2 ~ 'Woolsey',
    Fire == 3 ~ 'Thomas',
    TRUE ~ 'Neither'
  )) %>%
  mutate(Fire = ordered(Fire, levels = c("Neither",
                                         "Getty",
                                         "Woolsey",
                                         "Thomas")))

# non wf pm
pm_plot2 <- pm %>% 
  group_by(county, date) %>%
  summarise_at(
    .funs = c(mean),
    .vars = c("nonwf_pm25", "Fire"),
    na.rm = TRUE
  ) %>% mutate(Fire = as.factor(Fire)) %>%
  mutate(Fire = case_when(Fire == 1 ~ 'Getty',
                          Fire == 2 ~ 'Woolsey',
                          Fire == 3 ~ 'Thomas',
                          TRUE ~ 'Neither')) %>%
  mutate(Fire = ordered(
    Fire,
    levels = c(
      "Neither",
      "Getty",
      "Woolsey",
      "Thomas")))

# wf pm
pm_plot3 <- pm %>% 
  group_by(county, date) %>%
  summarise_at(
    .funs = c(mean),
    .vars = c("wf_pm25", "Fire"),
    na.rm = TRUE
  ) %>% mutate(Fire = as.factor(Fire)) %>%
  mutate(Fire = case_when(Fire == 1 ~ 'Getty',
                          Fire == 2 ~ 'Woolsey',
                          Fire == 3 ~ 'Thomas',
                          TRUE ~ 'Neither')) %>%
  mutate(Fire = ordered(
    Fire,
    levels = c(
      "Neither",
      "Getty",
      "Woolsey",
      "Thomas")))

# save plots
pm1 <- pm_plot %>% ggplot() +
  geom_hline(
    yintercept = 35,
    color = "black",
    linetype = "dashed",
    size = 0.3
  ) +
  geom_line(aes(x = date, y = all_pm25, colour = Fire), size = 0.8) +
  facet_wrap(facets = "county", ncol = 2) +
  labs(title = "",
       subtitle = "") +
  xlab("") +
  ylab("Mean concentration") +
  scale_color_manual(values = c('#778899', 'darkorange', "red2", "darkred")) +
  theme_minimal(base_size = 25) +
  theme(legend.position = 'bottom')

pm2 <- pm_plot2 %>% ggplot() +
  geom_hline(
    yintercept = 35,
    color = "black",
    linetype = "dashed",
    size = 0.3
  ) +
  geom_line(aes(x = date, y = nonwf_pm25, colour = Fire), size = 0.8) +
  facet_wrap(facets = "county", ncol = 2) +
  labs(title = "",
       subtitle = "") +
  xlab("") +
  ylab("Mean concentration") +
  scale_color_manual(values = c('#778899', 'darkorange', "red2", "darkred")) +
  theme_minimal(base_size = 25) +
  theme(legend.position = 'bottom')


pm3 <- pm_plot3 %>% ggplot() +
  geom_hline(
    yintercept = 35,
    color = "black",
    linetype = "dashed",
    size = 0.3
  ) +
  geom_line(aes(x = date, y = wf_pm25, colour = Fire), size = 0.8) +
  facet_wrap(facets = "county", ncol = 2) +
  labs(title = "",
       subtitle = "") +
  xlab("") +
  ylab("Mean concentration") +
  scale_color_manual(values = c('#778899', 'darkorange', "red2", "darkred")) +
  theme_minimal(base_size = 25) + 
  theme(legend.position = 'bottom')


# Save --------------------------------------------------------------------

ggsave(
  filename = here("writing", "figures", "pm_figures", "mean_pm_by_county.pdf"),
  plot = pm1,
  device = "pdf",
  width = 15, 
  height = 18
)
ggsave(
  filename = here(
    "writing",
    "figures",
    "pm_figures",
    "mean_nonwf_pm_by_county.pdf"
  ),
  plot = pm2,
  device = "pdf",
  width = 15,
  height = 18
)
ggsave(
  filename = here(
    "writing",
    "figures",
    "pm_figures",
    "mean_wf_pm_by_county.pdf"
  ),
  plot = pm3,
  device = "pdf",
  width = 15,
  height = 18
)
