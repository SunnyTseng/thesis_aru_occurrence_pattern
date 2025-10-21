###
### Produce the dataset for Olive-sided Flycatcher
### Author: Sunny Tseng
### Date: 2025-02-20
###



# library -----------------------------------------------------------------

# data wrangling
library(tidyverse)
library(here)
library(janitor) # for clean_names
library(birdnetTools)

# modelling
library(lme4) # for glmer - basic poisson regression
library(glmmTMB) # for glmmTMB - negative binomial regression
library(gamm4) # for gamm4 - generalized additive mixed model

library(datawizard)
library(performance)


# bird data cleaning ------------------------------------------------------

# bird detections from 3 years of data
bird_data <- birdnet_combine(here("data", "audio_output_combined"))

# keep only Olive-sided Flycatcher and valid detections
bird_data_target <- bird_data %>%
  birdnet_filter(species = "Olive-sided Flycatcher", threshold = 0.35)

# mutate site and recording, and other necessary columns
bird_data_target_cleaned <- bird_data_target %>%
  birdnet_add_datetime() %>%
  mutate(site = str_split_i(filepath, pattern = "\\\\", i = -2)) %>%
  select(site, date, year, yday, start, end)
  

# effort data cleaning ----------------------------------------------------

# get the effort data (site active datetime)
load(here("data", "effort", "effort_site_date.RData"))

# effort in each day (site active day)
effort_daily <- effort_eval_1 %>%
  mutate(date = date(datetime),
         year = year(datetime),
         yday = yday(datetime)) %>%
  distinct(site, date, year, yday)


# weather data cleaning ---------------------------------------------------

# Historical Data from ECCC: https://climate.weather.gc.ca/historical_data/search_historic_data_e.html
# Lat = -124.29 ; Lon = 54.46
weather_data <- list.files(here("data", "weather_2020_2022_daily"),
                        pattern = ".csv$", recursive = TRUE,
                        full.names = TRUE) %>%
  map_df(~ read_csv(file = .))

weather_data_cleaned <- weather_data %>%
  clean_names() %>%
  mutate(date = as_date(date_time),
         yday = yday(date),
         year = year(date)) %>%
  filter(month(date) %in% 5:7) %>% # the same as the effort, from May to July
  select(date, year, yday, max_temp_c, min_temp_c, mean_temp_c) 





# check the temporal change of the detections -----------------------------

# qualified ARUs (site-year combinations) 
qualified_ARUs <- bird_data_target_cleaned %>%
  group_by(site, year) %>%
  filter(n_distinct(date) >= 1) %>%
  distinct(site, year)

# effort data to fit the qualifed ARUs
effort_filtered <- effort_daily %>%
  semi_join(qualified_ARUs, by = c("site", "year"))

# detection data to fit the qualified ARUs
detection_filtered <- bird_data_target_cleaned %>%
  semi_join(qualified_ARUs, by = c("site", "year")) %>%
  count(site, date, year, yday, name = "detections") 

# summarize the effort data to get the number of detections on each date
aru_daily <- detection_filtered %>%
  full_join(effort_filtered, by = c("site", "date", "year", "yday")) %>%
  mutate(detections = replace_na(detections, 0)) %>%
  arrange(site, year, yday)


# # compute start and end dates for each ARU (site-year)
# aru_boundaries <- aru_daily %>%
#   group_by(site, year) %>%
#   summarise(
#     start_yday = min(yday, na.rm = TRUE),
#     end_yday = max(yday, na.rm = TRUE),
#     .groups = "drop"
#   )


# GLMM modelling ----------------------------------------------------------

test <- aru_daily %>%
  left_join(weather_data_cleaned) %>%
  mutate(year = as_factor(year),
         yday_scaled = standardize(yday)) 

# initial fit - variable scaling issue
model0 <- glmer(detections ~ yday + (1|site) + (1|year),
                data = test,
                family = poisson)

# fit with scaled yday - overdispersion issue
model1 <- glmer(detections ~ yday_scaled + (1|year) + (1|site), 
             data = test,
             family = poisson)

# fit to avoid dispersion
model2 <- glmer(detections ~ yday_scaled + year + (1|site),
                data = test,
                family = poisson)

model3 <- glmmTMB(detections ~ yday_scaled + (1|year) + (1|site),
                  data = test,
                  family = nbinom2)

model4 <- glmmTMB(detections ~ yday_scaled + (1|year) + (1|site),
                data = test,
                family = nbinom2)

# fit to improve the zero-inflation - no need!
model5 <- glmmTMB(detections ~ yday_scaled + (1|year) + (1|site),
                  ziformula = ~1,
                  data = test,
                  family = nbinom2)

# fit to account for the population trend
model6 <- glmmTMB(detections ~ poly(yday_scaled, 2) + (1|year) + (1|site),
                            data = test,
                            family = nbinom2)

model7 <- gam(detections ~ s(yday_scaled, bs = "cs", k = 5) + 
                s(site, bs = "re"), 
              data = test,
              family = nb(),
              method = 'REML')

model8 <- gamm(detections ~ s(yday_scaled, bs = "cs", k = 5),
              data = test,
              family = poisson(),
              random = list(site = ~1, year = ~1))



performance::check_overdispersion(model8)
performance::check_model(model8)


# test methods to integrate the information across sites ------------------

# quick comparison
year_i <- 2020

aru_year <- aru_daily %>%
  filter(year == year_i)

aru_boundaries_year <- aru_boundaries %>%
  filter(year == year_i)

# compute mean/sd per ARU (weighted by detections)
aru_norm <- aru_year %>%
  group_by(site) %>%
  summarise(
    mu = weighted.mean(yday, detections, na.rm = TRUE),
    sigma = sqrt(weighted.mean((yday - mu)^2, detections, na.rm = TRUE))
  )

# generate smooth normal curves per site
fit_curves <- aru_norm %>%
  group_by(site) %>%
  summarise(
    yday = seq(100, 250, by = 1),  # or range(aru_year$yday)
    detections_fit = dnorm(yday, mu, sigma)
  ) %>%
  group_by(site) %>%
  mutate(detections_fit = detections_fit / max(detections_fit) * 
           max(aru_year$detections[aru_year$site == first(site)]))

# combine with barplot
# plot
ggplot(aru_year, aes(x = yday, y = detections, fill = site)) +
  geom_col(alpha = 0.3, position = "identity") +
  geom_line(data = fit_curves, aes(x = yday, y = detections_fit, color = site), size = 1) +
  
  # add start and end boundaries
  geom_vline(
    data = aru_boundaries_year,
    aes(xintercept = start_yday),
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +
  geom_vline(
    data = aru_boundaries_year,
    aes(xintercept = end_yday),
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +
  
  facet_grid(site ~ .) +
  xlim(125, 225) +
  theme_minimal() +
  labs(
    title = paste("Daily detections and normal fits per ARU (", year_i, ")", sep = ""),
    x = "Day of Year",
    y = "Detections",
    fill = "Site",
    color = "Normal fit"
  ) +
  theme(legend.position = "none")





# others ------------------------------------------------------------------

# correlation calculation
daily_detection_cor <- daily_detection %>%
  group_nest(year) %>%
  mutate(cor = map(data, ~ cor(.x[5:8]))) 

# visualization of daily detections across a year, across qualified sites. Note: interpret with caution as the number of ARUs differs across years in the same yday. 



daily_detection_fig <- aru_daily %>%
  summarize(activity = sum(detections), .by = c(yday, year)) %>%
  
  
  left_join(weather_data_cleaned) %>%
  mutate(year = as_factor(year)) %>%
  
  
  ggplot(aes(x = yday, fill = year)) +
  geom_bar(aes(y = activity),
           stat = "identity",
           position = "identity") +
  geom_line(aes(y = mean_temp_c / 4),
            colour = "#fb4d3d",
            size = 1.5, 
            alpha = 0.3) +
  
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Julian day") +
  scale_y_continuous(name = "Detections per ARU",
                     sec.axis = sec_axis(~.*4, name = "Mean temperature (degree C)")) +
  scale_fill_manual(values = c("#eac435", "#345995", "#7bccc4")) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size = 10))

daily_detection_fig










