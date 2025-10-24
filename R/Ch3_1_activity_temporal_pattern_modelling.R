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
library(lme4) # for glm modelling, glmer - basic poisson regression
library(glmmTMB) # for glmm with flexible family, glmmTMB - negative binomial regression
library(gamm4) # for gam mixed effect modelling, gamm4 - generalized additive mixed model
library(splines) # for spline modelling
library(mgcv) # for gam modelling


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
  
#save(bird_data_target_cleaned, file = here("data", "R_objects", "bird_data_target_cleaned.rda"))


# effort data cleaning ----------------------------------------------------

# get the effort data (site active datetime)
load(here("data", "effort", "effort_site_date.RData"))

# effort in each day (site active day)
effort_daily <- effort_eval_1 %>%
  mutate(date = date(datetime),
         year = year(datetime),
         yday = yday(datetime)) %>%
  distinct(site, date, year, yday)

#save(effort_daily, file = here("data", "R_objects", "effort_daily.rda"))



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

#save(weather_data_cleaned, file = here("data", "R_objects", "weather_data_cleaned.rda"))



# check the temporal change of the detections -----------------------------

qualified_ARUs <- bird_data_target_cleaned %>%
  arrange(site, year, date) %>%  # ensure sorted by time
  group_by(site, year) %>%
  # compute the difference (in days) between consecutive detections
  mutate(day_diff = as.numeric(difftime(date, lag(date), units = "days"))) %>%
  # flag if any consecutive detection days differ by exactly 1
  summarize(has_consecutive = any(day_diff == 1, na.rm = TRUE),
            .groups = "drop") %>%
  # keep only those with at least one consecutive pair
  filter(has_consecutive) %>%
  select(site, year) 

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

#save(aru_daily, file = here("data", "R_objects", "aru_daily_detections.rda"))



# trend modelling GLMM and GAM --------------------------------------------

load(here("data", "R_objects", "aru_daily_detections.rda"))

model_data <- aru_daily %>%
  mutate(year = as_factor(year),
         site = as_factor(site),
         yday_poly = poly(yday, 2, raw = TRUE),
         yday_scaled = scale(yday),
         yday_scaled_poly = poly(yday_scaled, 2, raw = TRUE))


# initial fit - variable scaling issue
model_0 <- glmer(detections ~ yday * year + (1|site),
                 data = model_data,
                 family = poisson)

# fit with scaled yday - overdispersion issue
model_1 <- glmer(detections ~ yday_scaled * year+ (1|site),
                 data = model_data,
                 family = poisson)

# fit to avoid dispersion - by changing the family from poisson to NB
model_2 <- glmmTMB(detections ~ yday_scaled * year + (1|site),
                   data = model_data,
                   family = nbinom2)

# fit to improve the zero-inflation - no need!
model_3 <- glmmTMB(detections ~ yday_scaled * year + (1|site),
                   ziformula =~ 1,
                   data = model_data,
                   family = nbinom2)

# fit to account for the population trend
  # 1.poly solution
model_4 <- glmmTMB(detections ~ yday_scaled_poly * year + (1|site),
                            data = model_data,
                            family = nbinom2)

  # 2.spline solution
model_glmm_1 <- glmmTMB(detections ~ bs(yday, df = 6) * year + (1 | site),
                        data = model_data, 
                        family = poisson)

model_glmm_2 <- glmmTMB(detections ~ bs(yday, df = 6) * year + (1 | site),
                        data = model_data, 
                        family = nbinom2)

  # 3.GAM solution
model_gam_1 <- gam(detections ~ s(yday, bs = "cc", by = year) + s(site, bs = "re"),
                   family = poisson(),             # negative binomial, mgcv estimates theta
                   data = model_data,
                   method = "REML")

model_gam_2 <- gam(detections ~ s(yday, bs = "cc", by = year) + s(site, bs = "re"),
                   family = quasipoisson(),             # negative binomial, mgcv estimates theta
                   data = model_data,
                   method = "REML")

model_gam_3 <- gam(detections ~ s(yday, bs = "cc", by = year) + s(site, bs = "re"),
                   family = nb(),             # negative binomial, mgcv estimates theta
                   data = model_data,
                   method = "REML")

performance::check_overdispersion(model_5)
performance::check_model(model_5)



# model visualization -----------------------------------------------------

final_model <- model_gam_3 # model 6 or 9 are the best models

# models for each year random effect
final_model_vis <- model_data %>%
  mutate(predicted_population = predict(final_model, type = "response"), exclude = "s(site)") %>%
  #filter(site != "N_14", site != "14_41") %>% # remove sites that is outliers? 
  
  ggplot(aes(x = yday)) + 
  geom_point(aes(y = predicted_population,
                 colour = site)) +
  # geom_line(aes(y = detections, 
  #               group = site, 
  #               colour = year),
  #           linewidth = 1.2, alpha = 0.3) +
  facet_wrap(~ year, ncol = 1)
  #scale_colour_manual(values = c("#eac435", "#345995", "#7bccc4"))

final_model_vis




# Create a new dataframe for prediction — one sequence of yday per year
newdat <- model_data %>%
  dplyr::distinct(year) %>%
  tidyr::crossing(yday = seq(min(model_data$yday),
                             max(model_data$yday),
                             length.out = 200))

# Predict population-level (exclude site random effects)
newdat$pred <- predict(final_model, newdata = newdat, type = "response", exclude = "s(site)")

# Plot: one smooth per year
ggplot(newdat, aes(x = yday, y = pred, color = year)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  labs(y = "Predicted detections (population-level)",
       x = "Day of Year",
       title = "Seasonal activity patterns by year (aggregated across sites)") +
  theme_minimal(base_size = 14)
















