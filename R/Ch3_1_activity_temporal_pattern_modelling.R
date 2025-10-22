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

test <- aru_daily %>%
  #left_join(weather_data_cleaned) %>%
  mutate(year = as_factor(year),
         site = as_factor(site),
         yday_scaled = scale(yday),
         yday_scaled_poly = poly(yday_scaled, 2)) 

# initial fit - variable scaling issue
model0 <- glmer(detections ~ yday + (1|site) + (1|year),
                data = test,
                family = poisson)

# fit with scaled yday - overdispersion issue
model1 <- glmer(detections ~ yday_scaled + (1|year) + (1|site), 
             data = test,
             family = poisson)

# fit to avoid dispersion - by using year in fixed effect, or change the family from poisson to NB
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
model6 <- glmmTMB(detections ~ yday_scaled_poly + (1|year) + (1|site),
                            data = test,
                            family = nbinom2)

# fit this to see the year by year variation
model6_1 <- glmmTMB(detections ~ yday_scaled_poly + year + (1|site),
                    data = test,
                    family = nbinom2)

model6_2 <- glmmTMB(detections ~ yday_scaled_poly + year + (1|site/year),
                    data = test,
                    family = nbinom2)

model6_3 <- glmmTMB(detections ~ yday_scaled_poly*year + (1|site/year),
                    data = test,
                    family = nbinom2)




model7 <- gam(detections ~ s(yday_scaled, bs = "cs", k = 3) +
                s(year, bs = "re"), # model will have underdispersion if including site here
              data = test,
              family = nb(),
              method = 'REML')

model8 <- gam(detections ~ s(yday_scaled, bs = "cs", k = 3) +
                s(yday_scaled, year, bs = "re"),
              data = test,
              family = nb(),
              method = 'REML')

model9 <- gam(detections ~ s(yday_scaled, bs = "cs", k = 3) +
                s(year, bs = "re") +
                s(yday_scaled, year, bs = "re"),
              data = test,
              family = nb(),
              method = 'REML')


performance::check_overdispersion(model6_2)
performance::check_model(model9)




# model visualization -----------------------------------------------------

final_model <- model6_3 # model 6 or 9 are the best models

# models for each year random effect
final_model_vis <- test %>%
  mutate(predicted_population = predict(final_model, type = "response")) %>%
         #predicted_subject = predict(final_model, type = "response")) %>%
  #filter(year == 2022) %>%
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




# fit candidates
model0 <- glmmTMB(detections ~ yday_scaled_poly + year + (1|site),
                  data = test, family = nbinom2)

model_slope <- glmmTMB(detections ~ yday_scaled_poly + year + (1 + year|site),
                       data = test, family = nbinom2)

model_siteyear <- glmmTMB(detections ~ yday_scaled_poly + year + (1|site) + (1|site:year),
                          data = test, family = nbinom2)

model_yearRE <- glmmTMB(detections ~ yday_scaled_poly + year + (1|year) + (1|site),
                        data = test, family = nbinom2)


library(MuMIn)
AICc(model0, model_slope, model_siteyear, model_yearRE)

# nested LRT (if appropriate)
anova(model0, model_slope)         # check LRT
anova(model0, model_siteyear)      # etc.

# check variance components & R2
VarCorr(model_slope)
library(performance)
r2(model0); r2(model_slope)

# residual diagnostics
library(DHARMa)
plot(simulateResiduals(model_slope))
























