###
### Produce the dataset for Olive-sided Flycatcher
### Author: Sunny Tseng
### Date: 2025-02-20
###



# library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor) # for clean_names
library(lubridate)



# bird data cleaning ------------------------------------------------------

# bird detections from 3 years of data
bird_data <- list.files(here("data", "audio_output_combined"),
                        pattern = ".csv$", recursive = TRUE,
                        full.names = TRUE) %>%
  map_df(~ read_csv(file = .))

# keep only Olive-sided Flycatcher and valid detections
bird_data_OSFL <- bird_data %>%
  filter(str_detect(common_name, "Olive-sided Flycatcher")) %>%
  filter(confidence >= 0.35)

# mutate site and recording, and other necessary columns
bird_data_OSFL_cleaned <- bird_data_OSFL %>%
  mutate(site = str_split_i(filepath, pattern = "\\\\", i = -2),
         recording = str_split_i(filepath, pattern = "\\\\", i = -1)) %>%
  mutate(datetime = str_split_i(recording, pattern = ".WAV", i = 1) %>% as_datetime(),
         date = date(datetime),
         yday = yday(datetime)) %>%
  select(site, datetime, date, yday, start, end)



# effort data cleaning ----------------------------------------------------

# get the effort data
load(here("data", "effort", "effort_site_date.RData"))

# find the active # of ARUs for each of the date
ARUs_given_date <- effort_eval_1 %>%
  group_by(date = datetime %>% date()) %>%
  summarize(ARUs = n_distinct(site)) 



# weather data cleaning ---------------------------------------------------

# Historical Data from ECCC: https://climate.weather.gc.ca/historical_data/search_historic_data_e.html
# Lat = -124.29 ; Lon = 54.46
weather_data <- list.files(here("data", "weather_2020_2022_daily"),
                        pattern = ".csv$", recursive = TRUE,
                        full.names = TRUE) %>%
  map_df(~ read_csv(file = .))

weather_data_clean <- weather_data %>%
  clean_names() %>%
  mutate(date = as_date(date_time)) %>%
  filter(month(date) %in% 5:7) %>% # the same as the effort, from May to July
  select(date, min_temp_c, mean_temp_c, total_rain_mm, spd_of_max_gust_km_h)



# check the temporal change of the detections -----------------------------

# data wrangling
daily_detection <- bird_data_OSFL_cleaned %>%
  # only keep sites that have OSFL - at least 10 days with OSFL detections
  group_nest(site) %>%
  mutate(site_OSFL_days = map_dbl(data, ~ n_distinct(.x$date))) %>%
  filter(site_OSFL_days >= 15) %>% # 12 different sites with at least 15 days of OSFL detections
  unnest(data) %>%
  
  # calculate detections per ARU for each date
  group_by(date, yday) %>%
  summarize(detections = n()) %>%
  ungroup() %>%
  left_join(ARUs_given_date) %>%
  mutate(detections_per_ARU = detections / ARUs) %>%
  
  # remove outlier
  filter(detections_per_ARU < 10) 


# visualization
daily_detection_fig <- daily_detection %>%
  mutate(year = year(date) %>% as_factor()) %>%
  ggplot(aes(x = yday, y = detections_per_ARU, fill = year)) +
  geom_bar(stat = "identity",
           position = "identity",
           alpha = .7) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  theme_bw()

daily_detection_fig




# check the temporal change of the temperature ----------------------------


# data wrangling
daily_temperature <- bird_data_OSFL_cleaned %>%
  # only keep sites that have OSFL - at least 10 days with OSFL detections
  group_nest(site) %>%
  mutate(site_OSFL_days = map_dbl(data, ~ n_distinct(.x$date))) %>%
  filter(site_OSFL_days >= 15) %>% # 12 different sites with at least 15 days of OSFL detections
  unnest(data) %>%
  
  # calculate detections per ARU for each date
  group_by(date, yday) %>%
  summarize(detections = n()) %>%
  ungroup() %>%
  left_join(ARUs_given_date) %>%
  mutate(detections_per_ARU = detections / ARUs) %>%
  
  # remove outlier
  filter(detections_per_ARU < 10) 












