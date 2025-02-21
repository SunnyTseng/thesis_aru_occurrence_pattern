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
bird_data_target <- bird_data %>%
  filter(str_detect(common_name, "Olive-sided Flycatcher")) %>%
  filter(confidence >= 0.35)

# mutate site and recording, and other necessary columns
bird_data_target_cleaned <- bird_data_target %>%
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

weather_data_cleaned <- weather_data %>%
  clean_names() %>%
  mutate(date = as_date(date_time),
         yday = yday(date)) %>%
  filter(month(date) %in% 5:7) %>% # the same as the effort, from May to July
  select(date, yday, max_temp_c, min_temp_c, mean_temp_c) 





# check the temporal change of the detections -----------------------------

# data wrangling
daily_detection <- bird_data_target_cleaned %>%
  # only keep sites that have target species - at least n days with target species detections
  group_nest(site) %>%
  mutate(site_target_days = map_dbl(data, ~ n_distinct(.x$date))) %>%
  filter(site_target_days >= 10) %>% # 14 different sites with at least 10 days of target species detections
  unnest(data) %>%
  
  # calculate detections per ARU for each date
  group_by(date, yday) %>%
  summarize(detections = n()) %>%
  ungroup() %>%
  left_join(ARUs_given_date) %>%
  mutate(detections_per_ARU = detections / ARUs) %>%
  
  # remove outlier
  filter(detections_per_ARU < 10) %>%
  
  # combine with the weather data
  left_join(weather_data_cleaned) 


# correlation calculation
daily_detection_cor <- daily_detection %>%
  mutate(year = year(date)) %>%
  group_nest(year) %>%
  mutate(cor = map(data, ~ cor(.x[5:8]))) 


# visualization
daily_detection_fig <- daily_detection %>%
  mutate(year = year(date) %>% as_factor()) %>%
  
  ggplot(aes(x = yday, fill = year)) +
  geom_bar(aes(y = detections_per_ARU),
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










