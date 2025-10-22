
###
### Figure making scripts
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)

# for plotting
library(RColorBrewer)


# load data ---------------------------------------------------------------

# bird data cleaned
load(here("data", "R_objects", "bird_data_target_cleaned.rda"))

# effort data cleaned
load(here("data", "R_objects", "effort_daily.rda"))

# weather data cleaned
load(here("data", "R_objects", "weather_data_cleaned.rda"))

# aru activity data for temporal pattern modelling
load(here("data", "R_objects", "aru_daily_detections.rda"))


# daily occupancy matrix --------------------------------------------------

OSFL_occ_0 <- bird_data_target_cleaned %>%
  
  # check OSFL detection (1 or 0)
  count(site, date, year, yday, name = "detections_daily") %>%
  full_join(effort_daily, by = c("site", "date", "year", "yday")) %>%
  mutate(detections_daily = if_else(is.na(detections_daily), 0, 1)) %>%
  
  # check OSFL detections (NA or keep original value)
  pivot_wider(id_cols = site,
              names_from = date,
              values_from = detections_daily) 


# visualization 
OSFL_vis <- OSFL_occ_0 %>%
  pivot_longer(-site, names_to = "period", values_to = "detections_daily") %>%
  mutate(detections_daily = replace_na(detections_daily, -1),
         period = ymd(period),
         year = year(period)) %>%
  # plot
  ggplot(aes(x = period, y = site, fill = factor(detections_daily))) +
  geom_tile() + 
  scale_fill_manual(values = c("white", "lightsteelblue1", "darkslategrey")) +
  facet_wrap(~ year, scales = "free_x") + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%b%d") +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  
  theme_bw() +
  labs(x = "Date", y = "Site") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "azure3"),
        strip.text.x = element_text(size = 12),
        
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

OSFL_vis



# # compute start and end dates for each ARU (site-year)
# aru_boundaries <- aru_daily %>%
#   group_by(site, year) %>%
#   summarise(
#     start_yday = min(yday, na.rm = TRUE),
#     end_yday = max(yday, na.rm = TRUE),
#     .groups = "drop"
#   )





# daily pattern across years - exploratory visualization ------------------

daily_detection_fig <- aru_daily %>%
  summarize(activity = sum(detections),
            total_ARUs = n_distinct(site),
            OSFL_ARUs = n_distinct(site[detections != 0]),
            .by = c(yday, year)) %>%
  mutate(no_OSFL_ARUs = total_ARUs - OSFL_ARUs) %>%
  pivot_longer(cols = c(OSFL_ARUs, no_OSFL_ARUs), names_to = "ARU_type") %>%
  
  # cleaning before sending to plot
  # left_join(weather_data_cleaned) %>%
  # mutate(year = as_factor(year),
  #        activity_daily = no_OSFL_ARUs / no_ARUs) %>%
  
  # make the plot
  ggplot(aes(x = yday, fill = ARU_type)) +
  geom_bar(aes(y = value),
           stat = "identity",
           position = "fill") +
  # geom_line(aes(y = mean_temp_c / 40),
  #           colour = "#fb4d3d",
  #           size = 1.5, 
  #           alpha = 0.3) +
  
  facet_wrap(~ year, ncol = 1, scale = "free_y") +
  labs(x = "Julian day") +
  # scale_y_continuous(name = "Daily detections per ARU",
  #                    sec.axis = sec_axis(~.*40, name = "Mean temperature (degree C)")) +
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



