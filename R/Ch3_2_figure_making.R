
###
### Figure making scripts
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)



# daily pattern across years - exploratory visualization ------------------
daily_detection_fig <- aru_daily %>%
  summarize(activity = sum(detections),
            no_ARUs = n_distinct(site),
            no_OSFL_ARUs = n_distinct(site[detections != 0]),
            .by = c(yday, year)) %>%
  
  # cleaning before sending to plot
  left_join(weather_data_cleaned) %>%
  mutate(year = as_factor(year),
         activity_daily = no_OSFL_ARUs / no_ARUs) %>%
  
  # make the plot
  ggplot(aes(x = yday, fill = year)) +
  geom_bar(aes(y = activity_daily),
           stat = "identity",
           position = "identity") +
  geom_line(aes(y = mean_temp_c / 40),
            colour = "#fb4d3d",
            size = 1.5, 
            alpha = 0.3) +
  
  facet_wrap(~ year, ncol = 1, scale = "free_y") +
  labs(x = "Julian day") +
  scale_y_continuous(name = "Daily detections per ARU",
                     sec.axis = sec_axis(~.*40, name = "Mean temperature (degree C)")) +
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
