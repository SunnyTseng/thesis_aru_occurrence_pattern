
###
### Figure making scripts
###


# library -----------------------------------------------------------------

# general data wrangling
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

# wrangle data - check OSFL detection (1 or 0)
full_aru_daily <- bird_data_target_cleaned %>%
  count(site, date, year, yday, name = "detections") %>%
  right_join(effort_daily, by = c("site", "date", "year", "yday")) %>%
  mutate(detections = replace_na(detections, 0)) 


# visualization 
full_aru_daily_vis <- full_aru_daily %>%
  
  # main plot
  ggplot(aes(x = date, 
             y = fct_reorder(site, desc(detections), .fun = sum), 
             fill = detections)) +
  geom_tile() + 
  
  # fine tune
  scale_fill_gradientn(colours = c("lightgrey", brewer.pal(9, "YlGnBu")),
                       values = scales::rescale(c(0, 0.01, max(full_aru_daily$detections, na.rm = TRUE))),
                       name = "No. daily detections",
                       guide = guide_colorbar(barwidth = 25, 
                                              barheight = 0.8,
                                              title.vjust = 1.1)) +
  facet_wrap(~ year, scale = "free_x") +   
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%m/%d") +
  
  # theme setting
  theme_bw() +
  labs(x = "Date", y = "ARU site") +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "azure3"),
        strip.text.x = element_text(size = 12),
        
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        
        axis.title = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

full_aru_daily_vis

ggsave(plot = full_aru_daily_vis,
       filename = here("docs", "figures", "full_aru_daily_vis.png"),
       width = 28,
       height = 16,
       units = "cm",
       dpi = 300)





# daily pattern across years - exploratory visualization ------------------

daily_detection_fig <- aru_daily %>%
  summarize(activity = sum(detections),
            total_ARUs = n_distinct(site),
            OSFL_ARUs = n_distinct(site[detections != 0]),
            .by = c(yday, year)) %>%
  mutate(no_OSFL_ARUs = total_ARUs - OSFL_ARUs) %>%
  pivot_longer(cols = c(OSFL_ARUs, no_OSFL_ARUs), names_to = "ARU_type") %>%
  
  # make the plot
  ggplot(aes(x = yday, fill = ARU_type)) +
  geom_bar(aes(y = value),
           stat = "identity",
           position = "fill",
           width = 1,
           alpha = 0.8) +
  
  # fine-tune
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Julian day", y = "Proportion of qualified ARUs") +
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%b%d") +
  scale_fill_manual(values = c("#7bccc4", "#345995"),
                    labels = c("OSFL absent", "OSFL present")) +
  
  # theme
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.12, 0.92),
        strip.text = element_text(size = 10))

daily_detection_fig


ggsave(plot = daily_detection_fig,
       filename = here("docs", "figures", "daily_detection_fig.png"),
       width = 28,
       height = 16,
       units = "cm",
       dpi = 300)



# others ------------------------------------------------------------------

# # compute start and end dates for each ARU (site-year)
# aru_boundaries <- aru_daily %>%
#   group_by(site, year) %>%
#   summarise(
#     start_yday = min(yday, na.rm = TRUE),
#     end_yday = max(yday, na.rm = TRUE),
#     .groups = "drop"
#   )
