
###
### Figure making scripts
###


# library -----------------------------------------------------------------

# general data wrangling
library(tidyverse)
library(here)
library(raster)
library(sf)
library(ggspatial)
library(smoothr)

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

# aru coordinate
site_coordinates <- read_csv(here("data", "map", "JPRF_all_sites.csv"))

# JPRF boundary
jprf_raster <- raster(here("data", "JPRF_lidar_2015", 
                           "raster", "Crown_Closure_above_10m_zero1.tif")) %>%
  aggregate(fact = 15, fun = mean)

# Reclassify: inside = 1, outside = NA
jprf_raster[jprf_raster == 0] <- NA
jprf_raster[jprf_raster > 0] <- 1

jprf_poly <- jprf_raster %>%
  rasterToPolygons(dissolve = TRUE) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  fill_holes(threshold = units::set_units(4000000, m^2))



# daily occupancy matrix --------------------------------------------------

# wrangle data - check OSFL detection (1 or 0)
full_aru_daily <- bird_data_target_cleaned %>%
  count(site, date, year, yday, name = "detections") %>%
  right_join(effort_daily, by = c("site", "date", "year", "yday")) %>%
  mutate(detections = replace_na(detections, 0)) 


# visualization in a map version
OSFL_map <- full_aru_daily %>%
  
  # data wrangling
  group_by(site) %>%
  summarize(ARU_days = n(),
            OSFL_days = sum(detections > 0)) %>%
  left_join(site_coordinates, by = join_by("site" == "Name")) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  
  # make the plot
  ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_sf(data = jprf_poly, colour = "darkgrey", fill = "darkgreen", 
          size = 1, alpha = 0.2) +
  geom_sf(aes(size = ARU_days), colour = "#E69F00", alpha = 0.5) +
  geom_sf(aes(size = OSFL_days), colour = "#00868B", alpha = 0.5) +
  
  # fine tune
  labs(x = 'Easting', y = 'Northing', size = 'No. of days') +
  scale_size_continuous(range = c(1, 12)) +
  
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  
ggsave(plot = OSFL_map,
       filename = here("docs", "figures", "OFSL_map.png"),
       width = 28,
       height = 16,
       units = "cm",
       dpi = 300)





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

aru_proportion_vis <- aru_daily %>%
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
  labs(x = "Date", y = "Proportion of selected sites") +
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%b%d") +
  scale_fill_manual(values = c("#7bccc4", "#345995"),
                    labels = c("OSFL absent", "OSFL present")) +
  
  # theme
  theme_bw() +
  theme(strip.background = element_rect(fill = "azure3"),
        strip.text = element_text(size = 10),
        
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.12, 0.92))

aru_proportion_vis


ggsave(plot = aru_proportion_vis,
       filename = here("docs", "figures", "aru_proportion_vis.png"),
       width = 26,
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
