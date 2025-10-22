
#################################################################
### Chapter III: Olive-sided Flycatcher Pattern
### Author: Sunny Tseng
### Date: 2022/07/05
################################################################

library(tidyverse)
library(lubridate)
library(here)

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET.csv")) %>%
  filter(common_name == "Olive-sided Flycatcher" & confidence > 0.625)
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv")) %>%
  filter(common_name == "Olive-sided Flycatcher" & confidence > 0.625)

### Q: Is there any seasonal activity pattern? 

sites_2020 <- c("N_04", "N_06", "N_25")
sites_2021 <- c("14_09", "14_10", "14_41", "N_14")

for (location in sites_2021) {
  test <- data_2021 %>%
    filter(site == location) %>%
    mutate(week = ymd(paste0(year, month, day)) %>% week()) %>%
    group_nest(week, day) %>%
    ggplot(aes(week)) +
    geom_bar() +
    xlim(24, 29) +
    labs(title = paste0("2021 ", location))
  plot(test)
}



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

