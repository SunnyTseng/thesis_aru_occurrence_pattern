
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





  group_by(week) %>%
  summarize(count = n()) 