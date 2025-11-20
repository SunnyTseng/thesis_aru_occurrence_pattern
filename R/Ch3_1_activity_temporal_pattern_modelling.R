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
library(performance) # for model evaluation

# date time manipulation 
library(datawizard)


# Data - bird data cleaning ------------------------------------------------------

# # bird detections from 3 years of data
# bird_data <- birdnet_combine(here("data", "audio_output_combined"))
# 
# # keep only Olive-sided Flycatcher and valid detections
# bird_data_target <- bird_data %>%
#   birdnet_filter(species = "Olive-sided Flycatcher", threshold = 0.35)
# 
# # mutate site and recording, and other necessary columns
# bird_data_target_cleaned <- bird_data_target %>%
#   birdnet_add_datetime() %>%
#   mutate(site = str_split_i(filepath, pattern = "\\\\", i = -2)) %>%
#   select(site, date, year, yday, start, end)
  
#save(bird_data_target_cleaned, file = here("data", "R_objects", "bird_data_target_cleaned.rda"))
load(here("data", "R_objects", "bird_data_target_cleaned.rda"))

# Data - effort data cleaning ----------------------------------------------------

# # get the effort data (site active datetime)
# load(here("data", "effort", "effort_site_date.RData"))
# 
# # effort in each day (site active day)
# effort_daily <- effort_eval_1 %>%
#   mutate(date = date(datetime),
#          year = year(datetime),
#          yday = yday(datetime)) %>%
#   distinct(site, date, year, yday)

#save(effort_daily, file = here("data", "R_objects", "effort_daily.rda"))
load(here("data", "R_objects", "effort_daily.rda"))


# Data - weather data cleaning ---------------------------------------------------

# Historical Data from ECCC: https://climate.weather.gc.ca/historical_data/search_historic_data_e.html
# Lat = -124.29 ; Lon = 54.46
# weather_data <- list.files(here("data", "weather_2020_2022_daily"),
#                         pattern = ".csv$", recursive = TRUE,
#                         full.names = TRUE) %>%
#   map_df(~ read_csv(file = .))
# 
# weather_data_cleaned <- weather_data %>%
#   clean_names() %>%
#   mutate(date = as_date(date_time),
#          yday = yday(date),
#          year = year(date)) %>%
#   filter(month(date) %in% 5:7) %>% # the same as the effort, from May to July
#   select(date, year, yday, max_temp_c, min_temp_c, mean_temp_c) 

#save(weather_data_cleaned, file = here("data", "R_objects", "weather_data_cleaned.rda"))
load(here("data", "R_objects", "weather_data_cleaned.rda"))




# Data cleaning - check the temporal change of the detections -----------------------------

# qualified_ARUs <- bird_data_target_cleaned %>%
#   group_by(site, year) %>%
#   # summarize(OSFL_days = n_distinct(date)) %>%
#   # filter(OSFL_days >= 5) %>%
#   
#   # compute the difference (in days) between consecutive detections
#   mutate(day_diff = as.numeric(difftime(date, lag(date), units = "days"))) %>%
#   # flag if any consecutive detection days differ by exactly 1
#   summarize(has_consecutive = any(day_diff == 1, na.rm = TRUE),
#             .groups = "drop") %>%
#   # keep only those with at least one consecutive pair
#   filter(has_consecutive) %>%
#   select(site, year) 
#   
# 
# # effort data to fit the qualifed ARUs
# effort_filtered <- effort_daily %>%
#   semi_join(qualified_ARUs, by = c("site", "year"))
# 
# # detection data to fit the qualified ARUs
# detection_filtered <- bird_data_target_cleaned %>%
#   semi_join(qualified_ARUs, by = c("site", "year")) %>%
#   count(site, date, year, yday, name = "detections")
# 
# # summarize the effort data to get the number of detections on each date
# aru_daily <- detection_filtered %>%
#   full_join(effort_filtered, by = c("site", "date", "year", "yday")) %>%
#   mutate(detections = replace_na(detections, 0)) %>%
#   arrange(site, year, yday)

#save(aru_daily, file = here("data", "R_objects", "aru_daily_detections.rda"))
load(here("data", "R_objects", "aru_daily_detections.rda"))





# Exploratory - examination before modelling to check the trend ----------------

# Quick loess plots: detection trend by yday, by year and by site

model_data <- aru_daily %>%
  mutate(year = as_factor(year),
         site = as_factor(site),
         year_site = interaction(year, site)) 

# Raw detections by yday, faceted by year
detection_by_year <- ggplot(model_data, aes(yday, detections)) +
  geom_jitter(alpha = 0.15, height = 2, colour = "#53868B") +
  geom_smooth(method = "loess", se = FALSE, colour = "#53868B") +
  facet_wrap(~ year) +
  labs(y = "No. of OSFL detections",
       x = "Day of the year") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.88, 0.85))


# Raw detections by yday, faceted by site
detection_by_site <- ggplot(model_data, aes(yday, detections)) +
  geom_smooth(method = "loess", colour = "#53868B") +
  facet_wrap(~ site, ncol = 4) +
  labs(y = "No. of OSFL detections",
       x = "Day of the year") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.88, 0.85))



# combine and save the plot
year_site_raw <- (detection_by_year + detection_by_site) +
  plot_annotation(title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  theme = theme(),
                  tag_levels = "A") &
  theme(plot.tag = element_text(size = 16)) &
  ylab(NULL) & 
  theme(plot.margin = margin(5.5, 5.5, 0, 5.5))


year_site_raw_1 <- wrap_elements(panel = year_site_raw) +
  labs(tag = "No. of OSFL detections") +
  theme(plot.tag = element_text(size = 16, angle = 90),
        plot.tag.position = "left")


ggsave(plot = year_site_raw_1,
       filename = here("docs", "figures", "fig_detection_by_year_site.png"),
       width = 32,
       height = 16,
       units = "cm",
       dpi = 300)






# Model - Stepwise GAM model fitting ----------------------------------------------

# this section was done on Nov.17, to summarize the works that had been 
# done previously. As the previous versions were too messey to produce 
# meaningful report

# a null model - only random intercept for site
m0 <- gam(detections ~ 1 + s(site, bs="re"),
          family = nb(), data = model_data, method = "REML")

# single shared seasonal curve shape across years and sites
# k controls basis dimension — start ~20 (adjust if pattern is complex). Use bs="cc" because day-of-year is cyclic.
m1 <- gam(detections ~ s(yday, bs = "cc", k = 20) + 
            s(site, bs = "re"),
          family = nb(), data = model_data, method = "REML")

# single shared seasonal shape, differing overall level by year and by site
m2 <- gam(detections ~ s(yday, bs = "cc", k = 20) +
            s(year, bs = "re") + s(site, bs = "re"),
          family = nb(), data = model_data, method = "REML")

# each year gets its own smooth shape (i.e., same yday basis but different smooth for each year).
m3 <- gam(detections ~ s(yday, bs = "cc", by = year, k = 20) + 
            s(site, bs = "re"),
          family = nb(), data = model_data, method = "REML")


# Site × Year random intercept (separate intercepts for each year at each site), expecting sites have annual variation in intercepts
m4 <- gam(detections ~ s(yday, bs = "cc", k = 20) + 
            s(year_site, bs = "re"),
          family = nb(), data = model_data, method = "REML")

# Shared seasonal shape, with smooth interaction between yday and year to allow shape to vary smoothly across years
m5 <- gam(detections ~ s(yday, bs = "cc", k = 20) + 
            s(yday, year, bs = "fs", k = 10) + 
            s(site, bs = "re"),
          family = nb(), data = model_data, method = "REML")

# Each year has its own seasonal curve + Site × Year random intercept
m6 <- gam(detections ~ s(yday, bs="cc", by = year, k = 20) + 
            s(year_site, bs = "re"),
          family = nb(), data = model_data, method = "REML")









# Model - model comparison and selection ----------------------------------

m1_ml <- update(m1, method = "ML")
m2_ml <- update(m2, method = "ML")
m3_ml <- update(m3, method = "ML")
m4_ml <- update(m4, method = "ML")
m5_ml <- update(m5, method = "ML")
m6_ml <- update(m6, method = "ML")

model_list <- list(m1_ml, m2_ml, m3_ml, m4_ml, m5_ml, m6_ml)
names(model_list) <- c("m1","m2","m3","m4","m5","m6")

aic_table <- model.sel(model_list)


# show the model selection table
aic_table_save <- aic_table %>%
  as_tibble() %>%
  mutate(model = rownames(aic_table)) %>%
  select(model, df, AICc, delta, weight) %>%
  mutate(AICc = round(AICc, 0),
         delta = round(delta, 0)) %>%
  
  gt() %>%
  cols_label(model = "Model",
             df = "DF",
             delta = "Delta AICc",
             weight = "AICc weight") %>%
  fmt_percent(columns = weight,
              decimals = 0) %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12,
              heading.title.font.size = 16,
              heading.subtitle.font.size = 12)

gtsave(data = aic_table_save, 
       filename = here("docs", "tables", "aic_table.rtf"))


# final model evaluation and visualization --------------------------------

# choose the best and check the diagnostics
m_best <- m4

gam.check(m_best)

library(DHARMa)
res <- simulateResiduals(m_best)
plot(res)
testDispersion(res)
testZeroInflation(res)

library(gratia)
draw(m_best)


# model visualization
pred_data <- predict(m_best, type = "response",
                    se.fit = TRUE, exclude = "s(year_site)") %>%
  as_tibble() %>%
  rename(se_population = se.fit,
         pred_population = fit) %>%
  mutate(lower = pred_population - 1.96 * se_population,
         upper = pred_population + 1.96 * se_population) %>%
  mutate(pred_subject = predict(m_best, type = "response")) %>%
  bind_cols(model_data %>% 
              select(yday, detections, site, year, year_site))


# vis1 - visualize the error term in prediction
model_vis_population <- pred_data %>%
  ggplot(aes(x = yday)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.1, fill = "#1800ad") +
  geom_line(aes(y = pred_population),
            colour = "#1800ad", 
            linewidth = 2, alpha = 0.7) +
  labs(y = expression("Predicted detections (" * hat(y)[i] * ")"), 
       x = "Day of the year") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # setting the theme for the figure
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 3)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.88, 0.85))

model_vis_population


# vis2 - visualize the error term in prediction
model_vis_subject <- pred_data %>%
  ggplot(aes(x = yday)) + 
  geom_line(aes(y = pred_subject, group = year_site),
            colour = "#5ce1e6",
            linewidth = 2, alpha = 0.4) +
  labs(y = expression("Predicted detections (" * hat(y)[i] * ")"), 
       x = "Day of the year") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # setting the theme for the figure
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 3)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.88, 0.85))

model_vis_subject


# vis3 - visualize the error term in prediction
model_vis_residules <- pred_data %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "prediction_type",
               values_to = "predicted_values") %>%
  ggplot() +
  geom_jitter(aes(x = yday, 
                  y = detections - predicted_values, 
                  group = prediction_type,
                  colour = prediction_type),
              width = 0, height = 5, alpha = 0.4, size = 3) +
  scale_colour_manual(labels = c("pred_population" = "population",
                                 "pred_subject" = "subject"),
                      values = c("pred_population" = "#1800ad",
                                 "pred_subject" = "#5ce1e6")) +
  labs(y = expression("Residuals (" * y[i] - hat(y)[i] * ")"), 
       x = "Day of the year") +
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 3)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.8, 0.15))
  
model_vis_residules

model_vis_combined <- model_vis_population + model_vis_subject + model_vis_residules

ggsave(plot = model_vis_combined,
       filename = here("docs", "figures", "fig_model_vis_combined.png"),
       width = 36,
       height = 12,
       units = "cm",
       dpi = 300)


#

# trend modelling GLMM and GAM --------------------------------------------

model_data <- aru_daily %>%
  mutate(year = as_factor(year),
         site = as_factor(site),
         year_site = interaction(year, site),
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


# each year has its own seasonal curve
model_gam_3 <- gam(detections ~ s(yday, bs = "cc", by = year) + s(site, bs = "re"),
                   family = nb(), # negative binomial, mgcv estimates theta
                   data = model_data,
                   method = "REML")

model_gam_4 <- gam(detections ~ s(yday, bs = "cc", by = year) + 
                     s(year_site, bs = "re"),
                   family = nb(), # negative binomial, mgcv estimates theta
                   data = model_data,
                   method = "REML")

# single seasonal pattern across years

# Birds have one seasonal activity pattern that is stable across years.
# Different sites just have different overall detection levels.
model_gam_5 <- gam(detections ~ s(yday, bs = "cc") + s(site, bs = "re"),
                    family = nb(),
                    data = model_data,
                    method = "REML")

# One shared seasonal curve for all years (same timing).
# BUT each site can change independently across years in overall detection level.
model_gam_6 <- gam(detections ~ s(yday, bs = "cc") + s(year_site, bs = "re"),
                   family = nb(),
                   data = model_data,
                   method = "REML")

# One seasonal pattern that does not change in shape.
# Each year has a global shift (higher or lower overall activity).
# Each site has a constant difference (better or worse for detections).
model_gam_7 <- gam(detections ~ s(yday, bs = "cc") + s(year, bs = "re") + s(site, bs = "re"),
                   family = nb(),
                   data = model_data,
                   method = "REML")



model_gam_8 <- gam(detections ~ s(yday, bs="cc") + s(yday, year, bs="fs") + s(site, bs="re"))



# model goodness of fit check ---------------------------------------------

model_year <- model_gam_4 # based on AIC, REML, GAM diagnostics, overfitting, Biological plausibility
model_general <- model_gam_6 # based on AIC, REML, GAM diagnostics, overfitting, Biological plausibility

gam.check(model_gam_3)
gam.check(model_gam_4)

summary(model_gam_3)
summary(model_gam_4)

AIC(model_gam_3, model_gam_4)




# model visualization -----------------------------------------------------

# construct the dataframe for the predicted values
model_vis_data <- model_data %>%
  # model_year predictions
  mutate(pred_year_population = predict(model_year,
                                        type = "response",
                                        exclude = "s(year_site)")) %>%
  mutate(pred_year_subject = predict(model_year,
                                     type = "response")) 
  # model_general predictions
  # mutate(pred_general_population = predict(model_general, 
  #                                          type = "response",
  #                                          exclude = "s(year_site)")) %>%
  # mutate(pred_general_subject = predict(model_general,
  #                                       type = "response"))



# visualize the error term in prediction
model_vis_residules <- model_vis_data %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "prediction_type",
               values_to = "predicted_values") %>%
  ggplot() +
  geom_jitter(aes(x = yday, 
                  y = predicted_values - detections, 
                  group = prediction_type,
                  colour = prediction_type),
              width = 0,       # no horizontal jitter
              height = 5,    # adjust jitter around 0
              alpha = 0.4,
              size = 3) +
  scale_colour_manual(values = c("pred_year_population" = "#528B8B",
                                 "pred_year_subject" = "#8B5F65")) +
  
  theme_bw()

model_vis_residules

# visualize the predicted curves









model_vis <- model_vis_data %>%
  # plotting
  ggplot(aes(x = yday)) + 
  geom_point(aes(y = detections_scaled, group = site, colour = year),
            size = 2, alpha = 0.3) +
  geom_line(aes(y = predicted_population, colour = year), 
            linewidth = 2, alpha = 0.5) +
  labs(y = "OSFL detections (scaled)", x = "Day of Year") +
  scale_colour_manual(values = c("2020" = "#4F5D2F", 
                                 "2021" = "#D9AA55", 
                                 "2022" = "#70798C")) +
  # setting the theme for the figure
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.88, 0.85))

  
  # mutate(predicted_population = predict(final_model, type = "response", exclude = "s(site)")) %>%
  # 
  # ggplot(aes(x = yday)) + 
  # geom_point(aes(y = predicted_population,
  #                colour = site)) +
  # geom_line(aes(y = detections,
  #               group = site,
  #               colour = year),
  #           linewidth = 1.2, alpha = 0.3) +
  # 
  # facet_wrap(~ year, ncol = 1)
  #scale_colour_manual(values = c("#eac435", "#345995", "#7bccc4"))

model_vis


ggsave(plot = final_model_vis,
       filename = here("docs", "figures", "model_vis.png"),
       width = 20,
       height = 28,
       units = "cm",
       dpi = 300)




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
















