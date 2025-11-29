

# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(spOccupancy)
library(terra)
library(stars)
library(scales)




# import data -------------------------------------------------------------

# bird detection data
load(here("data", "R_objects", "bird_data_target_cleaned.rda"))



# effort data
load(here("data", "R_objects", "effort_daily.rda"))

effort_period <- effort_daily %>%
  # retain detection within study period
  filter(date %within% interval(ymd("2020-06-01"), ymd("2020-06-30")) |
           date %within% interval(ymd("2021-06-01"), ymd("2021-06-30")) | 
           date %within% interval(ymd("2022-06-01"), ymd("2022-06-30"))) %>%
  # check OSFL detection (1 or 0)
  mutate(period = floor_date(date, unit = "5 days")) %>%
  # only keep necessary data
  distinct(site, period)



# covariate data for modelling
cov_lidar <- readxl::read_xlsx(here("data", "JPRF_lidar_2015", 
                                    "JPRF_veg_Lidar_2015_summarized.xlsx"), 
                               sheet = "100") %>%
  rename(site = Site) %>%
  mutate(site = str_replace(site, "^N(\\d+)", "N_\\1")) %>%
  rename(age_80 = age80,
         d_lid_rip_wet_str_le = d_LID_rip_wet_str_le,
         d_vri_polyedge = d_VRIpolyedge,
         prop_decid = Prop_Decid_100m)


# covariate data for prediction
files <- here("data", "JPRF_lidar_2015", "raster") %>% 
  list.files(pattern = "\\.tif$", full.names = TRUE)

cov_prediction <- rast(files) %>%
  aggregate(fact = 15, fun = mean) %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(cc10 = Crown_Closure_above_10m_zero1,
         cc1_3 = Crown_Closure_between_1_and_3m_zero1)





# OSFL occurrence data ----------------------------------------------------

OSFL_occ_0 <- bird_data_target_cleaned %>%
  
  # retain detection within study period
  filter(date %within% interval(ymd("2020-06-01"), ymd("2020-06-30")) |
           date %within% interval(ymd("2021-06-01"), ymd("2021-06-30")) | 
           date %within% interval(ymd("2022-06-01"), ymd("2022-06-30"))) %>%
  
  # check OSFL detection (1 or 0)
  mutate(period = floor_date(date, unit = "5 days")) %>%
  
  # get the detection numbers: 1, or 0
  summarize(detections = n(), .by = c(site, period)) %>%
  right_join(effort_period) %>%
  mutate(detections = if_else(is.na(detections), 0, 1)) %>%
  arrange(period) %>%
  
  # check OSFL detections (NA or keep original value)
  pivot_wider(id_cols = site, 
              names_from = period, 
              values_from = detections) %>%
  arrange(site) 



# turn into a matrix and to reflect missing visit
OSFL_occ <- OSFL_occ_0 %>%
  column_to_rownames(var = "site") %>%
  as.matrix() 



# # visualization
# OSFL_vis <- OSFL_occ_0 %>%
#   pivot_longer(-site, names_to = "period", values_to = "detections") %>%
#   mutate(detections = replace_na(detections, -1),
#          period = ymd(period),
#          year = year(period)) %>%
#   # plot
#   ggplot(aes(x = period, y = site, fill = factor(detections))) +
#   geom_tile() +
#   scale_fill_manual(values = c("#EEE9E9", "#B9D3EE",  "#6CA6CD")) +
#   facet_wrap(~ year, scales = "free_x") +
# 
#   scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
#                date_labels = "%b%d") +
# 
#   theme_bw() +
#   labs(x = "Date", y = "Site") +
#   theme(legend.position = "none",
#         strip.background = element_rect(fill = "#C1CDCD"),
#         strip.text.x = element_text(size = 12),
# 
#         axis.title = element_text(size = 16),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
# 
# OSFL_vis
# 
# ggsave(filename = here("docs", "figures", "occupancy_matrix.png"),
#        width = 28,
#        height = 10,
#        units = "cm",
#        dpi = 300)




# occurrence covariates ---------------------------------------------------

OSFL_cov <- cov_lidar %>%
  right_join(OSFL_occ_0) %>%
  select(dem, slope, aspect, d_lid_rip_wet_str_le, 
         d_vri_polyedge, cc1_3, cc3_10, cc10, 
         age_80, prop_decid) 



# detection covariates ----------------------------------------------------

J <- nrow(OSFL_occ_0)
K <- ncol(OSFL_occ_0)
yday <- matrix(NA, nrow = J, ncol = K-1)
for (j in 1:J) {
  for (k in 2:K) {
    if (!is.na(OSFL_occ_0[j, k])) {
      yday[j, k -1] <- names(OSFL_occ_0)[k] %>% as_date() %>% yday()
    }}}

cc1_3 <- OSFL_cov$cc1_3

d_lid_rip_wet_str_le <- OSFL_cov$d_lid_rip_wet_str_le

OSFL_det <- list(yday = yday,
                 cc1_3 = cc1_3,
                 d_lid_rip_wet_str_le = d_lid_rip_wet_str_le)



# spOccupancy modelling ---------------------------------------------------

# data
OSFL_data <- list(y = OSFL_occ,
                  occ.covs = OSFL_cov,
                  det.covs = OSFL_det)

# formula
OSFL_occ_formula <- ~ scale(d_lid_rip_wet_str_le) + 
  scale(d_vri_polyedge) + scale(cc1_3) + scale(cc3_10) + 
  scale(cc10) + scale(age_80) + scale(prop_decid)


# initial setting
OSFL_inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(OSFL_data$y, 1, max, na.rm = TRUE))
OSFL_priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))
n.samples <- 5000
n.burn <- 3000
n.thin <- 2
n.chains <- 3

# model
out_full <- PGOcc(occ.formula = OSFL_occ_formula, 
                  det.formula = ~ cc1_3 + yday, 
                  data = OSFL_data, 
                  inits = OSFL_inits, 
                  n.samples = n.samples, 
                  priors = OSFL_priors, 
                  n.omp.threads = 1, 
                  verbose = TRUE, 
                  n.report = 1000, 
                  n.burn = n.burn, 
                  n.thin = n.thin, 
                  n.chains = n.chains)

out_cc10 <- PGOcc(occ.formula = ~ scale(cc10), 
                  det.formula = ~ cc1_3, 
                  data = OSFL_data, 
                  inits = OSFL_inits, 
                  n.samples = n.samples, 
                  priors = OSFL_priors, 
                  n.omp.threads = 1, 
                  verbose = TRUE, 
                  n.report = 1000, 
                  n.burn = n.burn, 
                  n.thin = n.thin, 
                  n.chains = n.chains)

out_null <- PGOcc(occ.formula = ~ 1, 
                  det.formula = ~ cc1_3 + yday, 
                  data = OSFL_data, 
                  inits = OSFL_inits, 
                  n.samples = n.samples, 
                  priors = OSFL_priors, 
                  n.omp.threads = 1, 
                  verbose = TRUE, 
                  n.report = 1000, 
                  n.burn = n.burn, 
                  n.thin = n.thin, 
                  n.chains = n.chains)

# Goodness of fit ---------------------------------------------------------

summary(out_full)

waicOcc(out_full)
waicOcc(out_cc10)
waicOcc(out_null)


plot(out_full, 'beta', density = FALSE) # Occupancy parameters.
plot(out_full, 'alpha', density = FALSE) # Detection parameters.


### For making table


### For making figure
# Extract occupancy & detection samples
occ_samps <- as.data.frame(out_full$beta)
det_samps <- as.data.frame(out_full$alpha)

# Add model type labels
occ_samps$model <- "Occupancy"
det_samps$model <- "Detection"

# Combine
samps <- bind_rows(occ_samps, det_samps)

samps_long <- samps %>%
  pivot_longer(cols = -model,
    names_to = "parameter",
    values_to = "value")

summary_df <- samps_long %>%
  group_by(model, parameter) %>%
  summarise(median = median(value, na.rm = TRUE),
    lower90 = quantile(value, 0.05, na.rm = TRUE),
    upper90 = quantile(value, 0.95, na.rm = TRUE),
    lower95 = quantile(value, 0.025, na.rm = TRUE),
    upper95 = quantile(value, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na() %>%
  filter(parameter != "(Intercept)") %>%
  mutate(parameter = c("CC (1–3 m)",
                       "Julian day",
                       "Forest age >80",
                       "CC (>10 m)",
                       "CC (1–3 m)",
                       "CC (3–10 m)",
                       "Dist. to water edge",
                       "Dist. to stand edge",
                       "Prop. deciduous"))


# Posterior effect sizes with 90% and 95% credible intervals
occupancy_effect <- ggplot(summary_df %>% filter(model == "Occupancy"),
       aes(x = reorder(parameter, median))) +
  
  # predicted intervals
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = lower95, ymax = upper95), 
                 linewidth = 2, alpha = 0.7, colour = "#191970") +
  geom_linerange(aes(ymin = lower90, ymax = upper90), 
                 linewidth = 6, colour = "#191970") +

  # fine tune
  labs(y = "Effect size (logit scale)", x = "") +
  
  # theme setting
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 13),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)))





detection_effect <- ggplot(summary_df %>% filter(model == "Detection"),
                           aes(x = reorder(parameter, median))) +
  
  # predicted intervals
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = lower95, ymax = upper95), 
                 linewidth = 2, alpha = 0.7, colour = "#48D1CC") +
  geom_linerange(aes(ymin = lower90, ymax = upper90), 
                 linewidth = 6, colour = "#48D1CC") +
  
  # fine tune
  labs(y = "", x = "") +
  
  # theme setting
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 13),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 10)))

effect_combined <- occupancy_effect + detection_effect

ggsave(plot = effect_combined,
       filename = here("docs", "figures", "fig_occupancy_modelling.png"),
       width = 26,
       height = 14,
       units = "cm",
       dpi = 300)






# prediction of occupancy probability -------------------------------------

occ_var <- cov_prediction %>%
  filter(cc10 != 0)

X.occ <- cbind(1, scale(occ_var$cc10))
  
out.pred <- predict(out_cc10, X.occ, type = "occupancy")

plot.dat <- data.frame(x = occ_var$x, 
                       y = occ_var$y, 
                       mean.psi = apply(out.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.pred$psi.0.samples, 2, sd), 
                       stringsAsFactors = FALSE) %>%
  st_as_sf(coords = c('x', 'y'), crs = 26910) %>%
  st_transform(crs = 4326) 

ggplot() + 
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_sf(data = plot.dat, aes(colour = mean.psi)) +
  scale_colour_gradient(low = "darkseagreen1", high = "darkgreen", na.value = NA) +
  labs(x = 'Easting', y = 'Northing', colour = '') +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  

# 

det_var <- cov_prediction %>%
  filter(cc1_3 != 0)

X.det <- cbind(1, det_var$cc1_3)

out.det.pred <- predict(out_cc10, X.det, type = "detection")

plot.det.dat <- data.frame(x = det_var$x, 
                           y = det_var$y, 
                           mean.psi = apply(out.det.pred$p.0.samples, 2, mean), 
                           sd.psi = apply(out.det.pred$p.0.samples, 2, sd), 
                           stringsAsFactors = FALSE) %>%
  st_as_sf(coords = c('x', 'y'), crs = 26910) %>%
  st_transform(crs = 4326) 

ggplot() + 
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_sf(data = plot.det.dat, aes(colour = mean.psi)) +
  scale_colour_gradient(low = "#FFF8DC", high = "#8B8878", na.value = NA) +
  labs(x = 'Easting', y = 'Northing', colour = '') +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))




# plot the cov_prediction
ggplot() + 
  geom_raster(data = cov_prediction %>% filter(cc10 != 0), 
              aes(x = x, y = y, fill = cc10)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Crown Closure above 10m (%)') +
  theme_bw()




