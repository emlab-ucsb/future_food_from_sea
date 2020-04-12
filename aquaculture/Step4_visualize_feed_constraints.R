
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)

# Directories
datadir <- "aquaculture/aquaculture_v3/output"
plotdir <- "aquaculture/aquaculture_v3/figures"

# Read data
wc_scurve <- readRDS(file.path(datadir, "wc_supply_curve.Rds"))


# Build data
#####################################################################################

# Salmon feed
fcr1 <- 1.15 # Atlantic salmon - Ytrestøyl et al. (2015) Utilisation of feed resources in production of Atlantic salmon (Salmo salar) in Norway
feed_fm_perc1 <- 0.183 # 18.3% fishmeal and 10.9% fish oil
feed_fo_perc1 <- 0.109

# Milkfish feed
fcr2 <- 1.5
feed_fm_perc2 <- 0.01
feed_fo_perc2 <- 0.005

# Barramundi feed
fcr3 <- 1.5
feed_fm_perc3 <- 0.08
feed_fo_perc3 <- 0.03

# Parameters
perc_landings2reduction <- 0.18 # Cashion et al 2017 - page 3, results
perc_landings2fo_by <- 0.011
perc_landings2fm_by <- 0.069
perc_landings2fo_whole <- 0.0485
perc_landings2fm_whole <- 0.224
perc_fo2aq <- 0.80
perc_fm2aq <- 0.73
tech_scalars <- 1 - c(0.5, 0.75, 0.95) # % reductions in vector

# Estimate FO/FM, feed, and finfish production based on WC availability at price
# Scenario #1: Feed from by-products only
# Scenario #2: Feed from by-products and directed fisheries
# Scenario #3: Feed from by-products and directed fisheries plus tech advances (50%, 75%, 95% reductions)
data <- wc_scurve %>%
  # Landings from reduction fisheries
  mutate(rf_mt_yr = wc_mt_yr * perc_landings2reduction) %>% 
  # Scenario 1 calculations
  mutate(fo_mt_yr_by = wc_mt_yr * perc_landings2fo_by,
         fm_mt_yr_by = wc_mt_yr * perc_landings2fm_by,
         fo_aq_mt_yr_by = fo_mt_yr_by * perc_fo2aq ,
         fm_aq_mt_yr_by = fm_mt_yr_by * perc_fm2aq ,
         feed_mt_yr_by_spp1 = pmin(fo_aq_mt_yr_by/feed_fo_perc1 , fm_aq_mt_yr_by/feed_fm_perc1),
         fish_mt_yr_by_spp1 = feed_mt_yr_by_spp1 * fcr1,
         feed_mt_yr_by_spp2 = pmin(fo_aq_mt_yr_by/feed_fo_perc2 , fm_aq_mt_yr_by/feed_fm_perc2),
         fish_mt_yr_by_spp2 = feed_mt_yr_by_spp1 * fcr2,
         feed_mt_yr_by_spp3 = pmin(fo_aq_mt_yr_by/feed_fo_perc3 , fm_aq_mt_yr_by/feed_fm_perc3),
         fish_mt_yr_by_spp3 = feed_mt_yr_by_spp1 * fcr3) %>% 
  # Scenario 2 calculations
  mutate(fo_mt_yr_whole = rf_mt_yr * perc_landings2fo_whole,
         fm_mt_yr_whole = rf_mt_yr * perc_landings2fm_whole,
         fo_mt_yr_by_scen2 = (wc_mt_yr-rf_mt_yr) * perc_landings2fo_by,
         fm_mt_yr_by_scen2 = (wc_mt_yr-rf_mt_yr) * perc_landings2fm_by,
         fo_mt_yr_combo = fo_mt_yr_whole + fo_mt_yr_by_scen2,
         fm_mt_yr_combo = fm_mt_yr_whole + fm_mt_yr_by_scen2,
         fo_aq_mt_yr_combo = fo_mt_yr_combo * perc_fo2aq,
         fm_aq_mt_yr_combo = fm_mt_yr_combo * perc_fm2aq,
         feed_mt_yr_combo_spp1 = pmin(fo_aq_mt_yr_combo/feed_fo_perc1, fm_aq_mt_yr_combo/feed_fm_perc1),
         fish_mt_yr_combo_spp1 = feed_mt_yr_combo_spp1 * fcr1,
         feed_mt_yr_combo_spp2 = pmin(fo_aq_mt_yr_combo/feed_fo_perc2, fm_aq_mt_yr_combo/feed_fm_perc2),
         fish_mt_yr_combo_spp2 = feed_mt_yr_combo_spp2 * fcr2,
         feed_mt_yr_combo_spp3 = pmin(fo_aq_mt_yr_combo/feed_fo_perc3, fm_aq_mt_yr_combo/feed_fm_perc3),
         fish_mt_yr_combo_spp3 = feed_mt_yr_combo_spp3 * fcr3) %>% 
  # Scenario 3 calculations
  mutate(feed_mt_yr_combo_scen3a_spp1 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc1*tech_scalars[1]), fm_aq_mt_yr_combo/(feed_fm_perc1*tech_scalars[1])),
         fish_mt_yr_combo_scen3a_spp1 = feed_mt_yr_combo_scen3a_spp1 * fcr1,
         feed_mt_yr_combo_scen3b_spp1 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc1*tech_scalars[2]), fm_aq_mt_yr_combo/(feed_fm_perc1*tech_scalars[2])),
         fish_mt_yr_combo_scen3b_spp1 = feed_mt_yr_combo_scen3b_spp1 * fcr1,
         feed_mt_yr_combo_scen3c_spp1 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc1*tech_scalars[3]), fm_aq_mt_yr_combo/(feed_fm_perc1*tech_scalars[3])),
         fish_mt_yr_combo_scen3c_spp1 = feed_mt_yr_combo_scen3c_spp1 * fcr1,
         feed_mt_yr_combo_scen3a_spp2 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc2*tech_scalars[1]), fm_aq_mt_yr_combo/(feed_fm_perc2*tech_scalars[1])),
         fish_mt_yr_combo_scen3a_spp2 = feed_mt_yr_combo_scen3a_spp2 * fcr2,
         feed_mt_yr_combo_scen3b_spp2 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc2*tech_scalars[2]), fm_aq_mt_yr_combo/(feed_fm_perc2*tech_scalars[2])),
         fish_mt_yr_combo_scen3b_spp2 = feed_mt_yr_combo_scen3b_spp2 * fcr2,
         feed_mt_yr_combo_scen3c_spp2 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc2*tech_scalars[3]), fm_aq_mt_yr_combo/(feed_fm_perc2*tech_scalars[3])),
         fish_mt_yr_combo_scen3c_spp2 = feed_mt_yr_combo_scen3c_spp2 * fcr2,
         feed_mt_yr_combo_scen3a_spp3 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc3*tech_scalars[1]), fm_aq_mt_yr_combo/(feed_fm_perc3*tech_scalars[1])),
         fish_mt_yr_combo_scen3a_spp3 = feed_mt_yr_combo_scen3a_spp3 * fcr3,
         feed_mt_yr_combo_scen3b_spp3 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc3*tech_scalars[2]), fm_aq_mt_yr_combo/(feed_fm_perc3*tech_scalars[2])),
         fish_mt_yr_combo_scen3b_spp3 = feed_mt_yr_combo_scen3b_spp3 * fcr3,
         feed_mt_yr_combo_scen3c_spp3 = pmin(fo_aq_mt_yr_combo/(feed_fo_perc3*tech_scalars[3]), fm_aq_mt_yr_combo/(feed_fm_perc3*tech_scalars[3])),
         fish_mt_yr_combo_scen3c_spp3 = feed_mt_yr_combo_scen3c_spp3 * fcr3)

# Export
write.csv(data, file=file.path(datadir, "wc_feed_availability_scen2-4.csv"), row.names = F)


# Plot data
#####################################################################################

data_plot <- data %>% 
  # Reduce columns
  ungroup() %>% 
  select(scenario, price, contains("feed_mt_yr_")) %>% 
  # Gather
  gather(key="key", value="feed_mt_yr", 3:ncol(.)) %>% 
  # Add species/scenario columns
  mutate(species=substr(key, start=nchar(key), stop=nchar(key)),
         aq_scen=gsub("feed_mt_yr_", "", key) %>% gsub("_spp1|_spp2|_spp3", "", .)) %>% 
  # Rearrange columns
  rename(cap_scen=scenario) %>% 
  select(cap_scen, aq_scen, species, price, feed_mt_yr) %>% 
  # Format columns
  mutate(species=recode(species, 
                        "1"="1-Atlantic salmon",
                        "2" = "2-Milkfish",
                        "3" = "3-Barramundi"))

# Feed availability by species and scenario
g <- ggplot(data_plot, aes(x=feed_mt_yr/1e6, y=price, color=species)) +
  facet_grid(cap_scen ~ aq_scen, scale="free_x") +
  geom_line() +
  labs(x="Feed availability (millions mt)", y="Price (USD/mt)") +
  scale_color_discrete(name="Species") +
  theme_bw() +
  theme(legend.position = "bottom")
g



