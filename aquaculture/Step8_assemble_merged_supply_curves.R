
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

# Correct WC total production (should be total reduced by 18%)
wc <- read.csv(file.path(datadir, "wc_feed_availability_scen2-4.csv")) %>% 
  select(scenario, price, wc_mt_yr) %>% 
  mutate(wc_dhc_yr=wc_mt_yr *0.82)


# Loop through feed price scenarios
#####################################################################################

# List scenarios to merge
outfiles_all <- list.files(datadir)
outfiles_to_merge <- outfiles_all[grepl("WC_AQ_supply_curve_merged", outfiles_all)]

# Loop through files to merge and merge
data <- purrr::map_df(outfiles_to_merge, function(x) {
  
  sdata <- readRDS(file.path(datadir, x))
  
})

# Format data
data1 <- data %>% 
  # Remove Scenario 2
  filter(aq_scen != "Scenario 2 - byproducts only") %>% 
  # Add corrected WC data
  left_join(wc, by=c("cap_scen"="scenario", "price"="price")) %>% 
  mutate(mt_yr=ifelse(sector=="Capture fisheries", wc_dhc_yr, mt_yr)) %>% 
  # Remove unnecessary columns
  select(-c(wc_mt_yr, wc_dhc_yr))

# Export data
saveRDS(data1, file.path(datadir, "WC_AQ_supply_curve_merged_all_scenarios.Rds"))


# Check results
#####################################################################################

# Capture
##############################

# Capture data
cdata <- data1 %>% 
  filter(sector=="Capture fisheries") %>% 
  select(cap_scen, aq_scen, price, meat_yr, mt_yr) %>% 
  unique() %>% 
  gather(key="prod_type", value="prod_mt", 4:5) %>% 
  mutate(prod_type=recode_factor(prod_type, "mt_yr"="Total production", "meat_yr"="Edible meat"))

# Plot capture data
g <- ggplot(cdata, aes(x=prod_mt/1e6, y=price, group=cap_scen, color=cap_scen)) +
  geom_line() +
  facet_grid(aq_scen ~ prod_type, scale="free") +
  scale_color_discrete(name="Scenario") +
  labs(x="Production (millions mt)", y="Price (USD/mt)") +
  theme_bw() +
  theme(legend.position = "bottom")
g

# Bivalves
##############################

# Bivalve data
bdata <- data1 %>% 
  filter(sector=="Bivalve aquaculture") %>% 
  select(aq_cost_scalar, price, meat_yr, mt_yr) %>% 
  unique() %>% 
  gather(key="prod_type", value="prod_mt", 3:4) %>% 
  mutate(prod_type=recode_factor(prod_type, "mt_yr"="Total production", "meat_yr"="Edible meat"),
         aq_cost_scalar=format(aq_cost_scalar, digits=2))

# Plot bivalve data
g <- ggplot(bdata, aes(x=prod_mt/1e6, y=price, group=aq_cost_scalar, color=aq_cost_scalar)) +
  geom_line() +
  facet_wrap(~prod_type, ncol=2, scale="free") +
  scale_color_discrete(name="Cost scalar") +
  labs(x="Production (millions mt)", y="Price (USD/mt)") +
  theme_bw() +
  theme(legend.position = "bottom")
g


# Finfish
##############################

# Finfish data
fdata <- data1 %>% 
  filter(sector=="Finfish aquaculture") %>% 
  select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, price, meat_yr, mt_yr) %>% 
  unique() %>% 
  gather(key="prod_type", value="prod_mt", 6:7) %>% 
  mutate(prod_type=recode_factor(prod_type, 
                                 "mt_yr"="Total production", 
                                 "meat_yr"="Edible meat"),
         aq_cost_scalar=format(aq_cost_scalar, digits=2),
         aq_spp=recode_factor(aq_spp, 
                             "1"="Salmon-like",
                             "2"="Milkfish-like",
                             "3"="Barramundi-like"))

# Plot salmon data
g <- ggplot(fdata %>% 
              filter(aq_spp=="Salmon-like" & prod_type=="Total production"), 
            aes(x=prod_mt/1e6, y=price, color=aq_cost_scalar, group=aq_cost_scalar)) +
  geom_line() +
  facet_grid(cap_scen ~ aq_scen, scale="free") +
  scale_color_discrete(name="Cost scalar") +
  labs(x="Production (millions mt)", y="Price (USD/mt)", title="Salmon-like") +
  theme_bw() +
  theme(legend.position = "bottom")
g

# Plot milkfish data
g <- ggplot(fdata %>% 
              filter(aq_spp=="Milkfish-like" & prod_type=="Total production"), 
            aes(x=prod_mt/1e6, y=price, color=aq_cost_scalar, group=aq_cost_scalar)) +
  geom_line() +
  facet_grid(cap_scen ~ aq_scen, scale="free") +
  scale_color_discrete(name="Cost scalar") +
  labs(x="Production (millions mt)", y="Price (USD/mt)", title="Milkfish-like") +
  theme_bw() +
  theme(legend.position = "bottom")
g

# Plot barramundi data
g <- ggplot(fdata %>% 
              filter(aq_spp=="Barramundi-like" & prod_type=="Total production"), 
            aes(x=prod_mt/1e6, y=price, color=aq_cost_scalar, group=aq_cost_scalar)) +
  geom_line() +
  facet_grid(cap_scen ~ aq_scen, scale="free") +
  scale_color_discrete(name="Cost scalar") +
  labs(x="Production (millions mt)", y="Price (USD/mt)", title="Barramundi-like") +
  theme_bw() +
  theme(legend.position = "bottom")
g





