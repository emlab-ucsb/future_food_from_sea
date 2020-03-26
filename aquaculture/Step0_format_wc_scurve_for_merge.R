
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
outdir <- "aquaculture/aquaculture_v3/output"
plotdir <- "aquaculture/aquaculture_v3/figures"
datadir <- "/Users/cfree/Box Sync/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/" # Chris
# datadir <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/harvest_x_price_x_conversion.csv" # Tracey

# Load AQ supply curves
data_orig <- read.csv(file.path(datadir, "harvest_x_price_x_conversion.csv"), as.is=T)

# Conversion factors
conv_factors <- data_orig %>% 
  select(seafood_type, convert_val) %>% 
  unique() %>% 
  mutate(conversion_prop=1/convert_val)

# Inspect
table(data_orig$sector)
table(data_orig$seafood_type)


# Format data
#####################################################################################

# Notes from Tracey
# 1) there are SIX scenarios, and production for each price is separated by seafood_type — 
# I think you already  sum the production by price with na.rm = T to get total capture production (column name is “mt” for each price))
# 2) Remove scenarios “FMSY_orig_inputs” and “F0_orig_inputs” — we don’t need these

# Calculate total production at each price
colnames(data_orig)
data <- data_orig %>% 
  # Remove columns
  select(-X) %>% 
  # Rename columns to be consistent with old work
  rename(mt_yr=mt, conversion_val=convert_val) %>% 
  # Remove scenarios
  filter(!scenario %in% c("FMSY_orig_inputs", "F0_orig_inputs")) %>% 
  # Calculate edible meat totals
  mutate(meat_yr_scen1=(mt_yr*0.82)/conversion_val,
         meat_yr_scen2=meat_yr_scen1,
         meat_yr_scen3=meat_yr_scen2,
         meat_yr_scen4=meat_yr_scen1) %>% 
  # Calculate total capture production and total edible meat production by scenario
  group_by(scenario, price) %>% 
  summarize(wc_mt_yr=sum(mt_yr, na.rm=T),
            wc_meat_yr_scen1=sum(meat_yr_scen1, na.rm=T),
            wc_meat_yr_scen2=sum(meat_yr_scen2, na.rm=T),
            wc_meat_yr_scen3=sum(meat_yr_scen3, na.rm=T),
            wc_meat_yr_scen4=sum(meat_yr_scen4, na.rm=T))

# Check edible meat at current prices
filter(data, price==1296)

# Plot to check
g <- ggplot(data, aes(x=wc_mt_yr/1e6, y=price, group=scenario, col=scenario)) +
  geom_line() +
  labs(x="Capture production (millions mt)", y="Price (USD/mt)") +
  theme_bw()
g

# Export
saveRDS(data, file.path(outdir, "wc_supply_curve.Rds"))

  