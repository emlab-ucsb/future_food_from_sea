##################################################
## Project: Global Ocean Protein Supply Curves
## Script purpose: Build dataset of aquaculture production and cost parameters per cell
## Date (YYYY-MM-DD): 2019-02-21
## Author: Tyler Clavelle
## Notes: This script takes input rasters from country_production_tyler.R
##        adds economic parameters, and saves results in a dataframe
##################################################

# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(raster)
library(tidyverse)
library(countrycode)
library(sf)
library(RColorBrewer)
# library(fasterize)

# Distance from shore raster library
# devtools::install_github("mdsumner/distancetocoast")
library(distancetocoast) 

# World Band API 
library(wbstats)

# Scrape World Bank?
scrape_wb <- FALSE


# Read data
#####################################################################################

# Bands for finfish and bivalve stacks are:
# 1) Number of suitable cells at original resolution (0.00833 degrees, ~1km squared)
# 2) Phi
# 3) Years to produce a 35cm fish OR 4 cm bivalve
# 4) Annual yield per cell (MT for finfish; number of bivalves)

# Read raster stacks
fish_stack <- stack('aquaculture/aquaculture_v1/results/fish_layers.tif') 
bivalve_stack <- stack('aquaculture/aquaculture_v1/results/bivalve_layers.tif')

# Bands for farm stack are:
# 1) Farm area
# 2) Farm depth
farm_stack <- stack('aquaculture/aquaculture_v1/results/farm_layers.tif')
farm_eez <- raster('aquaculture/aquaculture_v1/results/farm_eez.tif')

# Reproject distance_to_coastline_10 to  0.22 x 0.22 degrees to match projection of our rasters
distance_to_shore <- projectRaster(distance_to_coastline_10, fish_stack)

# Region lookup table from OHI
regions <- read_csv('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/regionData.csv')


# Merge data
#####################################################################################

# Extract data
#############################################

# Finfish
fish_suitable_vec <- getValues(fish_stack@layers[[1]])
fish_phi_vec <- getValues(fish_stack@layers[[2]])
fish_years_vec <- getValues(fish_stack@layers[[3]])
fish_yield_per_cell_vec <- getValues(fish_stack@layers[[4]])

# Bivalves
bivalve_suitable_vec <- getValues(bivalve_stack[[1]])
bivalve_phi_vec <- getValues(bivalve_stack[[2]])
bivalve_years_vec <- getValues(bivalve_stack[[3]])
bivalve_yield_per_cell_vec <- getValues(bivalve_stack[[4]]) # currently in numbers of bivalves

# Physical specs
area_vec <- getValues(farm_stack@layers[[1]]) # area is same for cell regardless of fish or bivalves, so just use fish area
suitable_depth_vec <- getValues(farm_stack@layers[[2]])
distance_to_shore_vec <- getValues(distance_to_shore)

# Cell coordinates
cell_coords <- rasterToPoints(distance_to_shore) %>% 
  as.data.frame() %>% 
  rename(long_dd=x, lat_dd=y) %>% 
  dplyr::select(long_dd, lat_dd)

# EEZ 
eez_vec <- getValues(farm_eez)


# Visualize time to harvest
#############################################

hist(fish_years_vec, breaks=seq(0,40,0.5), main="", xlab="Time to h", xlim=c(0,10))
hist(bivalve_years_vec)


# Merge data
#############################################

# Create results data frame
aqua_results <- tibble(cell = 1:length(area_vec),
                       long_dd=cell_coords$long_dd,
                       lat_dd=cell_coords$lat_dd,
                       area_km2 = area_vec,
                       eez = eez_vec,
                       f_suitable = fish_suitable_vec,
                       f_phi  = fish_phi_vec,
                       f_yrs = fish_years_vec,
                       f_mt_yr = fish_yield_per_cell_vec,
                       b_suitable = bivalve_suitable_vec,
                       b_phi = bivalve_phi_vec,
                       b_yrs = bivalve_years_vec,
                       b_n_yr = bivalve_yield_per_cell_vec,
                       depth_m = suitable_depth_vec,
                       distance_km = distance_to_shore_vec / 1000) %>% 
  filter(!is.na(f_phi) | !is.na(b_phi))

# Number of evaluated patches
nrow(aqua_results)

# Add bivalve biomass
#############################################

### Convert bivalve results from numbers to weight using allometry parameters for blue mussel
# Using length to weight formula of: W=aL^b
# Assuming a to be 0.00001, based on McKinney, R. A., Glatt, S. M., & Williams, S. R. (2004). 
# Allometric length-weight relationships for benthic prey of aquatic wildlife in coastal marine habitats. Wildlife Biology, 10(1), 241-250.
# Assuming b to be 3.42 based on same analysis as a
b <- 3.42
a <- 0.00001
bivalve4cm_kg <- a*40^b / 1000 # 4 cm shell length (40mm)

# Add bivalve biomass
aqua_results <- aqua_results %>% 
  mutate(b_mt_yr = b_n_yr * bivalve4cm_kg / 1000)


# Add regions
#############################################

# Add ISO-3 character codes to region data
regions <- regions %>% 
  mutate(iso3c = countrycode(rgn_nam, origin = 'country.name', destination = 'iso3c'))

# Add regions to results
aqua_results <- aqua_results %>% 
  left_join(regions %>% 
              dplyr::select(eez = rgn_id, rgn_nam, iso3c), by="eez")



# Add country fuel and wage prices
#############################################

# Scrape if necessary
if(scrape_wb) {
  
  # Search for World Bank cost parameters (fuel, wage)
  # new_wb_cache <- wbcache() # Cache to story WB search results
  # wbsearch("net national income", cache = new_wb_cache) # wage
  # indicatorID                                  indicator
  # 12875 EP.PMP.SGAS.CD    Pump price for gasoline (US$ per liter)
  # 12876 EP.PMP.DESL.CD Pump price for diesel fuel (US$ per liter)
  # 8669 NY.ADJ.NNTY.PC.CD       Adjusted net national income per capita (current US$)
  fuel_wage_cost <- wb(indicator = c('EP.PMP.SGAS.CD', 'EP.PMP.DESL.CD', 'NY.ADJ.NNTY.PC.CD'), mrv = 1)
  
  # Build lookup table of costs per region
  region_costs <- fuel_wage_cost %>% 
    filter(indicatorID == 'EP.PMP.DESL.CD') %>% 
    select(iso3c, country, date, diesel_price_per_liter = value) %>% 
    full_join(fuel_wage_cost %>% 
                filter(indicatorID == 'NY.ADJ.NNTY.PC.CD') %>% 
                select(iso3c, country, date, income_per_capita = value))
  
  # Join with OHI regions look up table
  regions <- regions %>% 
    left_join(region_costs %>% 
                select(iso3c, diesel_price_per_liter, income_per_capita)) %>% 
    mutate(diesel_source = ifelse(!is.na(diesel_price_per_liter), 'country', 'world'), # use global "World" value if no country specific value
           diesel_price_per_liter = ifelse(is.na(diesel_price_per_liter), 0.97000, diesel_price_per_liter),
           income_source = ifelse(!is.na(income_per_capita), 'country', 'world'), # use global "World" value if no country specific value
           income_per_capita = ifelse(is.na(income_per_capita), 8413.5052, income_per_capita))
  
  # Save WB cost data
  write_csv(regions, path = 'aquaculture/aquaculture_v1/results/world_bank_costs.csv')
  
} else {
  # Read in world bank cost data
  regions <- read_csv(file = 'aquaculture/aquaculture_v1/results/world_bank_costs.csv')
}

# Join labor and fuel cost data to region raster
aqua_results <- aqua_results %>% 
  left_join(regions %>% 
              dplyr::select(eez = rgn_id, diesel_price_per_liter, income_per_capita), by="eez")

# Final formatting
#####################################################################################

# Final formatting
data <- aqua_results %>% 
  rename(country=rgn_nam, fuel_usd_l=diesel_price_per_liter, pc_income_usd=income_per_capita, iso3=iso3c) %>% 
  dplyr::select(cell, long_dd, lat_dd, area_km2, depth_m, distance_km, 
                eez, iso3, country, fuel_usd_l, pc_income_usd, everything())

# Map and export
#####################################################################################

# Quick map
g <- ggplot() +
  geom_raster(data=data, mapping=aes(x=long_dd, y=lat_dd, fill=f_mt_yr/1000)) +
  xlab("") + ylab("") +
  scale_fill_gradientn(colours=brewer.pal(9, "Reds"), name="Production (1000s mt / year)", na.value="white") +
  coord_fixed() +
  theme_bw()
g

# Export results
# write_csv(data, path = 'aquaculture/aquaculture_v1/results/aqua_production_results.csv')
