##################################################
## Project: Global Ocean Protein Supply Curves
## Script purpose: Calculate aquaculture production per cell according to Gentry et al. (2017)
## Date (YYYY-MM-DD): 2019-01-16
## Author: Tyler Clavelle
## Notes: This script is a simplified/modified version of "Country Production Estimate.R" from Gentry et al. (2017)
##################################################

### Librarys
library(raster) 
library(tidyverse)
library(rgdal)

# Set tmp directory
tmpdir='~/tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)
# Remove old temp files to save space. Default is older than 24 hours.
removeTmpFiles() 

# Parameters & Regression Coefficients ------------------------------------

F_estCoef <- c(7.6792, (-5.8198)) #from regression estimated in VBGF_Fish_Final.r 
B_estCoef <- c(2.9959,(-1.6659)) #from regression estimate in VBGF_Bivalves.r 
density <- 20 #juveniles per m3
cagesize <- 9000 #m3
cagesperfarm <- 24 #located atleast 1 km apart
bivperfarm <- 130000000
weight35cm <- 554.8  ## in grams see VBGF_Fish_Final ** Tyler: Update this with group specific weights **  

# Analysis toggles
prep_rasters <- FALSE # Toggle to calculate intermediate rasters
lower_res <- TRUE # Toggle to lower resolution of rasters 
agg_factor <- 26.4 # 26.4 corresponds to a 0.22 degree resolution (highest resolution distance to shore raster)

# Data --------------------------------------------------------------------

# Prep intermediate rasters if needed
if(prep_rasters) {
  
  # GPI calculation (for now just read in mean GPI layer)
  FishPhiALLConstraints <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/Fish_Phi_All_Constraints_95LT2.tiff") # Global finfish phi raster
  BivalvePhiALLConstraints <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/Bivalve_Phi_All_Constraints_95LT1.tiff") # Global bivalve phi
  
  # suitable depth 
  suitableDepth <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/suitableDepthRaster.tif") 
  suitableDepth <- rotate(suitableDepth) # rotate to -180, 180
  depth <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/topo30_projected.tif") # Global depth raster.
  
  # # OHI2012 Countries
  # OHIcountries <- raster("aquaculture/data/regions.tif")
  # OHIcountriesLatLong <- projectRaster(OHIcountries,FishPhiALLConstraints, filename="aquaculture/data/OHICountriesLatLong.tif")
  OHIcountriesLatLong <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/OHICountriesLatLong.tif")
  
  # Create a raster with the years to grow a 35cm fish in each cell
  FishYears <- calc(FishPhiALLConstraints, fun=function(x){exp(F_estCoef[1]+F_estCoef[2]*log(x))})
  writeRaster(FishYears,'aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/FishYearsbyCell.tif', overwrite=TRUE)
  
  # Create a raster with the years it takes to grow a 4 cm bivlave in each cell
  BivalveYears <- calc(BivalvePhiALLConstraints, fun=function(x){exp(B_estCoef[1]+B_estCoef[2]*(x))})
  writeRaster(BivalveYears, 'aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/BivalveYearsByCell.tif', overwrite = TRUE)
  
  ### Suitability and Area per cell
  # Finfish
  FishSuitable <- FishPhiALLConstraints
  FishSuitable[FishSuitable>(0)] <- 1 # All suitable areas = to 1; Appears to be 15,639,851 suitable cells
  writeRaster(FishSuitable,'aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/FishSuitableByCell.tif', overwrite=TRUE)
  AreaFish <- area(FishSuitable, weights=FALSE, na.rm=TRUE, filename='aquaculture/data/AreaFishLT2.tif', overwrite=TRUE)

  # Bivalves
  BivalveSuitable <- BivalvePhiALLConstraints
  BivalveSuitable[BivalveSuitable>(0)] <- 1
  writeRaster(BivalveSuitable, 'aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/BivalveSuitableByCell.tif', overwrite = TRUE)
  AreaBivalve <- area(BivalveSuitable, weights=FALSE, na.rm=TRUE, filename = 'aquaculture/data/AreaBivalveLT1.tif')
  
} else {
  
  # Dims of rasters should be 21600, 43200, 933120000  (nrow, ncol, ncell) at full resolution. Approx 15.5 million cells after dropping NAs
  FishPhiALLConstraints <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/Fish_Phi_All_Constraints_95LT2.tiff") # Global finfish phi raster
  FishSuitable <- raster('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/FishSuitableByCell.tif') # fish suitability by cell
  OHIcountriesLatLong <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/OHICountriesLatLong.tif") # OHI Countries lat/long
  FishYears <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/FishYearsbyCell.tif") # Years to grow 35 cm fish
  AreaFish <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/AreaFishLT2.tif") # Area per cell

  BivalvePhiALLConstraints <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/Bivalve_Phi_All_Constraints_95LT1.tiff") # Global bivalve phi
  BivalveSuitable <- raster('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/BivalveSuitableByCell.tif') # Bivalve suitability
  BivalveYears <- raster('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/BivalveYearsByCell.tif') # Bivalve years
  AreaBivalve <- raster('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/AreaBivalveLT1.tif') # Bivalve area
  
  suitableDepth <- raster("aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/suitableDepthRaster.tif") # suitable depth 
  suitableDepth <- rotate(suitableDepth) # rotate to -180, 180
  depth <- raster('aquaculture/aquaculture_v1/data/raw/gentry_etal_2017/topo30_projected.tif')
  
  }

# Raster aggregation -----------------------------------

# Aggregate to lower resolution

### Aggregate finfish layers
fish_suitable <- aggregate(FishSuitable, fact = agg_factor, fun = sum) # number of suitable sites
fish_area <- aggregate(AreaFish, fact = agg_factor, fun = sum) # sum of area
fish_phi <- aggregate(FishPhiALLConstraints, fact = agg_factor, fun = mean) # mean phi
fish_years <- calc(fish_phi, fun=function(x){exp(F_estCoef[1]+F_estCoef[2]*log(x))}) # Create years to grow a 35cm fish in each cell
fish_yield_per_farm <- (weight35cm*density*cagesize*cagesperfarm) / 1000000 # Production per farm unit
fish_yield_per_cell <- (fish_yield_per_farm / fish_years) * fish_area # Calculte production per year per cell 

### Bivalves
bivalve_suitable <- aggregate(BivalveSuitable, fact = agg_factor, fun = sum) # number of suitable sites
bivalve_phi <- aggregate(BivalvePhiALLConstraints, fact = agg_factor, fun = mean) # mean phi
bivalve_area <- aggregate(AreaBivalve, fact = agg_factor, fun = sum) # sum of area
bivalve_years <- calc(bivalve_phi, fun = function(x){exp(B_estCoef[1]+B_estCoef[2]*(x))}) # calculate years to grow 4cm bivalve in each cell
bivalve_yield_per_cell <- (bivperfarm/bivalve_years)*bivalve_area 

### Depth - Need to find depth of suitable cells prior to aggregation
# We only want depth of cells that have suitable depth 
suitable_depth <- depth * suitableDepth
# Aggregate suitable depth raster
suitable_depth <- aggregate(suitable_depth, fact = agg_factor, fun = mean) # mean depth

# Area - merge area layers to single area
farm_area <- merge(fish_area, bivalve_area)

# EEZ
farm_eez <- aggregate(OHIcountriesLatLong, fact = agg_factor, fun = modal)

# Stack rasters and save
fish_stack <- stack(fish_suitable, fish_phi, fish_years, fish_yield_per_cell)
bivalve_stack <- stack(bivalve_suitable, bivalve_phi, bivalve_years, bivalve_yield_per_cell)
farm_stack <- stack(farm_area, suitable_depth)

writeRaster(fish_stack, filename = 'aquaculture/aquaculture_v1/results/fish_layers.tif', overwrite = TRUE)
writeRaster(bivalve_stack, filename = 'aquaculture/aquaculture_v1/results/bivalve_layers.tif', overwrite = TRUE)
writeRaster(farm_stack, filename = 'aquaculture/aquaculture_v1/results/farm_layers.tif', overwrite = TRUE)
writeRaster(farm_eez, filename = 'aquaculture/aquaculture_v1/results/farm_eez.tif', overwrite = TRUE)
