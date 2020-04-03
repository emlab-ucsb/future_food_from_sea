## Tracey Mangin
## March 9, 2020
## Price database

library(tidyverse)

## read price databases
prices <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/Exvessel price comparison national data.csv")
prices_est <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/Exvessel Price Database.csv")

# Get average prices for each species from national data
prices <- prices %>% 
  group_by(Species, Country, Year) %>% 
  summarize(Price = mean(Price, na.rm = T))


# Add estimated prices from Melnychuck et al. for remaining prices
prices_est <- prices_est %>% 
  select(scientific_name, Year, exvessel) %>% 
              rename(sci_name = scientific_name, 
                     year = Year,
                     price = exvessel)


