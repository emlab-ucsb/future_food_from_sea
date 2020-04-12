
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)
library(scales)
library(ggplot2)
library(RColorBrewer)

# Directories
datadir <- "aquaculture/aquaculture_v3/output"
plotdir <- "aquaculture/aquaculture_v3/figures"

# Data
data <- readRDS(file = file.path(datadir, 'aqua_cost_per_production_results.Rds'))

# Load WC supply curves
# wc_scurve <- readRDS(file.path(datadir, "wc_supply_curve.Rds"))
feed <- read.csv(file=file.path(datadir, "wc_feed_availability_scen2-4.csv"), as.is=T)


# Current mean cost
g_curr_usd_mt <- 1296 # global average from Tracey
b_curr_usd_mt <- 1700 # 1.5 EURO/kg = 1.7 USD/kg =
f_curr_usd_mt <- 7000 # 60 NOK/kg = 7 USD/kg = Norwegian aquaculture analysis 2017


# Helper functions
#####################################################################################

# Function to calculate profits
calc_profits <- function(q, p, c){
  p*q - c*q
}

# Species
spp_num <- 1:3

# Capture scenarios
cap_scen <- unique(feed$scenario)

# Cost scalars
cost_scalars <- c(0.75, 1, 1.5, 2)

# Prices to evaluate
# prices <- 0:20000
prices <- seq(0,20000,1000)

# Build key
key <- expand.grid(species_num=1:3, cap_scen=cap_scen, cost_scalar=cost_scalars, price=prices)



# Finfish only supply curve (constrained by scraps)
#####################################################################################

# Loop through scenarios
i <- 1
j <- 1
k <- 2
for(i in 1:length(spp_num)){
  for(j in 1:length(cap_scen)){
    for(k in 1:length(cost_scalars)){
  
      # Scenario
      spp_do <- spp_num[i]
      cap_scen_do <- cap_scen[j]
      cost_scalar_do <- cost_scalars[k]
      print(paste(spp_do, cap_scen_do, cost_scalar_do))

      # Loop through prices
      x <- 10000
      f_scurve <- purrr::map_df(prices, function(x) {
        
        # Price to evaluate and associated FM constraint
        usd_mt <- x
        feed_avail_mt <- feed %>% 
          filter(scenario==cap_scen_do & price==usd_mt) %>% 
          pull(paste0("feed_mt_yr_by_spp", spp_do))
        
        # Calculate and order by profits, cut off cells once feed is exceeded
        results <- data %>%
          # Identify cost and feed columns of species being evaluated
          rename(f_usd_per_mt_do=paste0("f_usd_per_mt", spp_do),
                 feed_mt_yr_do=paste0("feed_mt_yr", spp_do)) %>% 
          # Calculate and order by profits
          # Scale cost column of species being evaluated
          select(cell, f_mt_yr, f_usd_per_mt_do, feed_mt_yr_do) %>%
          mutate(f_usd_per_mt_do_scaled=f_usd_per_mt_do*cost_scalar_do,
                 price=usd_mt,
                 feed_avail_mt=feed_avail_mt,
                 f_profits_yr=calc_profits(q=f_mt_yr, p=usd_mt, c=f_usd_per_mt_do_scaled)) %>% 
          filter(f_profits_yr>0) %>%
          arrange(desc(f_profits_yr)) %>%
          # Calculate feed demand of each additional cell
          mutate(feed_mt_sum=cumsum(feed_mt_yr_do)) %>%
          # Remove sites that would lead to excedance of feed budget
          filter(feed_mt_sum<=feed_avail_mt) %>%
          # Calculate total production at price
          group_by(price, feed_avail_mt) %>%
          summarize(tot_mt_yr=sum(f_mt_yr, na.rm=T),
                    feed_mt_yr=sum(feed_mt_yr_do, na.rm=T)) %>% 
          ungroup()
        
        # Return results
        return(results)
        
      })
      
      # Plot finfish curve
      g <- ggplot(arrange(f_scurve, price), aes(x=tot_mt_yr/1E6, y=price)) +
        geom_line() + 
        geom_hline(yintercept=f_curr_usd_mt, lty=3) +
        # geom_vline(xintercept=30*1E6/1E9) +
        theme_bw() + 
        labs(x="Production (millions mt)", y="Price (USD)", title=paste0("Fish ", spp_do, " - ", cap_scen_do, " - ", cost_scalar_do, " cost scalar"))
      print(g)
      
      # Export results
      outfile <- paste0("AQ_finfish_species", spp_do, "_scenario2_byproducts_only_",  
                        cap_scen_do, "_", format(cost_scalar_do, nsmall=2), "catch_scalar", ".Rdata")
      save(f_scurve, file=file.path(datadir, outfile))
      
    }
  }
  
}


