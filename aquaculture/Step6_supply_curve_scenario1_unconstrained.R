
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

# Get world data
world <- fortify(map_data('world'))

# Current mean cost
g_curr_usd_mt <- 1296 # global average from Tracey
b_curr_usd_mt <- 1700 # 1.5 EURO/kg = 1.7 USD/kg =
f_curr_usd_mt <- 7000 # 60 NOK/kg = 7 USD/kg = Norwegian aquaculture analysis 2017


# Helper functions and parameters
#####################################################################################

# Function to calculate profits
calc_profits <- function(q, p, c){
  p*q - c*q
}

# Species
spp_num <- 1:3

# Cost scalars
cost_scalars <- c(0.75, 1, 1.5, 2)

# Prices to evaluate
# prices <- 0:20000
prices <- seq(0,20000,1)

# Build key
key <- expand.grid(species_num=1:3, cost_scalar=cost_scalars, price=prices)


# Finfish only supply curve
#####################################################################################

# Loop through feed price scenarios
i <- 1
j <- 2
for(i in 1:length(spp_num)){
  for(j in 1:length(cost_scalars)){
  
    # Cost scalar to do
    spp_do <- i
    cost_scalar_do <- cost_scalars[j]
  
    # Loop through prices
    f_scurve_eez <- purrr::map_df(prices, function(x) {
      
      # Calculate production at price
      usd_mt <- x
      results <- data %>%
        # Identify cost and feed columns of species being evaluated
        rename(f_usd_per_mt_do=paste0("f_usd_per_mt", spp_do),
               feed_mt_yr_do=paste0("feed_mt_yr", spp_do)) %>% 
        # Scale cost column of species being evaluated
        mutate(f_usd_per_mt_do_scaled=f_usd_per_mt_do*cost_scalar_do) %>% 
        # Calculate profits - classify cells based on max profitability
        mutate(price=usd_mt,
               f_profits_yr=calc_profits(q=f_mt_yr, p=usd_mt, c=f_usd_per_mt_do_scaled)) %>% 
        filter(f_profits_yr>0) %>% 
        # Summarize by type
        group_by(country, iso3, price) %>% 
        summarize(tot_mt_yr=sum(f_mt_yr, na.rm=T),
                  feed_mt_yr=sum(feed_mt_yr_do, na.rm=T)) %>% 
        filter(!is.na(price))
      
      # Return results
      return(results)
      
    })
    
    # Overall
    f_scurve <- f_scurve_eez %>% 
      group_by(price) %>% 
      summarize(tot_mt_yr=sum(tot_mt_yr, na.rm=T),
                feed_mt_yr=sum(feed_mt_yr, na.rm=T))
    
    # Plot finfish curve
    g <- ggplot(f_scurve, aes(x=tot_mt_yr/1E9, y=price)) +
      geom_line(col='red') + 
      geom_hline(yintercept=f_curr_usd_mt, lty=3) +
      # geom_vline(xintercept=30*1E6/1E9) +
      theme_bw() + 
      labs(x="Production (billions of MT)", y="Cost of finfish (USD / mt)", title=paste0("Fish ", spp_do, " - ", cost_scalar_do, " cost scalar"))
    print(g)
    
    # Plot feed demand
    # g <- ggplot(f_scurve, aes(x=feed_mt_yr/1E9, y=price)) +
    #   geom_line() + 
    #   # geom_vline(xintercept=30*1E6/1E9) +
    #   theme_bw() + 
    #   xlab("Feed demand (billions mt)") + ylab("Cost of finfish (USD / mt)")
    # g
    
    # Export results
    outfile <- paste0("AQ_finfish_species", spp_do, "_scenario1_unconstrained_w_eez_", format(cost_scalar_do, nsmall=2), "catch_scalar", ".Rdata")
    save(f_scurve, f_scurve_eez, 
         file=file.path(datadir, outfile))
    
  }

}


# Bivalve only supply curve
#####################################################################################

# Loop through feed price scenarios
i <- 1
for(i in 1:length(cost_scalars)){
    
  # Cost scalar to do
  cost_scalar_do <- cost_scalars[i]

  # Loop through prices
  b_scurve_eez <- purrr::map_df(prices, function(x) {
    
    # Calculate production at price
    usd_mt <- x
    results <- data %>%
      # Calculate profits - classify cells based on max profitability
      mutate(price=usd_mt,
             b_profits_yr=calc_profits(q=b_mt_yr, p=usd_mt, c=b_usd_per_mt*cost_scalar_do)) %>% 
      filter(b_profits_yr>0) %>% 
      # Summarize by type
      group_by(country, iso3, price) %>% 
      summarize(tot_mt_yr=sum(b_mt_yr, na.rm=T)) %>% 
      filter(!is.na(price))
    
    # Return results
    return(results)
    
  })

  # Overall
  b_scurve <- b_scurve_eez %>% 
    group_by(price) %>% 
    summarize(tot_mt_yr=sum(tot_mt_yr, na.rm=T))

  # Plot bivalve curve
  g <- ggplot(b_scurve, aes(x=tot_mt_yr/1E6, y=price)) +
    geom_line() + 
    geom_hline(yintercept=f_curr_usd_mt, lty=3) +
    # geom_vline(xintercept=30*1E6/1E9) +
    theme_bw() + 
    labs(x="Production (millions of MT)", y="Cost of mussels (USD / mt)", title=paste0("Bivalve - ", cost_scalar_do, " cost scalar"))
  print(g)

  # Export results
  outfile <- paste0("AQ_bivalve_w_eez_", format(cost_scalar_do, nsmall=2), "catch_scalar", ".Rdata")
  save(b_scurve, b_scurve_eez, 
       file=file.path(datadir, outfile))
  
}


