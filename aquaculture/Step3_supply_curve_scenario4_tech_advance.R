
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

# Tech scenarios
tech_scen <- c("a", "b", "c")

# Cost scalars
cost_scalars <- c(0.75, 1, 1.5, 2)

# Prices to evaluate
# prices <- 0:20000
prices <- seq(0,20000,1)

# Build key
scenario_key <- expand.grid(spp_num=spp_num, cap_scen=cap_scen, tech_scen=tech_scen, cost_scalar=cost_scalars)


# Old method
#####################################################################################

# Loop through scenarios
i <- 1
j <- 4
k <- 3
l <- 2
for(i in 1:length(spp_num)){
  for(j in 1:length(cap_scen)){
    for(k in 1:length(tech_scen)){
      for(l in 1:length(cost_scalars)){
        
        # Scenario
        spp_do <- spp_num[i]
        cap_scen_do <- cap_scen[j]
        tech_scen_do <- tech_scen[k]
        cost_scalar_do <- cost_scalars[l]
        print(paste(spp_do, cap_scen_do, tech_scen_do, cost_scalar_do, sep=" - "))
        
        # Loop through prices
        x <- 10000
        f_scurve <- purrr::map_df(prices, function(x) {
        
          # Price to evaluate and associated FM constraint
          usd_mt <- x
          feed_avail_mt <- feed %>% 
            filter(scenario==cap_scen_do & price==usd_mt) %>% 
            pull(paste0("feed_mt_yr_combo_scen3", tech_scen_do, "_spp", spp_do))
        
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
          labs(x="Production (millions mt)", y="Price (USD)", 
               title=paste0("Fish ", spp_do, " - ", cap_scen_do, " - ", tech_scen_do, " - ", cost_scalar_do, " cost scalar"))
        print(g)
        
        # Plot fishmeal used
        g <- ggplot(f_scurve, aes(x=price, y=feed_mt_yr)) +
          geom_line() +
          theme_bw()
        print(g)
        
        # Export results
        outfile <- paste0("AQ_finfish_species", spp_do, "_scenario4_tech_advance_", tech_scen_do, "_",    
                          cap_scen_do, "_", format(cost_scalar_do, nsmall=2), "catch_scalar", ".Rdata")
        save(f_scurve, file=file.path(datadir, outfile))
        
      }
    }
  }
}
  

# New method
#####################################################################################

# # Loop through scenarios
# x <- 1
# data <- purrr::map_df(1:nrow(scenario_key), function(x){
#   
#   # Scenario
#   spp_do <- scenario_key$spp_num[x]
#   cap_scen_do <- scenario_key$cap_scen[x]
#   tech_scen_do <- scenario_key$tech_scen[x]
#   cost_scalar_do <- scenario_key$cost_scalar[x]
#   
#   # Loop through prices
#   x <- 10000
#   f_scurve <- purrr::map_df(prices, function(p) {
#     
#     # Price to evaluate and associated FM constraint
#     usd_mt <- p
#     if(tech_scen_do=="a"){feed_avail_mt <- feed$feed_mt_yr_combo_scen3a[feed$scenario==cap_scen_do & feed$price==usd_mt]}
#     if(tech_scen_do=="b"){feed_avail_mt <- feed$feed_mt_yr_combo_scen3b[feed$scenario==cap_scen_do & feed$price==usd_mt]}
#     if(tech_scen_do=="c"){feed_avail_mt <- feed$feed_mt_yr_combo_scen3c[feed$scenario==cap_scen_do & feed$price==usd_mt]}
#     
#     # Calculate and order by profits, cut off cells once feed is exceeded
#     results <- data %>%
#       # Identify cost and feed columns of species being evaluated
#       rename(f_usd_per_mt_do=paste0("f_usd_per_mt", spp_do),
#              feed_mt_yr_do=paste0("feed_mt_yr", spp_do)) %>% 
#       # Calculate and order by profits
#       # Scale cost column of species being evaluated
#       select(cell, f_mt_yr, f_usd_per_mt_do, feed_mt_yr_do) %>%
#       mutate(f_usd_per_mt_do_scaled=f_usd_per_mt_do*cost_scalar_do,
#              price=usd_mt,
#              feed_avail_mt=feed_avail_mt,
#              f_profits_yr=calc_profits(q=f_mt_yr, p=usd_mt, c=f_usd_per_mt_do_scaled)) %>% 
#       filter(f_profits_yr>0) %>%
#       arrange(desc(f_profits_yr)) %>%
#       # Calculate feed demand of each additional cell
#       mutate(feed_mt_sum=cumsum(feed_mt_yr_do)) %>%
#       # Remove sites that would lead to excedance of feed budget
#       filter(feed_mt_sum<=feed_avail_mt) %>%
#       # Calculate total production at price
#       group_by(price, feed_avail_mt) %>%
#       summarize(tot_mt_yr=sum(f_mt_yr, na.rm=T),
#                 feed_mt_yr=sum(feed_mt_yr_do, na.rm=T)) %>% 
#       ungroup()
#     
#     # Return results
#     return(results)
#     
#   })
#   
#   # Plot finfish curve
#   g <- ggplot(arrange(f_scurve, price), aes(x=tot_mt_yr/1E6, y=price)) +
#     geom_line() + 
#     geom_hline(yintercept=f_curr_usd_mt, lty=3) +
#     # geom_vline(xintercept=30*1E6/1E9) +
#     theme_bw() + 
#     labs(x="Production (millions mt)", y="Price (USD)", 
#          title=paste0("Fish ", spp_do, " - ", cap_scen_do, " - ", tech_scen_do, " - ", cost_scalar_do, " cost scalar"))
#   g
#   
#   # Plot fishmeal used
#   g <- ggplot(f_scurve, aes(x=price, y=feed_mt_yr)) +
#     geom_line() +
#     theme_bw()
#   g
#   
#   # Format supply curve
#   scurve_out <- f_scurve %>% 
#     mutate(cap_scen=cap_scen_do, aq_spp=spp_do, aq_scen=paste0("Scenario 4", tech_scen_do), aq_cost_scalar=cost_scalar_do) %>% 
#     select(cap_scen, aq_scen, aq_spp, aq_cost_scalar, everything())
#   
# })
# 
# # Salmon
# g <- ggplot(data %>% 
#               filter(aq_spp==1), aes(x=tot_mt_yr/1e6, y=price, group=aq_cost_scalar, color=aq_cost_scalar)) +
#   geom_line() +
#   facet_wrap(aq_scen ~ cap_scen) +
#   labs(x="Total production (millions mt)", y="Price (USD/mt)") +
#   theme_bw()
# g
