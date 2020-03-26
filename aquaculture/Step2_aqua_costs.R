
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(ggridges)

# Directories
indir <- "aquaculture/aquaculture_v1/results"
outdir <- "aquaculture/aquaculture_v3/output"
plotdir <- "aquaculture/aquaculture_v3/figures"

# Read data
data <- read.csv(file = file.path(indir, 'aqua_production_results.csv'))


# Distance adjustments
#####################################################################################

# Exclude farms outside an EEZ
# Cap distance from shore at 200 nautical miles (370.4 kilometers) since all farms should in theory be within EEZs now
data <- data %>% 
  filter(!is.na(eez)) %>% 
  mutate(distance_nm = distance_km * 0.539957, # nautical miles
         distance_nm = ifelse(distance_nm > 200, 200, distance_nm), # cap distance for cells in EEZs that have suspect distances (a lot of small island countries)
         distance_km = ifelse(distance_km > 370.4, 370.4, distance_km)) %>%  # cap distance 
  dplyr::select(cell:distance_km, distance_nm, everything())


# Cost parameters
#####################################################################################

# General cost parameters
##############################################

# Discount rate
disc_rate <- 0.10

# Annuity function
annuity <- function(c, r = 0.1, t = 10) {
  a <- c / ((1-(1+r)^-t)/r)
  return(a)
}

# Labor cost parameters
# From Lester et al. 2018
worker_n <- 8 # 8 workers per farm
worker_hrs <- 2080 # 40 hour weeks x 52 weeks / year = 2080 hours / year (does not include transport time)

# Fuel cost parameters
# From Lester et al. 2018
vessel_n <- 2
vessel_lph <- 60.6 # vessel fuel efficiency, liters per hour (from 16 g/hr in Lester et al. 2018)
vessel_kmh <- 12.9 # vessel speed, km per hour (from 8 mph in Lester et al. 2018)
vessel_trips_yr <- 416 # two identical vessels - one making 5 trips/week and the other 3 trips/week (from Lester et al. 2018)

# Bivalve cost parameters
##############################################

# Bivalve lines 
line_n <- 100 # 100 lines per farm (from Gentry et al. farm design)
line_km <- 4 # 4,000 meters per line (from Gentry et al. farm design)

# Bivalve capital costs
# From NOAA 2008
ll_usd <- 10000
ll_life_yr <- 10
vessel_usd <- 95000
vessel_life_yr <- 30

# Amortized bivalve capital cost
vessel_cap_cost_yr <- annuity(c=vessel_usd*vessel_n, r=disc_rate, t=vessel_life_yr) 
ll_cap_cost_yr <- annuity(c=ll_usd*line_n, r=disc_rate, t=ll_life_yr) 
b_cap_cost_yr <- vessel_cap_cost_yr + ll_cap_cost_yr # total

# Bivalve operating costs
# From NOAA 2008
misc_supp_usd <- 1700
vessel_upkeep_usd <-  10000 + 5000
onshore_usd <- 173000
b_opp_cost_yr <- misc_supp_usd + vessel_upkeep_usd * vessel_n + onshore_usd # total

# Finfish cost parameters
##############################################

# Finfish cages
cage_n <- 24 # 24 cages per farm (from Gentry et al. farm design)
cage_m3 <- 9000 # 9,000 m3 per cage (from Gentry et al. farm design)
cage_m3_tot <- cage_n * cage_m3

# Fingerling costs
# Lifespand is determined by time to reach 35 cm so annuity happens below
juv_m3 <- 20 # Gentry et al. farm design
juv_farm <- juv_m3 * cage_m3_tot
usd_juv <- 0.85 # NOAA 2008
juv_cost <- juv_farm * usd_juv

# Feed cost parameters
feed_usd_kg <- 2

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

# Finfish capital costs
# From NOAA 2008
cage_usd_m3 <- 15
cage_install_usd_m3 <- 3
cage_life_yr <- 10
f_cap_cost_yr <- annuity(c=(cage_m3_tot*cage_usd_m3)+(cage_m3_tot*cage_install_usd_m3), r=disc_rate, t=cage_life_yr)

# Finfish operating costs
# From NOAA 2008
cage_maintain_usd_m3_yr <- 1
vessel_usd_yr <- 100000
onshore_usd_yr <- 150000
insurance_usd_yr <- 50000
f_opp_cost_yr <- vessel_usd_yr + onshore_usd_yr + insurance_usd_yr + (cage_maintain_usd_m3_yr * cage_m3_tot)


# Cost calculations
#####################################################################################

# Add costs
costs <- data %>% 
  # Add labor costs
  mutate(wage_usd_hr = pc_income_usd/(40*52),
         hours_per_trip = 2 * distance_km / vessel_kmh,
         wages_fixed = worker_n * worker_hrs * wage_usd_hr,
         wages_transport = worker_n * hours_per_trip * vessel_trips_yr * wage_usd_hr,
         wages_total = wages_fixed + wages_transport,
         f_labor_cost = f_suitable * wages_total,
         b_labor_cost = b_suitable * wages_total) %>% 
  # Add fuel costs
  mutate(fuel_l_trip = hours_per_trip * vessel_lph,
         fuel_usd_trip = fuel_l_trip * fuel_usd_l,
         fuel_usd_total = fuel_usd_trip * vessel_trips_yr,
         f_fuel_cost = f_suitable * fuel_usd_total,
         b_fuel_cost = b_suitable * fuel_usd_total) %>% 
  # Add fingerling cost
  mutate(f_juv_cost=annuity(c=juv_cost, r=disc_rate, t=f_yrs)) %>% 
  # Add feed costs 
  mutate(# Salmon
         feed_mt_yr1 = fcr1 * f_mt_yr,
         fm_mt_yr1 = feed_mt_yr1 * feed_fm_perc1,
         fo_mt_yr1 = feed_mt_yr1 * feed_fo_perc1,
         # Milkfish
         feed_mt_yr2 = fcr2 * f_mt_yr,
         fm_mt_yr2 = feed_mt_yr2 * feed_fm_perc2,
         fo_mt_yr2 = feed_mt_yr2 * feed_fo_perc2,
         # Barramundi
         feed_mt_yr3 = fcr3 * f_mt_yr,
         fm_mt_yr3 = feed_mt_yr3 * feed_fm_perc3,
         fo_mt_yr3 = feed_mt_yr3 * feed_fo_perc3,
         # doesn't have to be multiplied by n suitable sites b/c this is feed associated w/ production in cell
         f_feed_cost1 = feed_mt_yr1 * 1000 * feed_usd_kg,
         f_feed_cost2 = feed_mt_yr2 * 1000 * feed_usd_kg,
         f_feed_cost3 = feed_mt_yr3 * 1000 * feed_usd_kg) %>% # convert feed to kg to match units
  # Calculate amoretized capital and annual operating costs
  mutate(b_cap_cost = b_suitable * b_cap_cost_yr,
         f_cap_cost = f_suitable * f_cap_cost_yr,
         b_opp_cost = b_suitable * b_opp_cost_yr,
         f_opp_cost = f_suitable * f_opp_cost_yr) %>% 
  # Calculate total costs
  mutate(f_total_cost1 = f_labor_cost + f_fuel_cost + f_feed_cost1 + f_juv_cost + f_cap_cost + f_opp_cost,
         f_total_cost2 = f_labor_cost + f_fuel_cost + f_feed_cost2 + f_juv_cost + f_cap_cost + f_opp_cost,
         f_total_cost3 = f_labor_cost + f_fuel_cost + f_feed_cost3 + f_juv_cost + f_cap_cost + f_opp_cost,
         b_total_cost = b_labor_cost + b_fuel_cost + b_cap_cost + b_opp_cost) %>% 
  # Calculate cost per ton
  mutate(f_usd_per_mt1 = f_total_cost1 / f_mt_yr,
         f_usd_per_mt2 = f_total_cost2 / f_mt_yr,
         f_usd_per_mt3 = f_total_cost3 / f_mt_yr,
         b_usd_per_mt = b_total_cost / b_mt_yr)



# Inspect costs
#####################################################################################

# Calculate proportional source
f_cost_source <- costs %>% 
  filter(!is.na(f_total_cost1)) %>% 
  select(f_total_cost1, f_labor_cost, f_fuel_cost, f_feed_cost1, f_juv_cost, f_cap_cost, f_opp_cost) %>% 
  mutate_at(vars(-c(f_total_cost1)), funs(. / f_total_cost1)) %>% 
  select(-f_total_cost1) %>% 
  tidyr::gather(key=type, value=cost) %>% 
  mutate(type=plyr::revalue(type, c("f_opp_cost"="Operating costs",
                              "f_labor_cost"="Labor costs",
                              "f_fuel_cost"="Fuel costs",
                              "f_feed_cost1"="Feed costs",
                              "f_juv_cost"="Fingerling costs",
                              "f_cap_cost"="Amortized captital costs")))

# Summarize proportional source
f_cost_sum <- f_cost_source %>% 
  group_by(type) %>% 
  summarize(prop=median(cost))
  
# Plot cost source
g <- ggplot(f_cost_source, aes(x=cost, y=type, fill=type)) +
  geom_density_ridges() +
  xlab("Proportion of total costs") + ylab("") +
  scale_fill_discrete(name="Cost type") +
  theme_bw()
g


# Plot cost per ton
#####################################################################################

# Current mean cost
g_curr_usd_mt <- 1296 # global average from Tracey
b_curr_usd_mt <- 1700 # 1.5 EURO/kg = 1.7 USD/kg =
f_curr_usd_mt <- 7000 # 60 NOK/kg = 7 USD/kg = Norwegian aquaculture analysis 2017

# Reshape for plotting
cell_costs <- costs %>% 
  select(cell, b_usd_per_mt, f_usd_per_mt1, f_usd_per_mt2, f_usd_per_mt3) %>% 
  tidyr::gather(type, cost, 2:ncol(.)) %>% 
  mutate(type=recode_factor(type, "b_usd_per_mt"="Bivales",
                                  "f_usd_per_mt1"="Finfish (salmon-like)",
                                  "f_usd_per_mt2"="Finfish (milkfish-like)",
                                  "f_usd_per_mt3"="Finfish (barramundi-like)")) %>% 
  filter(!is.na(cost))

# Plot cost per cell
g <- ggplot(data=cell_costs, aes(y=cost, x=type)) +
  geom_boxplot() + 
  ylim(0, 20000) +
  ylab("Cost per metric ton ($USD)") +
  geom_hline(yintercept=c(b_curr_usd_mt, f_curr_usd_mt)) +
  theme_bw()
g
# ggsave(file.path(plotdir, "cost_per_mt_boxplots.png"), height=4, width=5, units="in", dpi=600)


# Export
#####################################################################################

# Export results
saveRDS(costs, file=file.path(outdir, "aqua_cost_per_production_results.Rds"))


