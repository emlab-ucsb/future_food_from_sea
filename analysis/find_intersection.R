

## future food from the sea
## note to user: must run scripts in init_cond, capture_fisheries, and aquaculture folder first (outputs needed).
## this creates information needed for SI



## -----------------------------------
## find intersecting points
## ------------------------------------

library(tidyverse)
library(reconPlots)
library(tictoc)


## supply curves
## note to user: change path, read in file created in all_supply_curves.R
data <- read_rds("all_supply_curves.rds")

## demand
## note to user: change path, read in file created in demand_curves
demand_df <- read_rds("demand_curves_final.rds")

## create a dataframe for matching later
id_df <- data %>%
  filter(price == 5) %>%
  select(scenario2, scenario, inl_scen:aq_cost_scalar) %>%
  unique()

## change path, save ids
saveRDS(id_df, "id_df.rds")


## -----------------------------------
## find intersecting points
## ------------------------------------

find_sd_point_indiv <- function(scen, sdf, ddf) {
  
  ## define the price and sector curve
  demand <- scen$dval
  id_scen <- scen$idval
  prod_sector <- scen$sector_val
  
  ## supply df
  scdf <- sdf %>%
    filter(scenario2 == id_scen,
           sector == prod_sector) %>%
    mutate(x = meat_yr,
           y = price)
  
  ## demand df
  dcdf <- ddf %>%
    filter(sector == prod_sector,
           demand_scen == demand) %>%
    mutate(x = quantity,
           y = price)
  
  intersect <- try({curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)})
  if (inherits(intersect, "try-error")){
    intersect <- list(y = NA, x = NA)
  } else {
    intersect <- curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)
  }
  
  output <- tibble(scenario2 = id_scen,
                   sector = prod_sector,
                   demand_scen = demand,
                   price = intersect$y,
                   meat_yr = intersect$x)
  
  # saveRDS(output, paste0(bp_path, "outputs/ss_analysis/", id_scen, " ", prod_sector, " ", demand, ".rds"))
  
}


## set up demand df 
demand_df2 <- demand_df %>%
  rename(sector2 = sector) %>%
  filter(sector2 != "aggregate_marine") %>%
  mutate(sector = ifelse(sector2 == "marine_capture", "Capture fisheries",
                         ifelse(sector2 == "inland_production", "Inland",
                                ifelse(sector2 == "bivalve_mariculture", "Bivalve aquaculture",
                                       ifelse(sector2 == "aggregate_lb", "perf_substitutes", "Finfish aquaculture")))))


## make df of all intersection points
demand_vec <- as.list(unique(demand_df2$demand_scen))
id_vec <- as.list(unique(data$scenario2))

# 
# ss_sector_names <- as.list(c("Capture fisheries", "Bivalve aquaculture", "Finfish aquaculture", "Inland"))
# ps_sector_names <- as.list(c("perf_substitutes"))

## getting ready to run 
# plan(multiprocess, workers = 2)



##----------------------------------
## separate sectors
##----------------------------------

## bivalve intersection points
bivalve_supply <- data %>% filter(sector == "Bivalve aquaculture")
bivalve_demand <- demand_df2 %>% filter(sector == "Bivalve aquaculture")

tic()
bivalve_prod_vals <- cross(list(dval = demand_vec,
                               idval = id_vec,
                               sector_val = "Bivalve aquaculture")) %>%
  map_df(find_sd_point_indiv,
         sdf = bivalve_supply, 
         ddf = bivalve_demand) %>%
  bind_rows()

## note to user: change path, save outputs
saveRDS(bivalve_prod_vals, "bivalve_intersects.rds")
toc()

## finfish intersection points
ff_supply <- data %>% filter(sector == "Finfish aquaculture")
ff_demand <- demand_df2 %>% filter(sector == "Finfish aquaculture")

tic()
ff_prod_vals <- cross(list(dval = demand_vec,
                           idval = id_vec,
                           sector_val = "Finfish aquaculture")) %>%
  map_df(find_sd_point_indiv,
         sdf = ff_supply, 
         ddf = ff_demand) %>%
  bind_rows()

## note to user: change path, save outputs
saveRDS(ff_prod_vals, "ff_intersects.rds")
toc()

## inland intersection points
inland_supply <- data %>% filter(sector == "Inland")
inland_demand <- demand_df2 %>% filter(sector == "Inland")

tic()
inland_prod_vals <- cross(list(dval = demand_vec,
                               idval = id_vec,
                               sector_val = "Inland")) %>%
  map_df(find_sd_point_indiv,
         sdf = inland_supply, 
         ddf = inland_demand) %>%
  bind_rows()

## note to user: change path, save outputs
saveRDS(inland_prod_vals, "inland_intersects.rds")
toc()


## capture intersection points
capture_supply <- data %>% filter(sector == "Capture fisheries")
capture_demand <- demand_df2 %>% filter(sector == "Capture fisheries")

tic()
capture_prod_vals <- cross(list(dval = demand_vec,
                                idval = id_vec,
                                sector_val = "Capture fisheries")) %>%
  map_df(find_sd_point_indiv,
         sdf = capture_supply, 
         ddf = capture_demand) 

## note to user: change path, save outputs
saveRDS(capture_prod_vals, "capture_intersects.rds")
toc()





##----------------------------------
## Perfect substitutes
##----------------------------------

ps_supply <- data %>% filter(sector == "perf_substitutes")
ps_demand <- demand_df2 %>% filter(sector == "perf_substitutes")

tic()
ps_prod_vals <- cross(list(dval = demand_vec,
                           idval = id_vec,
                           sector_val = "perf_substitutes")) %>%
  map_df(find_sd_point_indiv,
         sdf = ps_supply, 
         ddf = ps_demand) 

## note to user: change path, save outputs
saveRDS(ps_prod_vals, "ps_all_intersects.rds")
toc()

