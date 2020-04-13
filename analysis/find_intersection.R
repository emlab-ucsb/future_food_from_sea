## -----------------------------------
## find intersecting points
## ------------------------------------

library(tidyverse)
library(reconPlots)
library(tictoc)
# library(furrr)


## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

## supply curves
data <- read_rds(paste0(bp_path, "outputs/all_supply_curves.rds"))

## demand
demand_df <- read_rds(paste0(bp_path,  "outputs/demand_curves_final.rds"))

## create a dataframe for matching later
id_df <- data %>%
  filter(price == 5) %>%
  select(scenario2, scenario, inl_scen:aq_cost_scalar) %>%
  unique()

saveRDS(id_df, paste0(bp_path, "outputs/ss_analysis/id_df.rds"))


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

saveRDS(bivalve_prod_vals, paste0(bp_path, "outputs/ss_analysis/bivalve_intersects.rds"))
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

saveRDS(ff_prod_vals, paste0(bp_path, "outputs/ss_analysis/ff_intersects.rds"))
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

saveRDS(inland_prod_vals, paste0(bp_path, "outputs/ss_analysis/inland_intersects.rds"))
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

saveRDS(capture_prod_vals, paste0(bp_path, "outputs/ss_analysis/capture_intersects.rds"))
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

saveRDS(ps_prod_vals, paste0(bp_path, "outputs/ss_analysis/ps_all_intersects.rds"))
toc()

# ### now find sector specific outputs
# ## this will be a function
# 
# intersect_df <- indiv_prod_vals %>%
#   select(scen_lab = scenario, demand_scen, price, total_meat_yr = meat_yr) %>%
#   mutate(price = round(price),
#          total_meat_yr = total_meat_yr / 1e6)
# 
# supply_df <- data %>%
#   mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
#          sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
#                          ifelse(sector == "Inland", "Inland fisheries", sector)),
#          scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
#                            ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
#                                   ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen))))
# 
# 
# zeros_df <- expand_grid(scenario2 = unique(supply_df$scenario2),
#                         price = seq(0, 19999, 1))
# 
# supply_all_p_df <- supply_df %>%
#   select(scenario2:sector) %>%
#   unique() %>%
#   left_join(zeros_df) %>%
#   left_join(supply_df) %>%
#   mutate(meat_yr = ifelse(is.na(meat_yr), 0, meat_yr)) %>%
#   mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
#          sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
#                          ifelse(sector == "Inland", "Inland fisheries", sector)),
#          scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
#                            ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
#                                   ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen))))
# 
# 
# 
# sector_sup_df <- supply_all_p_df %>%
#   left_join(intersect_df) %>%
#   filter(!is.na(total_meat_yr)) %>%
#   select(scenario2:aq_scen, scen_lab, aq_cost_scalar, sector, demand_scen, price, sector_meat_yr = meat_yr, total_meat_yr) %>%
#   mutate(sector_meat_yr = sector_meat_yr / 1e6)
# 
# ## table for si
# 
# si_sub_table <- sector_sup_df %>%
#   select(sector:sector_meat_yr) %>%
#   pivot_wider(names_from = sector, values_from = sector_meat_yr) 
# 
# 
# 
# 
# 
# 

# data2 <- data %>% 
#   mutate(scenario_title=plyr::revalue(scenario, c("Scenario 1"="Scenario 1:\nFeed from by-products only",
#                                                   "Scenario 2"="Scenario 2:\nFeed from by-products + whole fish",
#                                                   "Scenario 3a"="Scenario 3a:\nFeed from by-products + whole fish\n(+50% reduction in FM/FO demand)",
#                                                   "Scenario 3b"="Scenario 3b:\nFeed from by-products + whole fish\n(+75% reduction in FM/FO demand)",
#                                                   "Scenario 3c"="Scenario 3c:\nFeed from by-products + whole fish\n(+95% reduction in FM/FO demand)",
#                                                   "Scenario 4"="Scenario 4:\nFeed unconstrained\nby capture fisheries")))
# 
# ## nature paper figure 4 scenario
# np_scen_vec <- c("Scenario 3c")
# 
# ## get scenarios that we are interested in
# data_np <- data2 %>%
#   filter(scenario %in% np_scen_vec) %>%
#   mutate(sector = str_replace(sector, "aquaculture", "mariculture")) %>%
#   mutate(scen_lab = ifelse(scenario == "Scenario 2", "Policy reforms",
#                            ifelse(scenario == "Scenario 3a", "Tech innovation",
#                                   ifelse(scenario == "Scenario 3c", "Tech innovation (Ambitious)", "Scenario D")))) %>%
#   filter(sector != "Aggregate",
#          inl_scen == "constant_cap")
# 
# ## individual supply and demand
# ## --------------------------------------------------
# 
# indiv_supply <- data_np %>%
#   mutate(sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
#                          ifelse(sector == "Inland", "Inland fisheries", sector)))
# 
# indiv_demand <- demand_df %>%
#   filter(sector %in% c("marine_capture", "inland_production", "finfish_mari_a", "bivalve_mariculture")) %>%
#   mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
#                          ifelse(sector == "inland_production", "Inland fisheries",
#                                 ifelse(sector == "finfish_mari_a", "Finfish mariculture", "Bivalve mariculture"))))
# 
# find_sd_point_indiv <- function(scen, sdf, ddf) {
#   
#   ## define the price and sector curve
#   demand <- scen$dval
#   supply_scen <- scen$sscen
#   prod_sector <- scen$sector_val
#   
#   ## supply df
#   scdf <- sdf %>%
#     filter(scenario == supply_scen,
#            sector == prod_sector) %>%
#     mutate(x = meat_yr,
#            y = price)
#   
#   ## demand df
#   dcdf <- ddf %>%
#     filter(sector == prod_sector,
#            demand_scen == demand) %>%
#     mutate(x = quantity,
#            y = price)
#   
#   intersect <- try({curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)})
#   if (inherits(intersect, "try-error")){
#     intersect <- list(y = 0, x = 0)
#   } else {
#     intersect <- curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)
#   }
#   
#   
#   output <- tibble(scenario = supply_scen,
#                    sector = prod_sector,
#                    demand_scen = demand,
#                    price = intersect$y,
#                    meat_yr = intersect$x)
#   
# }
# 
# ## make df of all intersection points
# demand_vec <- as.list(unique(indiv_demand$demand_scen))
# # demand_vec <- as.list(c("extreme"))
# supply_vec <- as.list(unique(indiv_supply$scenario))
# # supply_vec <- as.list(c("Scenario 2"))
# sector_names <- as.list(unique(indiv_supply$sector))
# # sector_names <- as.list(c("Capture fisheries"))
# 
# indiv_prod_vals <- cross(list(dval = demand_vec,
#                               sscen = supply_vec,
#                               sector_val = sector_names)) %>%
#   map(find_sd_point_indiv,
#       sdf = indiv_supply, 
#       ddf = indiv_demand) %>%
#   bind_rows()
