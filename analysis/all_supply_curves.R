## Tracey Mangin
## March 11, 2020
## create file with all supply curves

## note to user: nmust run scripts in init_cond, capture_fisheries, and aquaculture folder first (outputs needed).
## this creates a file with all supply curves (inland, marine capture, mariculture)

library(tidyverse)

## marine supply curves
## note to user: change path, read in output from aquaculture WC_AQ_supply_curve_merged_all_scenarios.Rds
data <- read_rds("WC_AQ_supply_curve_merged_all_scenarios.Rds")

## scenario = unique identifier for scenario
## cap_scen = capture fisheries scenario
## aq_spp = aquaculture species (1=salmon-like, 2=milkfisk-like, 3=barramundi-like)
## aq_scen = aquaculture scenario (A = 50% reduction, B = 75% reduction, C = 95% reduction)
## aq_cost_scalar = aquaculture cost scalar (0.75, 1.00, 1.50, 2.00)
## sector = capture, bivalve aq, finish aq
## price = USD/mt
## dhc_mt_yr = harvest (live weight) accounting for reduction fisheries (ie, 18% of total harvest removed)
## meat_yr = edible meat production (mt/yr)
                       
## inland production
## note to user: change the following paths and load inland fishery files.
## these files are created in the inland_supply_code_data folder.

inland_sc <- read_csv("inland_supply_dat.csv") %>%
  select(price = prices, food_mt = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "constant_cap")

inland_sc_h <- read_csv("inland_supply_dat_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "constant_cap") %>%
  left_join(inland_sc) %>%
  select(sector, inl_scen, price, harvest, food_mt)

##
inland_lg <- read_csv("inland_supply_dat.csv") %>%
  select(price = prices, food_mt = total_inland_supply_const_capt) %>%
  mutate(sector = "Inland",
         inl_scen = "low_growth")

inland_lg_h <- read_csv("inland_supply_dat_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_const_capt) %>%
  mutate(sector = "Inland",
         inl_scen = "low_growth") %>%
  left_join(inland_lg) %>%
  select(sector, inl_scen, price, harvest, food_mt)

##
inland2 <- read_csv("inland_supply_dat_low_elast.csv") %>%
  select(price = prices, food_mt = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "low_elast")

inland2_h <- read_csv("inland_supply_dat_low_elast_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "low_elast") %>%
  left_join(inland2) %>%
  select(sector, inl_scen, price, harvest, food_mt)

##
inland3 <- read_csv("inland_supply_dat_high_elast.csv") %>%
  select(price = prices, food_mt = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "high_elast")

inland3_h <- read_csv("inland_supply_dat_high_elast_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "high_elast") %>%
  left_join(inland3) %>%
  select(sector, inl_scen, price, harvest, food_mt)

##
inland4 <- read_csv("inland_supply_dat_high growth.csv") %>%
  select(price = prices, food_mt = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "high_growth")

inland4_h <- read_csv("inland_supply_dat_high growth_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_2050_constcapt) %>%
  mutate(sector = "Inland",
         inl_scen = "high_growth") %>%
  left_join(inland4) %>%
  select(sector, inl_scen, price, harvest, food_mt)


##
inland_pr_df <- read_csv("inland_supply_dat.csv") %>%
  select(price = prices, food_mt = total_inland_supply_2050_prc) %>%
  mutate(sector = "Inland",
         inl_scen = "price_responsive") 

inland_pr_df_h <- read_csv("inland_supply_dat_harvest.csv") %>%
  select(price = prices, harvest = total_inland_supply_2050_prc) %>%
  mutate(sector = "Inland",
         inl_scen = "price_responsive") %>%
  left_join(inland_pr_df) %>%
  select(sector, inl_scen, price, harvest, food_mt)

inland_sc_df <- rbind(inland_sc_h, inland_lg_h, inland2_h, inland3_h, inland4_h, inland_pr_df_h) %>%
  rename(mt_yr = harvest, meat_yr = food_mt) %>%
  mutate(dhc_mt_yr = mt_yr) %>%
  select(inl_scen, sector, price, mt_yr, dhc_mt_yr, meat_yr) 


### 
data2 <- expand_grid(scenario = unique(data$scenario),
                     inl_scen = unique(inland_sc_df$inl_scen)) %>%
  left_join(data)


## category ids for joining
scen_names_df <- data %>%
  select(scenario:aq_cost_scalar) %>%
  unique()

## add all senarios
inland_sc_df2 <- expand_grid(scenario = unique(data$scenario),
                             inl_scen = unique(inland_sc_df$inl_scen)) %>%
  left_join(scen_names_df) %>%
  left_join(inland_sc_df)

## prod outputs
data_all_ind <- rbind(data2, inland_sc_df2) %>%
  mutate(scenario2 = paste(inl_scen, scenario)) %>%
  select(scenario, scenario2, inl_scen:meat_yr)

data_all_agg <- data_all_ind %>%
  group_by(scenario2, scenario, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, price) %>%
  summarise(sector = "perf_substitutes",
            mt_yr = sum(mt_yr),
            dhc_mt_yr = sum(dhc_mt_yr),
            meat_yr = sum(meat_yr)) %>%
  ungroup() %>%
  filter(price <= max(inland_sc_df2$price))

## update path, save file. use in other scripts.
saveRDS(data_all_agg, "perf_sub_supply_curves.rds")

## put it all together and save
data_all <- rbind(data_all_ind, data_all_agg)

## update path, save file. use in other scripts.
saveRDS(data_all, "all_supply_curves.rds")

## save smaller version for main text
data_sub <- data_all %>%
  filter(inl_scen == "constant_cap",
         cap_scen == "original_inputs",
         aq_spp == 1,
         aq_cost_scalar == 1,
         aq_scen %in% c("Scenario 3 - byproducts + directed", "Scenario 4a - tech advance", "Scenario 4b - tech advance", "Scenario 4c - tech advance"))

## update path, save file. use in other scripts.
saveRDS(data_sub, "subset_supply_curves.rds")




