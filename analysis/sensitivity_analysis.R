## Tracey Mangin
## March 13, 2020
## Finalize sensitivity analyses

## note to user: this creates information needed for sensitivity analysis


library(tidyverse)


## read in id df
## note to user: change path and load file created in find_intersection.R
id_df <- read_rds("id_df.rds")

## demand
## for trouble shooting
## note to user: change path and load file created in demand_curves.R
demand_df <- read_rds(paste0(bp_path,  "outputs/demand_curves_final.rds"))

## supply df
## note to user: change path and load file created in all_supply_curves.rds
supply_df <- read_rds(paste0(bp_path, "outputs/all_supply_curves.rds")) 

supply_indiv_sect <- supply_df %>%
  filter(sector != "perf_substitutes")

## read in the indivual sector outputs
## note to user: change path and load files created in find_intersection.R
bivalve_pts_df <- read_rds("bivalve_intersects.rds")
inland_pts_df <- read_rds("inland_intersects.rds")
finfish_pts_df <- read_rds("ff_intersects.rds")
capture_pts_df <- read_rds("capture_intersects.rds")

## read in the perfect substitutes outputs
ps_df <- read_rds("ps_all_intersects.rds")

## fill in the missing pieces for capture fisheries
## ----------------------------------------------------

## find max production  for each capture scenario
max_prod_capt <- supply_indiv_sect %>%
  filter(inl_scen == "constant_cap",
         aq_spp == 1,
         aq_scen == "Scenario 4c - tech advance",
         aq_cost_scalar == 1,
         sector == "Capture fisheries") %>%
  select(cap_scen, price, mt_yr, meat_yr) %>%
  group_by(cap_scen) %>%
  filter(price == max(price)) %>%
  ungroup()
         
capture_pts_df2 <- capture_pts_df %>%
  left_join(id_df) %>%
  mutate(price = ifelse(is.na(price), max_prod_capt$price[match(cap_scen, max_prod_capt$cap_scen)], price),
         meat_yr = ifelse(is.na(meat_yr), max_prod_capt$meat_yr[match(cap_scen, max_prod_capt$cap_scen)], meat_yr)) %>%
  select(scenario2, sector, demand_scen, price, meat_yr)


## fill in the missing pieces for finfish mariculture
## ----------------------------------------------------

## find max production  for each capture scenario
max_prod_ff <- supply_indiv_sect %>%
  filter(sector == "Finfish aquaculture") %>%
  select(scenario2, price, mt_yr, meat_yr) %>%
  group_by(scenario2) %>%
  filter(price == max(price)) %>%
  ungroup()

finfish_pts_df2 <- finfish_pts_df %>%
  # left_join(id_df) %>%
  mutate(price = ifelse(is.na(price), max_prod_ff$price[match(scenario2, max_prod_ff$scenario2)], price),
         meat_yr = ifelse(is.na(meat_yr), max_prod_ff$meat_yr[match(scenario2, max_prod_ff$scenario2)], meat_yr)) %>%
  select(scenario2, sector, demand_scen, price, meat_yr)


## combine all individual supply curves
ss_df <- rbind(bivalve_pts_df, inland_pts_df, finfish_pts_df2, capture_pts_df2) %>%
  left_join(id_df) %>%
  mutate(sub_scen = "separate_sectors") %>%
  select(scenario2, scenario, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr)

ss_df_total <- ss_df %>%
  group_by(scenario2, demand_scen) %>%
  summarise(meat_yr = sum(meat_yr)) %>%
  ungroup() %>%
  mutate(sector = "Total",
         price = NA) %>%
  left_join(id_df) %>%
  mutate(sub_scen = "separate_sectors") %>%
  select(scenario2, scenario, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr)

ss_df_all <- rbind(ss_df, ss_df_total)


## PERF SUBSTITUTES
## ------------------------------------------------------------

zeros_df <- expand_grid(scenario2 = unique(id_df$scenario2),
                        sector = unique(supply_indiv_sect$sector),
                        price = seq(0, 19999, 1))

supply_all_p_df <- id_df %>%
  select(scenario2) %>%
  left_join(zeros_df) %>%
  left_join(supply_indiv_sect) %>%
  # left_join(id_df) %>%
  mutate(meat_yr = ifelse(is.na(meat_yr), 0, meat_yr)) %>%
  select(scenario2, sector, price, mt_yr, meat_yr) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)))


sector_sup_ps_df <- ps_df %>%
  select(scenario2, demand_scen, price) %>%
  mutate(price = round(price)) %>%
  left_join(supply_all_p_df) %>%
  left_join(id_df) %>%
  mutate(sub_scen = "perfect_substitutes") %>%
  select(scenario2, scenario, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr)

## total for binding
ps_df2 <- ps_df %>%
  left_join(id_df) %>%
  mutate(sector = "Total",
         sub_scen = "perfect_substitutes") %>%
  select(scenario2, scenario, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr)

## perfect substitutes df
perf_sub_df <- rbind(ps_df2, sector_sup_ps_df) %>%
  arrange(scenario2, demand_scen)

## all
## ------------------------------------

ss_df_all2 <- ss_df_all %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)))
  
sensi_df <- rbind(perf_sub_df, ss_df_all2) %>%
  arrange(sub_scen, scenario2, demand_scen) %>%
  mutate(meat_mmt_yr = meat_yr / 1e6)

## note to user: change path and save outputs for sensitivity analysis
write_csv(sensi_df, "all_si_outputs.csv")


