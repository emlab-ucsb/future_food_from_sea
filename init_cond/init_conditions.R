## Tracey Mangin
## December 19, 2019
## initial conditions

## This is STEP 3 in the init_cond folder.
## Run edible_conv.R first to create required file.

library(tidyverse)
library(janitor)


## note to user: update the path and load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F) 

## conversion factors
## note: this file is created in edible_conv.R
conv_vals <- read_csv("isscaap_food_conv_all.csv")

## fao historical production
hist_prod <- read_csv(paste0(bp_path, "project-materials/nature-revision/outputs/historical_prod.csv"))

## land based prod
inland_init <- read_csv(paste0(bp_path, "project-materials/nature-revision/outputs/inland_supply_info.csv"))

## fao hist prod by sp
hist_prod_sp <- read_csv(paste0(bp_path, "project-materials/nature-revision/outputs/historical_prod_x_sp.csv"))

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

## meat production
meat_prod <- read_csv(paste0(bp_path, "data/FAOSTAT_data_3-10-2020.csv"))

## init mari prod
init_mari_prod <- read_csv(paste0(bp_path, "project-materials/nature-revision/outputs/init_mari_prod.csv"))

## upsides 2012 production (harvest and food)
upsides_conv <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) %>%
  select(id_orig = IdOrig, SpeciesCat, Catch) %>%
  left_join(conv_vals) %>%
  mutate(food = Catch * red_scale_mult * convert_mult)

upsides_grp_prod <- upsides_conv %>%
  mutate(year = 2012) %>%
  group_by(year, seafood_type) %>%
  summarise(hsum = sum(Catch),
            fsum = sum(food)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_h = sum(hsum),
            total_f = sum(fsum)) %>%
  ungroup() %>%
  mutate(rel_h = hsum / total_h,
         rel_f = fsum / total_f)

upsides_prod <- upsides_conv %>%
  summarise(total_catch_up = sum(Catch),
            total_food_up = sum(food))
  
## fao historical production for 2017
fao_prod_2017 <- hist_prod %>%
  filter(year == 2017,
         sp_type != "aquatic_plant",
         measurement %in% c("food_mt", "harvest"),
         prod_type == "capture",
         location == "marine") %>%
  select(measurement, total_q) %>%
  pivot_wider(names_from = measurement, values_from = total_q) %>%
  rename(total_catch_fao = harvest, total_food_fao = food_mt)

## fao historical production for 2012
fao_prod <- hist_prod %>%
  filter(year == 2012,
         sp_type != "aquatic_plant",
         measurement %in% c("food_mt", "harvest"),
         prod_type == "capture",
         location == "marine") %>%
  select(measurement, total_q) %>%
  pivot_wider(names_from = measurement, values_from = total_q) %>%
  rename(total_catch_fao = harvest, total_food_fao = food_mt)

## cbind
prod_df <- cbind(upsides_prod, fao_prod) %>%
  mutate(rel_catch = total_catch_up / total_catch_fao, 
         rel_food = total_food_up / total_food_fao)

write_csv(prod_df, paste0(bp_path, "upside_fao_scale_df.csv"))

## scale capture fishery production
cf_scale_val <- prod_df %>%
  select(rel_catch) %>%
  as.numeric()

## initial marine capture production
mari_cap_prod <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) %>%
  select(id_orig = IdOrig, SpeciesCat, Catch) %>%
  left_join(conv_vals) %>%
  mutate(h_scl = Catch / cf_scale_val,
         food = h_scl * red_scale_mult * convert_mult)

mari_cap_init <- sum(mari_cap_prod$food)
mari_cap_inith <- sum(mari_cap_prod$h_scl)
mari_cap_inith_r <- sum(mari_cap_prod$h_scl) * red_scale_mult



## mariculture
## -----------------------
init_prod_feed_grp <- init_mari_prod %>%
  filter(year == max(init_mari_prod$year))

init_prod_fg <- init_prod_feed_grp %>%
  group_by(feed_grp) %>%
  summarise(totalh = sum(harvest_mmt),
            totalf = sum(food_mmt))


init_ff_food <- init_prod_feed_grp %>%
  filter(sp_group == "finfish") %>%
  select(food_mmt) %>%
  as.numeric()

init_ff_h <- init_prod_feed_grp %>%
  filter(sp_group == "finfish") %>%
  select(harvest_mmt) %>%
  as.numeric()

init_bivalve_food <- init_prod_feed_grp %>%
  filter(sp_group == "mollusc") %>%
  select(food_mmt) %>%
  as.numeric()

init_bivalve_h <- init_prod_feed_grp %>%
  filter(sp_group == "mollusc") %>%
  select(harvest_mmt) %>%
  as.numeric()

## total mariculture production
init_mari_prod_val <- sum(init_prod_feed_grp$food_mmt)
init_mari_prod_valh <- sum(init_prod_feed_grp$harvest_mmt)

## total fed
init_prod_fg %>% filter(feed_grp == "fed") %>% select(totalf) %>% as.numeric() / init_mari_prod_val


## total ocean production
ocean_init <- mari_cap_init + (init_mari_prod_val * 1e6)

## freshwater initital
fao_inland_prod <- hist_prod %>%
  filter(year == 2017,
         sp_type != "aquatic_plant",
         measurement %in% c("food_mt", "harvest"),
         location == "inland") %>%
  select(prod_type, measurement, total_q) %>%
  pivot_wider(names_from = measurement, values_from = total_q) %>%
  rename(total_catch_fao = harvest, total_food_fao = food_mt)

lb_aq_init <- fao_inland_prod %>%
  filter(prod_type == "aquaculture") %>%
  select(total_food_fao) %>%
  as.numeric()

lb_capture_init <- fao_inland_prod %>%
  filter(prod_type == "capture") %>%
  select(total_food_fao) %>%
  as.numeric()

lb_total_init <- fao_inland_prod %>%
  select(total_food_fao) %>%
  summarise(sum(total_food_fao)) %>%
  as.numeric()

## current meat production
meat_prod2 <- clean_names(meat_prod) %>%
  filter(element == "Food",
         year_code == 2017) %>%
  mutate(item = ifelse(item == "Bovine Meat", "beef",
                       ifelse(item == "Mutton & Goat Meat", "mutton",
                              ifelse(item == "Pigmeat", "pork",
                                     ifelse(item == "Poultry Meat", "poultry",
                                            ifelse(item == "Meat, Other", "other",
                                                   ifelse(item == "Offals, Edible", "offals", item)))))))

## table from https://www.sciencedirect.com/science/article/pii/S0306919212000942?via%3Dihub
## Nijdam et al

mean_conv <- mean(c(0.7, 0.75, 0.75, 0.8))

meat_conv_df <- tibble(item = c("beef", "pork", "mutton", "poultry", "other", "offals"),
                       conv_val = c(0.7, 0.75, 0.75, 0.8, mean_conv, mean_conv)) 

meat_prod3 <- meat_prod2 %>% 
  left_join(meat_conv_df) %>%
  filter(item %in% meat_conv_df$item) %>%
  select(item, year, value, unit, conv_val) %>%
  mutate(value = value / 1000,
         edible = value * conv_val) %>%
  group_by(year) %>%
  mutate(total_ed_meat = sum(edible)) %>%
  ungroup() %>%
  mutate(unit = "million tonnes") 


meat_init <- meat_prod3 %>%
  select(total_ed_meat) %>%
  unique() %>%
  as.numeric()

## all animal food
animal_f_total <- meat_init + (ocean_init / 1e6) + (lb_total_init / 1e6)

## percent meat that is seafood
((ocean_init / 1e6) + (lb_total_init / 1e6)) / animal_f_total
## 30.9%

## percent meat from the ocean
(ocean_init / 1e6) / animal_f_total
## 16.9

## percent food from sea that is wild
mari_cap_init / ocean_init
## 81.0%

# ## percent food from the sea mariculture
init_mari_prod_val / (ocean_init / 1e6)
## 19.0 %

## initial price
upsides0 <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) %>%
  left_join(conv_vals) %>%
  mutate(food = (Catch / cf_scale_val) * red_scale_mult * convert_mult)

wm_price0 <- weighted.mean(upsides0$Price, upsides0$food)

upsides0_adj <- upsides0 %>%
  mutate(Price = ifelse(SciName == "Engraulis ringens", 250, Price))

wm_price_adj <- weighted.mean(upsides0_adj$Price, upsides0_adj$food)

## finfish mariculture prices per mt
# Atlantic salmon (Salmo salar) - $6193/mt
# Milkfish (Chanos chanos) - $1545/mt
# Barramundi (Lates calcarifer) - $4384/mt

## finfish price
finfish_vec <- c(11, 12, 13, 22, 23, 25, 31, 32, 33, 36, 37, 39, 34, 21, 24)

## ff mariculture production
ff_mari_prod <- hist_prod_sp %>%
  filter(location == "marine",
         prod_type == "aquaculture",
         sp_type == "aquatic_animal",
         species_cat %in% finfish_vec) %>%
  group_by(year, prod_type, name_en, species_cat, sci_name) %>%
  summarise(total_h = sum(quantity),
            total_food = sum(food_mt)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(sum_food = sum(total_food)) %>%
  ungroup() %>%
  mutate(rel_food = total_food / sum_food) %>%
  filter(year == max(hist_prod_sp$year)) %>%
  mutate(sp_group = ifelse(str_detect(name_en, pattern = "almon") == TRUE, "salmon",
                           ifelse(str_detect(name_en, pattern = "Milkfish"), "milkfish",
                                  ifelse(str_detect(name_en, pattern = "Barramundi"), "barramundi", NA)))) %>%
  group_by(sp_group) %>%
  summarise(food_yr = sum(total_food)) %>%
  ungroup() %>%
  filter(!(is.na(sp_group))) %>%
  mutate(price = ifelse(sp_group == "salmon", 6193,
                        ifelse(sp_group == "milkfish", 1545, 4384)))

## use this in init conditions file!! 
mean_maric_price = weighted.mean(ff_mari_prod$price, ff_mari_prod$food_yr)
## 4407.526


## inland production
inland_prod <- clean_names(inland_init) %>%
  filter(product_category == "total_quantity") %>%
  select(production_type) %>%
  as.numeric()

## inland price
inland_price <- clean_names(inland_init) %>%
  filter(product_category == "quantity_weighted_price") %>%
  select(production_type) %>%
  as.numeric()

## make initital production and quantity data frame
init_pq_df <- tibble(sector = c("marine_capture", "inland_production", "finfish_mariculture", "bivalve_mariculture"),
                     quantity = c(mari_cap_init, inland_prod, init_ff_food * 1e6, init_bivalve_food * 1e6),
                     price = c(wm_price_adj, inland_price, mean_maric_price, 1700)) 

# price_df <- tibble(sector = c("marine_capture", "finfish_mari", "bivalve_mari"),
#                    price = c(wm_price_adj, mean_maric_price, 1700),
#                    production = c(mari_cap_init, (init_ff_food * 1e6), (init_bivalve_food * 1e6)))

sf_price_lb <- weighted.mean(init_pq_df$price, init_pq_df$quantity)

price_df_marine <- init_pq_df %>% 
  filter(sector != "inland_production")

sf_price_marine <- weighted.mean(price_df_marine$price, price_df_marine$quantity)

threesector_sum <- mari_cap_init + (init_ff_food * 1e6) + (init_bivalve_food * 1e6)
foursector_sum <- mari_cap_init + (init_ff_food * 1e6) + (init_bivalve_food * 1e6) + inland_prod

ag_init_pq <- tibble(sector = c("aggregate_lb", "aggregate_marine"),
                     quantity = c(foursector_sum, threesector_sum),
                     price = c(sf_price_lb, sf_price_marine))

init_pq_df2 <- rbind(init_pq_df, ag_init_pq)

write_csv(init_pq_df2, paste0(bp_path, "project-materials/nature-revision/outputs/init_pq_df.csv"))


### for si

threesector_sumh <- mari_cap_inith + (init_ff_h * 1e6) + (init_bivalve_h * 1e6)
foursector_sumh <- mari_cap_inith + (init_ff_h * 1e6) + (init_bivalve_h * 1e6) + 60960038

init_pq_dfh <- tibble(sector = c("marine_capture", "inland_production", "finfish_mariculture", "bivalve_mariculture", "aggregate_lb", "aggregate_marine"),
                      quantity = c(mari_cap_inith, 60960038, init_ff_h * 1e6, init_bivalve_h * 1e6, foursector_sumh, threesector_sumh))

write_csv(init_pq_dfh, paste0(bp_path, "project-materials/nature-revision/outputs/init_pq_dfh.csv"))


## 2007 prod
prod2007 <- hist_prod %>%
  filter(year == 2007,
         sp_type == "aquatic_animal",
         measurement == "food_mt")

sum(prod2007$total_q) / 1e6




 
# ## FAO projection is 52kg per captia in 2050
# n_popt <- 9.8e9 ## UN article
# fut_meat_need <- (52 * n_popt * 0.001) / 1e6
# 
# diff <- fut_meat_need - total_meat
