## Tracey Mangin
## March 6, 2020
## Future of Food From the Sea

## Create supply curves -- this script uses outputs from they fishery model and produces supply curves

## Make sure to run files in init_cond folder before proceeding.

## This is STEP 2 for the marine capture fishery analysis. 

## attach librariess
library(tidyverse)
library(purrr)
library(furrr)

## source functions
functions <- list.files(here::here("capture_functions"))

walk(functions, ~ here::here("capture_functions", .x) %>% source()) # load local functions

## read in steady state file
## note to user: this file is produced in capture_run.R (STEP 1). 
ss_outputs_scenarios <- read.csv("ss_outputs_scenarios.csv")

## read in projection df
## note to user: this file is produced in capture_run.R (STEP 1). 
projection_df <- readRDS("projection_outputs.rds")

## upsides
## note to user: update the path and load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F)

## conversions
## note to user: update the path and load conversion file created in init_cond/edible_conv.R script.
conv_vals <- read_csv("isscaap_food_conv_all.csv")

## filter for starting year (2012)
upsides0 <- upsides %>%
  filter(Year == 2012) %>%
  mutate(BMSY = Biomass / BvBmsy) 

## stocks to remove
filter_out <- upsides0 %>%
  filter(MSY > BMSY)

## get fao scale value
## note to user: update the path and load conversion file created in init_cond/init_conditions.R script.
fao_scale_df <- read_csv("upside_fao_scale_df.csv")

## scale capture fishery production
cf_scale_val <- fao_scale_df %>%
  select(rel_catch) %>%
  as.numeric()

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

## ------------------------------------------------------------------------------------
## get production under different price values
## -------------------------------------------------------------------------------------

projection_df2 <- projection_df %>%
  mutate(year = time + 2011) %>%
  filter(year == 2050) %>%
  select(id_orig, f_policy, harvest_2050 = harvest)


## run production function (Fmsy or Fcurrent)
## -------------------------------------------------------

## remove stocks
ss_outputs_scenarios2 <- ss_outputs_scenarios %>%
  filter(!id_orig %in% filter_out$IdOrig)

plan(multiprocess)

price_vals <- as.list(seq(0, 20000, 1))
# price_vals <- as.list(c(100, 1500))

## main text scenario
production_df <- future_map(price_vals,
                            calc_production,
                            input_df = ss_outputs_scenarios2 %>% filter(scenario == "original_inputs")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(production_df, "production_df.rds")

## group by price
production_df_smry <- production_df %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
         total_ss_h = sum(adj_harvest),
         total_2050_h = sum(harvest_2050_adj),
         total_2050_h_scl = sum(harvest_2050_adj_scl),
         total_2050_h_red = sum(h_2050_scl_red),
         total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. 
saveRDS(production_df_smry, "production_df_smry.rds")


## sensitivity analysis 1: improved tech
production_df_s1 <- future_map(price_vals,
                            calc_production,
                            input_df = ss_outputs_scenarios2 %>% filter(scenario == "improved_tech")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(production_df_s1, "production_df_s1.rds")

## group by price
production_s1_smry <- production_df_s1 %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
            total_ss_h = sum(adj_harvest),
            total_2050_h = sum(harvest_2050_adj),
            total_2050_h_scl = sum(harvest_2050_adj_scl),
            total_2050_h_red = sum(h_2050_scl_red),
            total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. 
saveRDS(production_s1_smry, "production_s1_smry.rds")



## sensitivity analysis 2: cs_price
production_df_s2 <- future_map(price_vals,
                               calc_production,
                               input_df = ss_outputs_scenarios2 %>% filter(scenario == "cs_price")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(production_df_s2, "production_df_s2.rds")

## group by price
production_s2_smry <- production_df_s2 %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
            total_ss_h = sum(adj_harvest),
            total_2050_h = sum(harvest_2050_adj),
            total_2050_h_scl = sum(harvest_2050_adj_scl),
            total_2050_h_red = sum(h_2050_scl_red),
            total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. 
saveRDS(production_s2_smry, "production_s2_smry.rds")




## sensitivity analysis 3: improved tech and cs cost of management
production_df_s3 <- future_map(price_vals,
                               calc_production,
                               input_df = ss_outputs_scenarios2 %>% filter(scenario == "improved_tech_cs")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(production_df_s3, "production_df_s3.rds")

## group by price
production_s3_smry <- production_df_s3 %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
            total_ss_h = sum(adj_harvest),
            total_2050_h = sum(harvest_2050_adj),
            total_2050_h_scl = sum(harvest_2050_adj_scl),
            total_2050_h_red = sum(h_2050_scl_red),
            total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. 
saveRDS(production_s3_smry, "production_s3_smry.rds")



## repeat, but assume that you would always choose to manage with FMSY
calc_fmsy_prod <- function(price_val, input_df) {
  
  fmsy_df <- input_df %>%
    mutate(price = price_val,
           extract_cost = calc_extract_cost(g = g, f = FvFmsy, c = c, beta = beta),
           profit = (ss_h * price_val) - (mcost_mt * ss_h) - extract_cost,
           adj_harvest = ifelse(profit < 0, 0, ss_h),
           adj_profit = ifelse(adj_harvest == 0, 0, profit))

}

fmsy_prod_df <- map(price_vals,
                    calc_fmsy_prod,
                    input_df = ss_outputs_scenarios2 %>% filter(f_policy == "FMSY",
                                                               scenario == "original_inputs")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(fmsy_prod_df, "fmsy_prod_df.rds")

## group by price
fmsy_prod_smry <- fmsy_prod_df %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
            total_ss_h = sum(adj_harvest),
            total_2050_h = sum(harvest_2050_adj),
            total_2050_h_scl = sum(harvest_2050_adj_scl),
            total_2050_h_red = sum(h_2050_scl_red),
            total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. 
saveRDS(fmsy_prod_smry, "fmsy_prod_smry.rds")


## repeat, with f0
calc_f0_prod <- function(price_val, input_df) {

  f0_df <- input_df %>%
    mutate(price = price_val,
           extract_cost = calc_extract_cost(g = g, f = FvFmsy, c = c, beta = beta),
           profit = (ss_h * price_val) - (mcost_mt * ss_h) - extract_cost,
           adj_harvest = ifelse(profit < 0, 0, ss_h),
           adj_profit = ifelse(adj_harvest == 0, 0, profit))

}

f0_prod_df <- map(price_vals,
                  calc_f0_prod,
                  input_df = ss_outputs_scenarios2 %>% filter(f_policy == "F_current",  scenario == "original_inputs")) %>%
  bind_rows() %>%
  left_join(projection_df2) %>%
  mutate(harvest_2050_adj = ifelse(adj_harvest == 0, 0, harvest_2050),
         harvest_2050_adj_scl = harvest_2050_adj / cf_scale_val,
         h_2050_scl_red = harvest_2050_adj_scl * red_scale_mult,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  mutate(food_2050 = h_2050_scl_red * convert_mult)

## note to user: update path and save rds file. 
saveRDS(f0_prod_df, "f0_prod_df.rds")


## group by price
f0_prod_smry <- f0_prod_df %>%
  group_by(scenario, price, seafood_type, convert_val) %>%
  summarise(total_profit = sum(adj_profit),
            total_ss_h = sum(adj_harvest),
            total_2050_h = sum(harvest_2050_adj),
            total_2050_h_scl = sum(harvest_2050_adj_scl),
            total_2050_h_red = sum(h_2050_scl_red),
            total_2050_food = sum(food_2050)) %>%
  ungroup()

## note to user: update path and save rds file. s
saveRDS(f0_prod_smry, "f0_prod_smry.rds")


