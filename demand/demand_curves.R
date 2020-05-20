## Tracey Mangin 
## December 4, 2019
## demand curves

## future of food from the sea

## note to user: must run files in init_cond folder first

## this creates demand curves

## attach libraries
library(tidyverse)
library(rJava)
library(tabulizer)
library(tabulizerjars)
library(rebus)
library(viridis)
library(reconPlots)
library(scales)
library(furrr)

## initial production and price
## note to user: change path, load file created in init_conditions.R
init_pq_df <- read_csv("init_pq_df.csv")

## for pulling out gammas
pdf1 <- '~/GitHub/future_food_from_sea/demand/cai_2017.pdf'

caietal <- file.path(pdf1)

pdf_txt <- tabulizer::extract_text(caietal)
tbls <- tabulizer::extract_tables(caietal, pages = 34, output = "data.frame")

## clean up the table
gamma_tbl <- tbls[[1]]

gamma_coln <- c("region", "marine_mean", "marine_lb", "marine_ub", "fresh_diad_mean", "fresh_diad_lb", "fresh_diad_ub",
                "crust_mean", "crust_ub_lb", "shm_mean", "shm_lb", "shm_ub", "ceph_mean", "ceph_lb", "ceph_ub")

colnames(gamma_tbl) <- gamma_coln

gamma_tbl2 <- gamma_tbl %>%
  mutate(crust_lb = str_extract(crust_ub_lb, pattern = START %R% one_or_more(DIGIT) %R% DOT %R% one_or_more(DIGIT)),
         crust_ub = str_extract(crust_ub_lb, pattern = SPACE %R% one_or_more(DIGIT) %R% DOT %R% one_or_more(DIGIT)),
         crust_ub = str_remove(crust_ub, pattern = SPACE)) %>%
  select(region:crust_mean, crust_lb, crust_ub, shm_mean:ceph_ub) 

region_df <- data.frame(geography = c(rep("region", 20), rep("country", 20)))

gamma_tbl3 <- cbind(region_df, gamma_tbl2) %>%
  select(region, geography, marine_mean:ceph_ub)

## median gamma for regions -- marine
gamma_est <- gamma_tbl3 %>%
  group_by(geography) %>%
  summarise(mean_gamma_mf = mean(marine_mean),
            med_gamma_mf = median(marine_mean))

gamma_mf <- gamma_est %>%
  filter(geography == "region") %>%
  select(mean_gamma_mf) %>%
  as.numeric()

## median gamma for regions -- bivalve
gamma_est_biv <- gamma_tbl3 %>%
  group_by(geography) %>%
  mutate(shm_mean = as.numeric(shm_mean)) %>%
  summarise(mean_gamma_shm = mean(shm_mean, na.rm = T),
            med_gamma_shm = median(shm_mean))

gamma_biv <- gamma_est_biv %>%
  filter(geography == "region") %>%
  select(mean_gamma_shm) %>%
  as.numeric()

## median gamma for regions -- freshwater
gamma_est_fresh <- gamma_tbl3 %>%
  group_by(geography) %>%
  mutate(fresh_mean = as.numeric(fresh_diad_mean)) %>%
  summarise(mean_gamma_fresh = mean(fresh_mean, na.rm = T),
            med_gamma_fresh = median(fresh_mean))

gamma_fresh <- gamma_est_fresh %>%
  filter(geography == "region") %>%
  select(mean_gamma_fresh) %>%
  as.numeric()


## find gamma for aggegrate scenarios
## -------------------------------------

ps_gamma_3sect <- init_pq_df %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture")) %>%
  mutate(gamma = ifelse(sector == "marine_capture", gamma_mf,
                        ifelse(sector == "finfish_mariculture", gamma_mf, gamma_biv)),
         gamma_grp = ifelse(sector == "marine_capture", "finfish",
                            ifelse(sector == "finfish_mariculture", "finfish", "bivalves"))) %>%
  group_by(gamma_grp, gamma) %>%
  summarise(sum_q = sum(quantity))
  
ps_gamma_3sect_val <- weighted.mean(ps_gamma_3sect$gamma, ps_gamma_3sect$sum_q)

ps_gamma_4sect <- init_pq_df %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture", "inland_production")) %>%
  mutate(gamma = ifelse(sector == "marine_capture", gamma_mf,
                        ifelse(sector == "finfish_mariculture", gamma_mf, 
                               ifelse(sector == "bivalve_mariculture", gamma_biv, gamma_fresh))),
         gamma_grp = ifelse(sector == "marine_capture", "finfish",
                            ifelse(sector == "finfish_mariculture", "finfish", 
                                   ifelse(sector == "bivalve_mariculture", "bivalves", "fresh")))) %>%
  group_by(gamma_grp, gamma) %>%
  summarise(sum_q = sum(quantity))

ps_gamma_4sect_val <- weighted.mean(ps_gamma_4sect$gamma, ps_gamma_4sect$sum_q)


## functions

## calculate consumption
calc_cons <- function(pval, n_pop, alpha, beta, y_gdpc, gamma) {
  
  consumption <- n_pop * alpha * pval ^ beta * y_gdpc ^ gamma
  
}

## calculate alpha
calc_alpha <- function(cons_val, n_pop, pval, beta, y_gdpc, gamma) {
  
  alpha <- cons_val / (n_pop * pval ^ beta * y_gdpc ^ gamma)
  
}

## estimate beta val, taken from our report which cites Muhammed et al. 2013
low_ope <- -0.48
med_ope <- -0.382
high_ope <- -0.278

## define current values
n_pop0 <- 7.39e9 ## IMF, 2017
y_gdpc0 <- 17117.153 ## the world bank, 2017
beta_val <- mean(c(low_ope, med_ope, high_ope)) ## own price elasticity



## repeat with future estimates
n_popt <- 9.8e9 ## UN article
# y_gdpct <- y_gdpc0 + y_gdpc0 * 1.3 ## PWC https://www.pwc.com/gx/en/issues/economy/the-world-in-2050.html

##
gdp2016 <- 128.567 * 1e12
gdp2050 <- gdp2016 + gdp2016 * 1.3
y_gdpct <- gdp2050 / n_popt

## demand curve input df
input_df <- init_pq_df %>%
  mutate(demand_scen = "current",
         population = n_pop0,
         beta = beta_val,
         gamma = ifelse(sector %in% c("marine_capture", "finfish_mariculture"), gamma_mf,
                        ifelse(sector == "inland_production", gamma_fresh,
                               ifelse(sector == "bivalve_mariculture", gamma_biv,
                                      ifelse(sector == "aggregate_lb", ps_gamma_4sect_val, ps_gamma_3sect_val)))),
         yval = y_gdpc0) %>%
  rowwise() %>%
  mutate(alpha = calc_alpha(cons_val = quantity, n_pop = population, pval = price, beta = beta, y_gdpc = yval, gamma = gamma)) %>%
  ungroup()

input_df_fut <- input_df %>%
  mutate(demand_scen = "future",
         population = n_popt,
         yval = y_gdpct)

input_df_all <- rbind(input_df, input_df_fut) %>%
  mutate(scenario = paste(sector, demand_scen, sep = "_"))


create_demand <- function(dscen, inputs){
  
  ## define the price and sector curve
  sector_name <- dscen$sector
  p <- dscen$price_val
  
  ## define inputs
  input_df <- inputs %>%
    filter(sector == sector_name)
  
  cons <- calc_cons(pval = p, 
                    n_pop = input_df$population, 
                    alpha = input_df$alpha, 
                    beta = input_df$beta, 
                    y_gdpc = input_df$yval, 
                    gamma = input_df$gamma)
  
  demand <- tibble(sector = sector_name,
                   demand_scen = input_df$demand_scen,
                   price = p,
                   quantity = cons)
  
}




## make current demand curve
price_vals <- as.list(seq(0, 20000, 1))
sector_vec <- as.list(unique(input_df_all$sector))
#sector_vec <- as.list(c("marine_capture", "finfish_mari_a", "finfish_mari_b", "finfish_mari_c", "bivalve_mariculture", "aggregate"))

# plan(multiprocess)

demand_df0 <- cross(list(sector_name = sector_vec,
                      price_val = price_vals)) %>%
  map(create_demand,
      inputs = input_df_all %>% filter(demand_scen == "current")) %>%
  bind_rows()

## now for future
demand_df_fut <- cross(list(sector_name = sector_vec,
                         price_val = price_vals)) %>%
  map(create_demand,
      inputs = input_df_all %>% filter(demand_scen == "future")) %>%
  bind_rows()


## now do extreme for each sector
## double future demand curve

mult_val <- 2

demand_df_ext <- demand_df_fut %>%
  mutate(quantity = quantity * mult_val) %>%
  mutate(demand_scen = "extreme")


## combine all three curves
demand_df_all <- rbind(demand_df0, demand_df_fut, demand_df_ext)

## note to user: change path, save for scripts in analysis folder
saveRDS(demand_df_all, "demand_curves_final.rds")





