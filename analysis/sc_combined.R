## Tracey Mangin
## December 15, 2019
## combined supply curves
## future food from the sea

## note to user: must run scripts in init_cond, capture_fisheries, and aquaculture folder first (outputs needed).
## must run all_supply_curves.R first to create file needed here.
## this script creates figure 3 in the main text.

library(tidyverse)
library(janitor)
library(scales)
library("wesanderson")
library(plotly)
library(ggnewscale)
library(viridis)


## all data
## note to user: update path, read file created in all_supply_curves.R
data <- read_rds("subset_supply_curves.rds")


## initial production and price
## note to user: update path, read file created in init_conditions.R
init_pq_df <- read_csv("init_pq_df.csv")

## initial production and price, harvest
## note to user: update path, read file created in init_conditions.R
init_pq_dfh <- read_csv(paste0(bp_path, "outputs/init_pq_dfh.csv"))

## read in relevant outputs/processed data
## note to user -- update the following line of code to load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F)

## read in projection df
## note to user -- update the following line of code to load file created in catpure_run.R
projection_df <- readRDS("projection_outputs.rds")

## conversion factors
## note to user -- update path, load file created in edible_conv.R file
conv_vals <- read_csv("isscaap_food_conv_all.csv") %>%
  filter(seafood_type != "plant")

## get fao scale value
## note to user: update path, read file created in init_conditions.R
fao_scale_df <- read_csv("outputs/upside_fao_scale_df.csv")

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

## scale value
cf_scale_val <- fao_scale_df %>%
  select(rel_catch) %>%
  as.numeric()

## supply curves -- for pulling out bau scenario
## note to user: update path, read file created sc_outputs.R
mc_supply <- read_csv("harvest_x_price_x_conversion.csv") %>%
  select(-X1)

## calculate FMSY supply in 2050 for supply curves
## ------------------------------------------------
upsides0 <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) 

ss_fmsy <- upsides0 %>%
  mutate(scl_msy = MSY / cf_scale_val) %>%
  select(SpeciesCat, MSY, scl_msy) %>%
  left_join(conv_vals) %>%
  group_by(seafood_type, convert_mult) %>%
  summarise(scl_msy = sum(scl_msy)) %>%
  ungroup() %>%
  mutate(food = scl_msy * convert_mult)

sum(ss_fmsy$food)

fmsy_proj_df <- projection_df %>%
  mutate(year = time + 2011) %>%
  filter(year == 2050,
         id_orig %in% upsides0$IdOrig,
         f_policy == "FMSY") %>%
  select(id_orig, harvest) %>%
  mutate(scenario = "FMSY_2050",
         sector = "capture",
         mt = harvest / cf_scale_val,
         SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  group_by(scenario, sector, seafood_type, convert_mult) %>%
  summarise(mt = sum(mt)) %>%
  ungroup()

price_vec <- seq(0, 20000, 1)
seafood_type <- unique(conv_vals$seafood_type)

conv_vals2 <- conv_vals %>%
  select(seafood_type, convert_mult) %>%
  unique()

fmsy_proj_df2 <- expand_grid(seafood_type, price_vec) %>%
  # left_join(conv_vals2) %>%
  left_join(fmsy_proj_df) %>%
  rename(price = price_vec) %>%
  select(scenario, sector, seafood_type, convert_mult, price, mt) %>%
  mutate(meat_mt = mt * red_scale_mult * convert_mult) %>%
  group_by(scenario, sector, price) %>%
  summarise(mt = sum(mt),
            meat_mt = sum(meat_mt)) %>%
  ungroup()

fmsy_2050_mt <- unique(fmsy_proj_df2$mt)

inith_mcf <- init_pq_dfh %>%
  filter(sector == "marine_capture") %>%
  select(quantity) %>%
  as.numeric()

## increase in harvest
(fmsy_2050_mt - inith_mcf) / inith_mcf


## BAU
## ---------------

mc_supply2 <- mc_supply %>%
  left_join(conv_vals2) %>%
  select(scenario:seafood_type, convert_mult, price, mt)

bau_df <- mc_supply2 %>%
  filter(scenario == "F0_orig_inputs") %>%
  mutate(mt_red = mt * red_scale_mult, 
         food_mt = mt_red * convert_mult) %>%
  group_by(scenario, sector, price) %>%
  summarise(mt = sum(mt),
            meat_mt = sum(food_mt, na.rm = T)) %>%
  ungroup()

## rational reform
## ------------------
rr_df <- mc_supply2 %>%
  filter(scenario == "original_inputs") %>%
  mutate(mt_red = mt * red_scale_mult, 
         food_mt = mt_red * convert_mult) %>%
  group_by(scenario, sector, price) %>%
  summarise(mt = sum(mt),
            meat_mt = sum(food_mt, na.rm = T)) %>%
  ungroup()

## marine capture supply curves for fig 3
## ------------------

marine_cap_df <- rbind(fmsy_proj_df2, rr_df, bau_df)


## 2050 BAU production at current prices for marine capture
p0_marine <- init_pq_df %>%
  filter(sector == "marine_capture") %>%
  select(price) %>%
  as.numeric()

bau_prod <- mc_supply2 %>%
  filter(scenario == "F0_orig_inputs") %>%
  mutate(mt_red = mt * red_scale_mult, 
         food_mt = mt_red * convert_mult) %>%
  group_by(price) %>%
  summarise(tot_mt = sum(mt, na.rm = T),
            tot_food_mt = sum(food_mt, na.rm = T)) %>%
  ungroup() %>%
  filter(price == round(p0_marine))

## rational reform
rf_prod <- mc_supply2 %>%
  filter(scenario == "original_inputs") %>%
  mutate(mt_red = mt * red_scale_mult, 
         food_mt = mt_red * convert_mult) %>%
  group_by(price) %>%
  summarise(tot_mt = sum(mt, na.rm = T),
            tot_food_mt = sum(food_mt, na.rm = T)) %>%
  ungroup() %>%
  filter(price == round(p0_marine))

## increase in harvest
(as.numeric(rf_prod$tot_mt[1]) - inith_mcf) / inith_mcf

## fmsy reform
fmsy_prod <- fmsy_proj_df2 %>%
  filter(price == round(p0_marine))



## all three sectors


## initial production points for each sector
init_prod_df <- init_pq_df %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture"))

create_curr_sup <- function(df) {
  
  sector_name <- df$sector[1]
  
  qval <- df %>%
    mutate(quantity = quantity / 1e6) %>%
    select(quantity) %>%
    as.numeric()
  
  pval <- df %>%
    select(price) %>%
    as.numeric()
  
  init_df_sup <- tibble(price = seq(0, pval, 1),
                        total_ed = rep(qval, length(seq(0, pval, 1))),
                        scenario = rep("current", length(seq(0, pval, 1)))) %>%
    mutate(sector = sector_name)
  
}

##
curr_sup_mc <- create_curr_sup(init_prod_df %>% filter(sector == "marine_capture"))
curr_sup_ff <- create_curr_sup(init_prod_df %>% filter(sector == "finfish_mariculture"))
curr_sup_b <- create_curr_sup(init_prod_df %>% filter(sector == "bivalve_mariculture"))

curr_sup_df <- rbind(curr_sup_mc, curr_sup_ff, curr_sup_b)

## other df
create_curr_dem <- function(df) {
  
  sector_name <- df$sector[1]
  
  qval <- df %>%
    mutate(quantity = quantity / 1e6) %>%
    select(quantity) %>%
    as.numeric()
  
  pval <- df %>%
    select(price) %>%
    as.numeric()
  
  init_df_dem <- tibble(price = rep(pval, length(seq(0, qval, 1))),
                        total_ed = (seq(0, qval, 1)),
                        scenario = rep("current", length(seq(0, qval, 1)))) %>%
    mutate(sector = sector_name)
  
}

##
curr_dem_mc <- create_curr_dem(init_prod_df %>% filter(sector == "marine_capture"))
curr_dem_ff <- create_curr_dem(init_prod_df %>% filter(sector == "finfish_mariculture"))
curr_dem_b <- create_curr_dem(init_prod_df %>% filter(sector == "bivalve_mariculture"))

curr_dem_df <- rbind(curr_dem_mc, curr_dem_ff, curr_dem_b)

## test with a figure
# 
# ggplot() +
#   geom_line(data = curr_dem_df, aes(x = total_ed, y = price, group = scenario), size = 1, alpha = 0.8, lty = 1, color = "black") +
#   geom_line(data = curr_sup_df, aes(x = total_ed, y = price, group = scenario), size = 1, alpha = 0.8, lty = 1, color = "black") +
#   geom_point(data = init_pq_df %>% filter(sector %in% c("marine_capture", "Finfish mariculture", "bivalve_mariculture")),
#              aes(x = quantity / 1e6, y = price), size = 3, color = "black") +
#   facet_wrap(~sector)

## nature paper scenarios
np_scen_vec <- c("Scenario 3 - byproducts + directed", "Scenario 4a - tech advance", "Scenario 4c - tech advance")

## get scenarios that we are interested in
data_np <- data %>%
  filter(aq_scen %in% np_scen_vec,
         sector != "perf_substitudes") %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture")) %>%
  mutate(scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) %>%
  filter(sector %in% c("Finfish mariculture", "Bivalve mariculture"))

# data_np$sector <- factor(data_np$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Capture fisheries"))

# ## check capture numbers, just in case
# capt_prod <- data %>%
#   filter(aq_scen %in% np_scen_vec,
#          sector != "perf_substitudes") %>%
#   mutate(sector = str_replace(sector, "aquaculture", "mariculture")) %>%
#   mutate(scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
#                            ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
#                                   ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) %>%
#   filter(price == 1308,
#          sector == "Capture fisheries")

## Fed mariculture supply curves and inital production points
## ------------------------------------------------------------------------

## max production under scenario A
scen_a <- data_np %>%
  filter(sector == "Finfish mariculture",
         scen_lab== "Policy reforms")

## max production under current feed assumptions
max(scen_a$meat_yr) / 1e6

init_ffm_p <- init_pq_df %>%
  filter(sector == "finfish_mariculture") %>%
  select(price) %>%
  as.numeric()

init_ffm_q <- init_pq_df %>%
  filter(sector == "finfish_mariculture") %>%
  select(quantity) %>%
  as.numeric()

## difference in current vs potential
pot_cp <- scen_a %>% filter(price == round(init_ffm_p)) %>%
  select(meat_yr) %>%
  as.numeric()

(pot_cp - init_ffm_q) / 1e6

## production potential at current prices, total harvest lwe
init_ffm_h <- init_pq_dfh %>%
  filter(sector == "finfish_mariculture") %>%
  select(quantity) %>%
  as.numeric()

tech_prod_pot_h <- data_np %>% filter(price == round(init_ffm_p), sector == "Finfish mariculture") %>%
  mutate(mt_year_mmt = mt_yr / 1e6) %>%
  mutate(curr_prod = init_ffm_h,
         delta_perc = (mt_yr - curr_prod) / curr_prod * 100)

## bivalves
## --------------------------

## max production under scenario A
scen_a_b <- data_np %>%
  filter(sector == "Bivalve mariculture",
         scen_lab == "Policy reforms")

## max production under current feed assumptions
max(scen_a_b$meat_yr) / 1e6

init_bm_p <- init_pq_df %>%
  filter(sector == "bivalve_mariculture") %>%
  select(price) %>%
  as.numeric()

init_bm_q <- init_pq_df %>%
  filter(sector == "bivalve_mariculture") %>%
  select(quantity) %>%
  as.numeric()

## difference in current vs potential
pot_cp_b <- scen_a_b %>% filter(price == init_bm_p) %>%
  select(meat_yr) %>%
  as.numeric()

pot_cp_b - init_bm_q

## production potential at current prices, total harvest lwe
init_bm_h <- init_pq_dfh %>%
  filter(sector == "bivalve_mariculture") %>%
  select(quantity) %>%
  as.numeric()

tech_prod_pot_b_h <- data_np %>% filter(price == round(init_bm_p), sector == "Bivalve mariculture") %>%
  mutate(mt_year_mmt = mt_yr / 1e6) %>%
  mutate(curr_prod = init_bm_h,
         delta_perc = (mt_yr - curr_prod) / curr_prod * 100)


## Combine the dataframes
## -------------------------------------

data_np2 <- data_np %>%
  select(scenario = scen_lab, sector, price, total_ed = meat_yr) 


marine_cap_df2 <- marine_cap_df %>%
  select(-mt) %>%
  rename(total_ed = meat_mt) %>%
  mutate(sector = "Marine wild fisheries")

supply_curves <- rbind(data_np2, marine_cap_df2) %>%
  mutate(type = ifelse(sector == "Marine wild fisheries", "Wild", "Mariculture"))

supply_curves_biv <- supply_curves %>%
  filter(sector == "Bivalve mariculture") %>%
  filter(price <= 2500,
         scenario == "Policy reforms")

supply_curves_ff <- supply_curves %>%
  filter(sector == "Finfish mariculture") %>%
  filter(price <= 6000,
         price >= 3000)

supply_curves_m <- rbind(supply_curves_biv, supply_curves_ff)

supply_curves_wc <- supply_curves %>%
  filter(type != "Mariculture",
         price <= 2500) %>%
  mutate(scenario = ifelse(scenario == "F0_orig_inputs", "F current",
                           ifelse(scenario == "FMSY_2050", "MSY", "Rational reform")))


## do it the right way
## ------------------------------------------------------------

supply_curves_wc$sector <- factor(supply_curves_wc$sector, levels = c("Marine wild fisheries"))
supply_curves_m$sector <- factor(supply_curves_m$sector, levels = c("Finfish mariculture", "Bivalve mariculture"))

curr_dem_df2 <- curr_dem_df %>%
  mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
                         ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")),
         sector_lab =  ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                              ifelse(sector == "Finfish mariculture", paste0("b ", sector), paste0("c ", sector))))

curr_sup_df2 <- curr_sup_df %>%
  mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
                         ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")),
         sector_lab =  ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                              ifelse(sector == "Finfish mariculture", paste0("b ", sector), paste0("c ", sector))))

init_intersect <- init_pq_df %>%
  rename(total_ed = quantity) %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture")) %>%
  mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
                         ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")),
         sector_lab =  ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                              ifelse(sector == "Finfish mariculture", paste0("b ", sector), paste0("c ", sector))))



## ggplot supply curves
# color_palette <- c("#8bbdd9", "#298fca", "#094c72")

supply_curves_wc_n <- supply_curves_wc %>%
  mutate(sector_lab = paste0("a ", sector))

supply_curves_m_n <- supply_curves_m %>%
  mutate(sector_lab = ifelse(sector == "Bivalve mariculture", paste0("c ", sector), paste0("b ", sector)))

curr_dem_df2$sector <- factor(curr_dem_df2$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture"))
curr_sup_df2$sector <- factor(curr_sup_df2$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture"))
init_intersect$sector <- factor(init_intersect$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture"))



combined_fig <- ggplot(supply_curves_wc_n, aes(x = total_ed / 1e6, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 0.5) +
  scale_color_grey(name = "Marine wild scenario: ",
                   breaks = c("F current", "Rational reform", "MSY")) +
  new_scale_color() +
  geom_path(data = supply_curves_m_n, aes(x = total_ed / 1e6, y = price, group = scenario, color = scenario), size = 0.5, inherit.aes = F) +
  scale_color_manual(name = "Mariculture scenario: ",
                     breaks = c("Policy reforms", "Tech innovation", "Tech innovation (Ambitious)"),
                     values = c("Policy reforms" = "#009E73",
                                "Tech innovation" = "#56B4E9",
                                "Tech innovation (Ambitious)" = "#0072B2")) +
  facet_wrap(~sector_lab, scales = "free") + 
  geom_line(data = curr_dem_df2, aes(x = total_ed, y = price, group = sector), size = 0.25, alpha = 0.8, lty = 2, color = "black", inherit.aes = F) +
  geom_line(data = curr_sup_df2, aes(x = total_ed, y = price, group = sector), size = 0.25, alpha = 0.8, lty = 2, color = "black", inherit.aes = F) +
  geom_point(data = init_intersect, aes(x = total_ed / 1e6, y = price, group = sector), size = 2, color = "black", inherit.aes = F) +
  xlab("Edible production (mmt)") +
  ylab("Price (USD per mt)") +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  # scale_linetype_manual(name = "Marine wild scenario: ",
  #                       breaks = c("F current", "Rational reform", "MSY"),
  #                       values = c("F current" = "solid", 
  #                                  "Rational reform" = 4,
  #                                  "MSY" = "dotted")) +
  scale_y_continuous(label=comma) +
  # geom_hline(yintercept = pval0, lty = "dashed", color = "black") +
  # annotate("text", x = 300, y = pval0 + 75, label = paste0("Current weighted average global price = $", format(round(pval0), nsmall=0, big.mark=","), " / mt"), size = 5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8, hjust = 0),
        legend.box = "vertical",
        # legend.key.width = unit(1.5, "cm"),
        strip.background = element_rect(color = "white", fill = "white"),
        legend.spacing.y = unit(-2, "mm"))


## note to user: update path, save figure (fig 3 main text.)
ggsave(filename = "figures/fig3.pdf", combined_fig, width = 136, height = 100, units = "mm", dpi = 600)




