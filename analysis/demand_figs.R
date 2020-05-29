## Tracey Mangin
## March 10, 2020
## future food from the sea: figures for SI

library(tidyverse)
library(janitor)
library(scales)
library(viridis)
library(reconPlots)
# library("wesanderson")
# library(plotly)
library(cowplot)
library(gridExtra)


## supply curves for main text
## note to user: change path and read in file created in all_supply_curves.R
data <- read_rds("subset_supply_curves.rds")

## initial production and price
## note to user: change path and read in file created in init_conditions.R
init_pq_df <- read_csv("init_pq_df.csv")

## demand
## note to user: change path and read in file created in demand_curves.R
demand_df <- read_rds("demand_curves_final.rds")

## read in csv
## note to user: change path and read in file created in sector_sp_outputs.R
final_indiv_df <- read_csv("sector_sp_prod_final.csv")


## ADD PERFECT SUB SUPPLY CURVE WITHOUT LAND BASED
## ------------------------------------------------
data_ps3 <- data %>%
  filter(!sector %in% c("perf_substitutes", "Inland")) %>% 
  mutate(inl_scen == "none") %>%
  group_by(scenario, scenario2, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, price) %>%
  summarise(mt_yr = sum(mt_yr),
            meat_yr = sum(meat_yr)) %>%
  ungroup() %>%
  mutate(sector = "aggregate_marine") %>%
  select(scenario, scenario2, inl_scen, cap_scen, aq_spp, aq_scen, aq_cost_scalar, sector, price,
         mt_yr, meat_yr)
  
supply_data <- data %>%
  mutate(sector = ifelse(sector == "perf_substitutes", "aggregate_lb", sector)) %>%
  select(-dhc_mt_yr) %>%
  rbind(data_ps3)

 
## Aggregate supply curves, scenarios A-D
## ------------------------------------------------------------------------
np_scen_vec <- c("Scenario 3 - byproducts + directed", "Scenario 4c - tech advance")

## get scenarios that we are interested in
data_np <- supply_data %>%
  filter(aq_scen %in% np_scen_vec,
         !sector %in% c("aggregate_lb", "aggregate_marine"),
         price <= 10000) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 

## get scenarios that we are interested in
data_perf_sub <- supply_data %>%
  filter(aq_scen %in% np_scen_vec,
         sector == c("aggregate_lb"),
         price <= 10000) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 


data_np$sector <- factor(data_np$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Marine wild fisheries", "Inland fisheries"))
data_perf_sub$sector <- factor(data_perf_sub$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Marine wild fisheries", "Inland fisheries"))


## fig
agg_sc <- ggplot(data_np, aes(y = meat_yr / 1e6, x = price, fill = sector), alpha = 0.9) +
  geom_area(stat = "identity") +
  # geom_vline(xintercept = sf_price, lty = 2) +
  facet_wrap(~scen_lab, ncol = 2, scales = "fixed") +
  coord_flip() +
  # geom_area(stat="identity") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  # scale_x_continuous(labels = comma, limits = c(0,10000 / sf_price)) +
  scale_fill_viridis(discrete=TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  geom_line(data = data_perf_sub, aes(y = meat_yr / 1e6, x = price), color = "black", size = 1, inherit.aes = FALSE) +
  xlab("Price (USD per mt)") + 
  ylab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16))

# ggsave(filename =  paste0(bp_path, "figures/si_1.png"), agg_sc, width = 8, height = 5, units = "in", dpi = 300)
# 
# ## fig
# agg_sc_harvest <- ggplot(data_np, aes(y = mt_yr / 1e6, x = price, fill = sector), alpha = 0.9) +
#   geom_area(stat = "identity") +
#   # geom_vline(xintercept = sf_price, lty = 2) +
#   facet_wrap(~scen_lab, ncol = 2, scales = "fixed") +
#   coord_flip() +
#   # geom_area(stat="identity") +
#   guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
#   # scale_x_continuous(labels = comma, limits = c(0,10000 / sf_price)) +
#   scale_fill_viridis(discrete=TRUE) +
#   scale_y_continuous(labels = comma) +
#   scale_x_continuous(labels = comma) +
#   geom_line(data = data_perf_sub, aes(y = meat_yr / 1e6, x = price), color = "black", size = 1, inherit.aes = FALSE) +
#   xlab("Price (USD per mt)") + 
#   ylab("Total harvest (mmt)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 18),
#         title = element_text(size = 14),
#         legend.title = element_blank(),
#         legend.position = "top",
#         legend.text = element_text(size = 14),
#         strip.text = element_text(size = 16))
# 

### part two
###########################################
data2 <- supply_data %>%
  filter(aq_scen %in% np_scen_vec,
         sector == "aggregate_lb",
                price <= 10000) %>%
  mutate(scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 

demand_df2 <- demand_df %>%
  filter(sector == "aggregate_lb",
         quantity <= 600 * 1e6,
         price <= 10000) %>%
  # mutate(sector = ifelse(sector == "aggregate", "perf_substitutes")) %>%
  rename(meat_yr = quantity) %>%
  mutate(demand_scen = ifelse(demand_scen == "current", "Current",
                              ifelse(demand_scen == "extreme", "Extreme", "Future")))




demand_df2$demand_scen <- factor(demand_df2$demand_scen, levels = c("Current", "Future", "Extreme"))

## combined with demand curves
agg_sup_dem <- ggplot(data2, aes(y = meat_yr / 1e6, x = price)) +
  geom_line(color = "black", size = 1.5) +
  facet_wrap(~scen_lab, ncol = 3, scales = "fixed") +
  # geom_vline(xintercept = sf_price, lty = 2) +
  geom_line(data = demand_df2, aes(y = meat_yr / 1e6, x = price, color = demand_scen), size = 1) +
  coord_flip() +
  # guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_color_manual(name = "Demand: ",
                     breaks = c("Current", "Future", "Extreme"), 
                     values = c("#009966", "#3333CC", "#ff3366")) +
  # geom_area(stat="identity") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  xlab("Price (USD per mt)") + 
  ylab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_blank(), 
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16))

## note to user: change path and save si figure
ggsave(filename = "si_1_updated.png", arrangeGrob(agg_sc, agg_sup_dem, ncol = 1), width = 8, height = 10, units = "in", dpi = 300)


## -----------------------------------
## find intersecting points
## ------------------------------------

find_sd_point_indiv <- function(scen, sdf, ddf) {
  
  ## define the price and sector curve
  demand <- scen$dval
  supply_scen <- scen$sscen
  prod_sector <- scen$sector_val
  
  ## supply df
  scdf <- sdf %>%
    filter(scen_lab == supply_scen,
           sector == prod_sector) %>%
    mutate(x = meat_yr,
           y = price)
  
  ## demand df
  dcdf <- ddf %>%
    filter(sector == prod_sector,
           demand_scen == demand) %>%
    mutate(x = meat_yr,
           y = price)
  
  intersect <- try({curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)})
  if (inherits(intersect, "try-error")){
    intersect <- list(y = NA, x = NA)
  } else {
    intersect <- curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)
  }
  
  
  output <- tibble(scenario = supply_scen,
                   sector = prod_sector,
                   demand_scen = demand,
                   price = intersect$y,
                   meat_yr = intersect$x)
  
}

## make df of all intersection points
demand_vec <- as.list(unique(demand_df2$demand_scen))
# demand_vec <- as.list(c("extreme"))
supply_vec <- as.list(unique(data2$scen_lab))
# supply_vec <- as.list(c("Scenario 2"))
sector_names <- as.list(unique(data2$sector))
# sector_names <- as.list(c("Capture fisheries"))

indiv_prod_vals <- cross(list(dval = demand_vec,
                              sscen = supply_vec,
                              sector_val = sector_names)) %>%
  map(find_sd_point_indiv,
      sdf = data2, 
      ddf = demand_df2) %>%
  bind_rows()

### now find sector specific outputs
## this will be a function

intersect_df <- indiv_prod_vals %>%
  select(scen_lab = scenario, demand_scen, price, total_meat_yr = meat_yr) %>%
  mutate(round_price = round(price),
         total_meat_yr = total_meat_yr / 1e6)

supply_df <- supply_data %>%
  filter(sector %in% c("Capture fisheries", "Bivalve aquaculture", "Finfish aquaculture", "Inland")) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen))))


zeros_df <- expand_grid(scenario2 = unique(supply_df$scenario2),
                        price = seq(0, 19999, 1))

supply_all_p_df <- supply_df %>%
  select(scenario2:sector) %>%
  unique() %>%
  left_join(zeros_df) %>%
  left_join(supply_df) %>%
  mutate(meat_yr = ifelse(is.na(meat_yr), 0, meat_yr)) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) %>%
  rename(round_price = price)



sector_sup_df <- supply_all_p_df %>%
  left_join(intersect_df) %>%
  filter(!is.na(total_meat_yr)) %>%
  select(scenario2:aq_scen, scen_lab, aq_cost_scalar, sector, demand_scen, price, mt_yr, sector_meat_yr = meat_yr, total_meat_yr) %>%
  mutate(mt_year = mt_yr / 1e6,
         sector_meat_yr = sector_meat_yr / 1e6)

# ## make table for SI
# si_ps_table <- sector_sup_df %>%
#   select(scen_lab, sector, demand_scen, price, sector_meat_yr, total_meat_yr)
# 
#   
# 
# ## table for si
# 
# si_sub_table <- sector_sup_df %>%
#   select(aq_scen, scen_lab, sector:sector_meat_yr) %>%
#   pivot_wider(names_from = sector, values_from = sector_meat_yr) 



## MAKE DONUT
## --------------------------------------------------------------

subst_donut_df <- sector_sup_df %>%
  select(scenario = scen_lab, sector, demand = demand_scen, price, meat_yr = sector_meat_yr, total_prod = total_meat_yr) %>%
  mutate(rel_prod = meat_yr / total_prod,
         meat_yr_mmt = meat_yr,
         tot_meat_mmt = total_prod) 


donut_df <- subst_donut_df %>%
  arrange(scenario, demand, sector) %>%
  filter(scenario == "Tech innovation (Ambitious)") %>%
  group_by(scenario, demand) %>%
  mutate(ymax = cumsum(rel_prod),
         ymin = c(0, head(ymax, n=-1))) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod) %>%
  group_by(scenario, demand) %>%
  mutate(adj_sum = sum(meat_yr_mmt),
         rel_prod = meat_yr_mmt / adj_sum) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod)


donut_df2 <- init_pq_df %>%
  mutate(scenario = "Tech innovation (Ambitious)") %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture", "inland_production")) %>%
  mutate(sector = ifelse(sector == "marine_capture","Marine wild fisheries",
                         ifelse(sector == "inland_production", "Inland fisheries",
                                ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")))) %>%
  mutate(demand = "Initial production",
         meat_yr_mmt = quantity / 1e6) %>%
  group_by(scenario) %>%
  mutate(total_meat = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  mutate(rel_prod = meat_yr_mmt / total_meat) %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod)


donut_df3 <- rbind(donut_df, donut_df2) 

donut_df3$sector <- factor(donut_df3$sector,
                           levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))

donut_df4 <- donut_df3 %>%
  mutate(demand = ifelse(demand == "Current", "2050 under current demand",
                         ifelse(demand == "Future", "2050 under future demand",
                                ifelse(demand == "Extreme", "2050 under extreme demand", "Initial production")))) %>%
  mutate(label = paste0(round(rel_prod * 100), "%")) %>%
  group_by(demand) %>%
  arrange(desc(sector)) %>%
  # arrange(desc(sector)) %>%
  mutate(cumrp = cumsum(rel_prod),
         centers = cumrp - rel_prod / 2,
         tot_prod = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  mutate(center_text = paste(round(tot_prod),"mmt",sep=""))

max_prod <- max(donut_df4$tot_prod)

donut_df4$demand <- factor(donut_df4$demand,
                           levels = c("Initial production", "2050 under current demand", "2050 under future demand", "2050 under extreme demand"))

donut_df5 <- donut_df4 %>%
  mutate(substitute = "Perfect substitutes")


## MAKE THE OTHER DONUT FOR SI
## ------------------------------------


donut_df_ss <- final_indiv_df %>%
  group_by(demand) %>%
  mutate(meat_yr_mmt = meat_yr / 1e6,
         total_prod = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  mutate(rel_prod = meat_yr_mmt / total_prod) %>%
  arrange(scenario, demand, sector) %>%
  filter(scenario == "Tech innovation (Ambitious)") %>%
  group_by(scenario, demand) %>%
  mutate(ymax = cumsum(rel_prod),
         ymin = c(0, head(ymax, n=-1))) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt) %>%
  group_by(scenario, demand) %>%
  mutate(adj_sum = sum(meat_yr_mmt),
         rel_prod = meat_yr_mmt / adj_sum) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod)




donut_df3_ss <- rbind(donut_df_ss, donut_df2) 

donut_df3_ss$sector <- factor(donut_df3_ss$sector,
                              levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))

donut_df4_ss <- donut_df3_ss %>%
  mutate(demand = ifelse(demand == "current", "2050 under current demand",
                         ifelse(demand == "future", "2050 under future demand",
                                ifelse(demand == "extreme", "2050 under extreme demand", "Initial production")))) %>%
  mutate(label = paste0(round(rel_prod * 100), "%")) %>%
  group_by(demand) %>%
  arrange(desc(sector)) %>%
  # arrange(desc(sector)) %>%
  mutate(cumrp = cumsum(rel_prod),
         centers = cumrp - rel_prod / 2,
         tot_prod = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  mutate(center_text = paste(round(tot_prod),"mmt",sep=""))

max_prod <- max(donut_df4_ss$tot_prod)

donut_df4_ss$demand <- factor(donut_df4_ss$demand,
                              levels = c("Initial production", "2050 under current demand", "2050 under future demand", "2050 under extreme demand"))



donut_sector_df5 <- donut_df4_ss %>%
  mutate(substitute = "Separate sectors") %>%
  rbind(donut_df5) %>%
  mutate(label = ifelse(label == "0%", NA, label))

donut_sector_df5$substitute <- factor(donut_sector_df5$substitute,
                                  levels = c("Separate sectors", "Perfect substitutes"))

## note to user: change path and save the file for SI
write_csv(donut_sector_df5, "si_prod_all_sources.csv")


## Time to make the donuts
donut_si_fig <- ggplot(donut_sector_df5, aes(x=1.7, y=rel_prod, fill=sector, width = tot_prod / max_prod)) +
  geom_bar(position = 'fill', stat = 'identity', alpha = 1)  +
  # geom_label(x=2.3, aes(y=centers, label=label), fill = "none", color = "white", size=5, inherit.aes = F) +
  geom_text(x = 2.5, aes(y=centers, label = label), size = 4) +
  geom_text(x = .5,y=.8,size = 4, aes(label=center_text)) +
  facet_grid(demand ~ substitute) + 
  xlim(0.5, 2.5) +
  coord_polar(theta = 'y') + 
  scale_fill_manual(values = c("Marine wild fisheries" = "#0072B2",
                               "Bivalve mariculture" = "#009E73",
                               "Finfish mariculture" = "#E69F00",
                               "Inland fisheries" = "#D55E00"),
                    breaks = c("Marine wild fisheries", "Bivalve mariculture", "Finfish mariculture", "Inland fisheries"),
                    labels = c("Marine wild fisheries", "Bivalve mariculture", "Finfish mariculture", "Inland fisheries")) +
  labs(x=NULL, y=NULL) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## note to user: change path and save the si figure
ggsave(filename =  "si_donut.png", donut_si_fig, width = 8, height = 10, units = "in", dpi = 300)



## MAKE TABLE WITH HARVEST
## --------------------------------------

## note to user: change path and read in file init_conditions.csv
init_pq_dfh <- read_csv("init_pq_dfh.csv")

## supply curves

## note to user: change path and read in file created in sc_outputs.R
mc_supply <- read_csv("harvest_x_price_x_conversion.csv") %>%
  select(-X1) %>%
  filter(scenario == "original_inputs") %>%
  group_by(price) %>%
  summarise(mt_yr = sum(mt)) %>%
  ungroup() %>%
  mutate(sector = "Marine wild fisheries")



subst_donut_dfh <- sector_sup_df %>%
  # mutate(round_price = round(price)) %>%
  select(scenario = scen_lab, sector, demand = demand_scen, price, mt_yr, meat_yr = sector_meat_yr, total_prod = total_meat_yr) %>%
  mutate(
    # mt_yr2 = ifelse(sector == "Marine wild fisheries", mc_supply$mt_yr[match(round_price, mc_supply$price)], mt_yr),
    mt_yr = mt_yr / 1e6,
    rel_prod = meat_yr / total_prod,
    meat_yr_mmt = meat_yr,
    tot_meat_mmt = total_prod) %>%
  group_by(scenario, demand) %>%
  mutate(total_mt = sum(mt_yr, na.rm = T)) %>%
  ungroup()



donut_dfh <- subst_donut_dfh %>%
  arrange(scenario, demand, sector) %>%
  filter(scenario == "Tech innovation (Ambitious)") %>%
  group_by(scenario, demand) %>%
  mutate(ymax = cumsum(rel_prod),
         ymin = c(0, head(ymax, n=-1))) %>%
  ungroup() %>%
  select(scenario, sector, demand, mt_yr, meat_yr_mmt, rel_prod) %>%
  group_by(scenario, demand) %>%
  mutate(adj_mt_sum = sum(mt_yr, na.rm = T),
         adj_sum = sum(meat_yr_mmt),
         rel_prod = meat_yr_mmt / adj_sum) %>%
  ungroup() %>%
  select(scenario, sector, demand, mt_yr)


##
donut_dfh2 <- init_pq_dfh %>%
  mutate(scenario = "Tech innovation (Ambitious)") %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture", "inland_production")) %>%
  mutate(sector = ifelse(sector == "marine_capture","Marine wild fisheries",
                         ifelse(sector == "inland_production", "Inland fisheries",
                                ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")))) %>%
  mutate(demand = "Initial production",
         mt_yr = quantity / 1e6) %>%
  group_by(scenario) %>%
  mutate(total_meat = sum(mt_yr)) %>%
  ungroup() %>%
  mutate(rel_prod = mt_yr / total_meat) %>%
  select(scenario, sector, demand, mt_yr)


donut_df3h <- rbind(donut_dfh, donut_dfh2) 

donut_df3h$sector <- factor(donut_df3h$sector,
                           levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))

donut_df4h <- donut_df3h %>%
  mutate(demand = ifelse(demand == "Current", "2050 under current demand",
                         ifelse(demand == "Future", "2050 under future demand",
                                ifelse(demand == "Extreme", "2050 under extreme demand", "Initial production")))) %>%
  # mutate(label = paste0(round(rel_prod * 100), "%")) %>%
  group_by(demand) %>%
  arrange(desc(sector)) %>%
  mutate(tot_prod = sum(mt_yr, na.rm = TRUE)) %>%
  ungroup()

max_prodh <- max(donut_df4h$tot_prod)

donut_df4h$demand <- factor(donut_df4h$demand,
                           levels = c("Initial production", "2050 under current demand", "2050 under future demand", "2050 under extreme demand"))

donut_df5h <- donut_df4h %>%
  mutate(substitute = "Perfect substitutes")


## MAKE THE OTHER DONUT FOR SI
## ------------------------------------

supply_h <- data %>%
  filter(aq_scen == "Scenario 4c - tech advance",
         sector != "perf_substitutes") %>%
  select(sector:mt_yr) %>%
  mutate(sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Bivalve aquaculture", "Bivalve mariculture",
                                ifelse(sector == "Finfish aquaculture", "Finfish mariculture", "Inland fisheries")))) %>%
  filter(sector == "Marine wild fisheries" | sector == "Inland fisheries") %>%
  rename(round_price = price)


donut_df_ssh <- final_indiv_df %>%
  mutate(round_price = round(price)) %>%
  left_join(supply_h) %>%
  mutate(mt_yr = ifelse(sector == "Bivalve mariculture", meat_yr * 6,
                        ifelse(sector == "Finfish mariculture", meat_yr * 1.15, mt_yr)),
         h_mmt_yr = mt_yr / 1e6) %>%
  group_by(demand) %>%
  mutate(total_prod_h = sum(h_mmt_yr)) %>%
  ungroup() %>%
  # mutate(rel_prod = meat_yr_mmt / total_prod) %>%
  arrange(scenario, demand, sector) %>%
  filter(scenario == "Tech innovation (Ambitious)") %>%
  group_by(scenario, demand) %>%
  # mutate(ymax = cumsum(rel_prod),
  #        ymin = c(0, head(ymax, n=-1))) %>%
  # ungroup() %>%
  select(scenario, sector, demand, h_mmt_yr) %>%
  group_by(scenario, demand) %>%
  mutate(adj_sum = sum(h_mmt_yr)) %>%
  # ,
  #          rel_prod = meat_yr_mmt / adj_sum) %>%
  ungroup() %>%
  select(scenario, sector, demand, mt_yr = h_mmt_yr) 



donut_df3_ssh <- rbind(donut_df_ssh, donut_dfh2) 

donut_df3_ssh$sector <- factor(donut_df3_ssh$sector,
                              levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))

donut_df4_ssh <- donut_df3_ssh %>%
  mutate(demand = ifelse(demand == "current", "2050 under current demand",
                         ifelse(demand == "future", "2050 under future demand",
                                ifelse(demand == "extreme", "2050 under extreme demand", "Initial production")))) %>%
  # mutate(label = paste0(round(rel_prod * 100), "%")) %>%
  group_by(demand) %>%
  arrange(desc(sector)) %>%
  mutate(tot_prod = sum(mt_yr, na.rm = TRUE)) %>%
  ungroup()

max_prodh <- max(donut_df4_ssh$tot_prod)

donut_df4_ssh$demand <- factor(donut_df4_ssh$demand,
                              levels = c("Initial production", "2050 under current demand", "2050 under future demand", "2050 under extreme demand"))



donut_sector_df5h <- donut_df4_ssh %>%
  mutate(substitute = "Separate sectors") %>%
  rbind(donut_df5h)

donut_sector_df5h[is.na(donut_sector_df5h)] <- 0

# donut_sector_df5$substitute <- factor(donut_sector_df5$substitute,
#                                       levels = c("Separate sectors", "Perfect substitutes"))

## note to user: change path and save for the SI
write_csv(donut_sector_df5h, "si_prod_all_sources_harvest.csv")


