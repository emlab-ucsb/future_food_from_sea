## Tracey Mangin
## December 10, 2019
## sector specific outcomes

## future food from the sea
## note to user: must run scripts in init_cond, capture_fisheries, and aquaculture folder first (outputs needed).
## this creates figure 4 in the main text and other relevant outputs.

library(tidyverse)
library(scales)
library(reconPlots)


## supply curves for main text
## note to user: change path, load file created in all_supply_curves.R
data <- read_rds("subset_supply_curves.rds")

## initial production and price
## note to user: change path, load file created in init_conditions.R
init_pq_df <- read_csv("init_pq_df.csv")

## initial production and price, harvest
## note to user: change path, load file created in init_conditions.R
init_pq_dfh <- read_csv(paste0(bp_path, "outputs/init_pq_dfh.csv"))


## demand
## note to user: change path, load file created in demand_curves.R
demand_df <- read_rds("demand_curves_final.rds")

## nature paper scenarios
np_scen_vec <- c("Scenario 3 - byproducts + directed", "Scenario 4a - tech advance", "Scenario 4c - tech advance")

## get scenarios that we are interested in
data_np <- data %>%
  filter(aq_scen %in% np_scen_vec,
         !sector %in% c("perf_substitutes")) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries", 
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 




## individual supply and demand
## --------------------------------------------------

indiv_demand <- demand_df %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture", "inland_production")) %>%
  mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
                                ifelse(sector == "finfish_mariculture", "Finfish mariculture", 
                                       ifelse(sector == "inland_production", "Inland fisheries", "Bivalve mariculture"))))

indiv_supply2 <- data_np %>%
  mutate(ymax = ifelse(sector == "Bivalve mariculture", 2000,
                       ifelse(sector == "Finfish mariculture", 6000, 
                              ifelse(sector == "Marine wild fisheries", 20000, 10000))),
         xmax = ifelse(sector == "Bivalve mariculture", 100e6,
                       ifelse(sector == "Finfish mariculture", 200e6, 
                              ifelse(sector == "Marine wild fisheries", 200e6, 200e6)))) %>%
  filter(price <= ymax,
         meat_yr <= xmax) %>%
  mutate(curve_type = "Supply",
         sector_lab = ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                             ifelse(sector == "Bivalve mariculture", paste0("c ", sector), paste0("b ", sector))))


indiv_supply2$sector <- factor(indiv_supply2$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))


indiv_demand2 <- indiv_demand %>%
  rename(meat_yr = quantity) %>%
  mutate(ymax = ifelse(sector == "Bivalve mariculture", 2000,
                       ifelse(sector == "Finfish mariculture", 6000, 
                              ifelse(sector == "Marine wild fisheries", 20000, 10000))),
         xmax = ifelse(sector == "Bivalve mariculture", 100e6,
                       ifelse(sector == "Finfish mariculture", 200e6, 
                              ifelse(sector == "Marine wild fisheries", 200e6, 200e6)))) %>%
  filter(price <= ymax,
         meat_yr <= xmax) %>%
  mutate(demand_scen = ifelse(demand_scen == "current", "Current",
                              ifelse(demand_scen == "future", "Future", "Extreme"))) %>%
  mutate(curve_type = "Demand",
         sector_lab = ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                             ifelse(sector == "Bivalve mariculture", paste0("c ", sector), paste0("b ", sector))))

indiv_demand2$demand_scen <- factor(indiv_demand2$demand_scen, levels = c("Current", "Future", "Extreme"))
indiv_demand2$sector <- factor(indiv_demand2$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))


init_prod_pts <- init_pq_df %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture")) %>%
  mutate(sector = ifelse(sector == "marine_capture", "Marine wild fisheries",
                         ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")))

init_prod_pts$sector <- factor(init_prod_pts$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))


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
    mutate(x = quantity,
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
demand_vec <- as.list(unique(indiv_demand$demand_scen))
# demand_vec <- as.list(c("extreme"))
supply_vec <- as.list("Tech innovation (Ambitious)")
# supply_vec <- as.list(c("Scenario 2"))
sector_names <- as.list(unique(data_np$sector))
# sector_names <- as.list(c("Capture fisheries"))

indiv_prod_vals <- cross(list(dval = demand_vec,
                              sscen = supply_vec,
                              sector_val = sector_names)) %>%
  map(find_sd_point_indiv,
      sdf = data_np, 
      ddf = indiv_demand) %>%
  bind_rows()

### 
## fill in missing value for wild extreme

capture_prod <- data_np %>%
  filter(sector == "Marine wild fisheries",
         scen_lab == "Tech innovation (Ambitious)")

max_wild_prod <- max(capture_prod$meat_yr)

### do it to it
final_indiv_df <- indiv_prod_vals %>%
  mutate(meat_yr = ifelse(sector == "Marine wild fisheries" & demand_scen == "extreme", max_wild_prod, meat_yr),
         price = ifelse(sector == "Marine wild fisheries" & demand_scen == "extreme", 20000, price)) %>%
  rename(demand = demand_scen) %>%
  group_by(scenario, demand) %>%
  mutate(total_prod = sum(meat_yr) / 1e6) %>%
  ungroup() %>%
  mutate(rel_prod = (meat_yr / 1e6) / total_prod,
         meat_yr_mmt = meat_yr / 1e6) %>%
  group_by(scenario, demand) %>%
  mutate(tot_meat_mmt = sum(meat_yr_mmt)) %>%
  ungroup()

## note to user: change path, save file for use in donut_fig.R
write_csv(final_indiv_df, "sector_sp_prod_final.csv")

## add mt
mt_df <- data_np %>%
  filter(sector %in% c("Marine wild fisheries", "Inland fisheries")) %>%
  select(scenario = scen_lab, sector, price, mt_yr, food_yr = meat_yr) %>%
  filter(price %in% unique(round(final_indiv_df$price))) %>%
  rename(round_price = price)

## get LWE values, take capture from supply curve file, calculate for finfish mariculture and bivalve mariculture using conversion rate
final_indiv_df_mt <- final_indiv_df %>%
  mutate(round_price = round(price)) %>%
  left_join(mt_df) %>%
  mutate(mt_yr = ifelse(sector == "Finfish mariculture", meat_yr * 1.15,
                        ifelse(sector == "Bivalve mariculture", meat_yr * 6, mt_yr)),
         mmt_yr = mt_yr / 1e6,
         food_yr = food_yr / 1e6) %>%
  group_by(scenario, demand) 

tot_harvest <- final_indiv_df_mt %>%
  filter(sector != "Inland fisheries") %>%
  group_by(scenario, demand) %>%
  mutate(total_h = sum(mmt_yr),
         tot_marine_meat = sum(meat_yr_mmt)) %>%
  ungroup()


###


final_indiv_df2 <- final_indiv_df %>%
  mutate(demand_scen = ifelse(demand == "current", "Current",
                              ifelse(demand == "extreme", "Extreme", "Future")),
         sector_lab = ifelse(sector == "Marine wild fisheries", paste0("a ", sector),
                             ifelse(sector == "Bivalve mariculture", paste0("c ", sector), paste0("b ", sector))))


final_indiv_df2$sector <- factor(final_indiv_df2$sector, levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture", "Inland fisheries"))

## make df for Xs on figure
## --------------------------------------


fig4 <- ggplot(indiv_supply2 %>% filter(aq_scen == "Scenario 4c - tech advance",
                                                    sector != "Inland fisheries"), aes(x = meat_yr / 1e6, y = price, size = curve_type)) +
  geom_path(alpha = 0.9, color = "black") +
  geom_line(data = indiv_demand2 %>% filter(sector != "Inland fisheries"), aes(x = meat_yr / 1e6, y = price, group = demand_scen, color = demand_scen), size = 0.6, alpha = 0.8) +
  facet_wrap(~sector_lab, scales = "free") + 
  geom_point(data = init_prod_pts, aes(x = quantity / 1e6, y = price), color = "black", size = 1) +
  geom_point(data = final_indiv_df2 %>% filter(sector != "Inland fisheries"), aes(x = meat_yr / 1e6, y = price, color = demand_scen), shape = 4, size = 2) +
  xlab("Edible production (mmt)") +
  ylab("Price (USD per mt)") +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(name = "Demand curves: ",
                     breaks = c("Current", "Future", "Extreme"), 
                     values = c("#009966", "#3333CC", "#ff3366")) +
  scale_size_manual(name = "",
                    breaks = c("Supply"),
                    values = c(0.6),
                    labels = c("Supply curves")) +
  scale_y_continuous(label=comma) +
  guides(colour = guide_legend(override.aes=list(shape=NA))) +
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
        legend.box = "vertical",
        plot.margin = margin(10, 30, 10, 10),
        strip.text = element_text(size = 8, hjust = 0),
        strip.background = element_rect(color = "white", fill = "white"),
        legend.spacing.y = unit(-2, "mm"))

## note to user: change path, save fig 4 in main text
ggsave(filename = "fig4.pdf", fig4, width = 136, height = 70, units = "mm", dpi = 600)



