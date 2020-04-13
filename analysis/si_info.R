## Tracey Mangin
## December 16, 2019
## SI values

library(tidyverse)
library(scales)

## save path for figures
savepath <- "~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/"

## supply
supdatadir <- "aquaculture_v2/output"
sc_data_l <- read.csv(file.path(supdatadir, "ocean_supply_curve_scenarios1-4_feed3.csv"))

demand <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/demand/demand_curves_final.rds") %>%
  mutate(curve = "demand")

## read in csv
final_indiv_df <- read.csv("bp1_nature/results/sector_sp_prod_final.csv")
final_agg_df <- read.csv("bp1_nature/results/agg_intersect_pts.csv")

## Agg, $2 per kg, Scenario D, Extreme
high_df <- final_agg_df %>%
  filter(scen_lab == "Scenario D",
         time == "extreme") %>%
  mutate(meat_yr_mmt = meat_yr / 1e6)

tot_high <- sum(high_df$meat_yr_mmt)

## low addition
## ------------------------------------------

## sep sectors, $5 per kg, future demand, Policy
## individual supply and demand
## --------------------------------------------------

indiv_supply <- sc_data_l %>%
  filter(scenario %in% c("Scenario 2", "Scenario 3b", "Scenario 3c")) %>%
  mutate(sector = ifelse(sector == "Capture fisheries", "Wild fisheries",
                         ifelse(sector == "Bivalve aquaculture", "Bivalve mariculture", "Finfish mariculture")))

indiv_demand <- demand %>%
  filter(sector %in% c("capture", "bivalve_aq", "finfish_aq")) %>%
  mutate(sector = ifelse(sector == "capture", "Wild fisheries",
                         ifelse(sector == "bivalve_aq", "Bivalve mariculture", "Finfish mariculture")))

# sector_prices <- data.frame(sector = c(rep("Bivalve aquaculture", 1000), rep("Capture fisheries", 1000), rep("Finfish aquaculture", 1000)),
#                             price = c(rep(1700, 1000), rep(1296, 1000), rep(7000, 1000)),
#                             quantity = rep(seq(1,1000,1), 3))

indiv_supply$sector <- factor(indiv_supply$sector, levels = c("Wild fisheries", "Bivalve mariculture", "Finfish mariculture"))



indiv_supply2 <- indiv_supply %>%
  mutate(ymax = ifelse(sector == "Bivalve mariculture", 1500,
                       ifelse(sector == "Finfish mariculture", 20000, 20000)),
         xmax = ifelse(sector == "Bivalve mariculture", 100e6,
                       ifelse(sector == "Finfish mariculture", 150e6, 200e6))) %>%
  filter(price <= ymax,
         meat_yr <= xmax) %>%
  mutate(curve_type = "Supply")

# indiv_supply2$time <- factor(indiv_supply2$time, levels = c("Current", "Future", "Extreme"))
indiv_supply2$sector <- factor(indiv_supply2$sector, levels = c("Wild fisheries", "Finfish mariculture", "Bivalve mariculture"))

indiv_demand2 <- indiv_demand %>%
  mutate(ymax = ifelse(sector == "Bivalve mariculture", 1500,
                       ifelse(sector == "Finfish mariculture", 20000, 20000)),
         xmax = ifelse(sector == "Bivalve mariculture", 100e6,
                       ifelse(sector == "Finfish mariculture", 150e6, 200e6))) %>%
  filter(price <= ymax,
         quantity <= xmax) %>%
  mutate(time = ifelse(time == "current", "Current",
                       ifelse(time == "2050", "Future", "Extreme"))) %>%
  mutate(curve_type = "Demand")

indiv_demand2$time <- factor(indiv_demand2$time, levels = c("Current", "Future", "Extreme"))
indiv_demand2$sector <- factor(indiv_demand2$sector, levels = c("Wild fisheries", "Finfish mariculture", "Bivalve mariculture"))


test <- ggplot(indiv_supply2 %>% filter(scenario == "Scenario 2", sector == "Finfish mariculture"), aes(x = meat_yr / 1e6, y = price, size = curve_type)) +
  geom_path(alpha = 0.9, color = "black") +
  geom_line(data = indiv_demand2 %>% filter(time != "D-Lancet", sector == "Finfish mariculture"), aes(x = quantity / 1e6, y = price, group = time, color = time), size = 1.5, alpha = 0.8) +
  facet_wrap(~sector, scales = "free") + 
  xlab("Edible production (mmt)") +
  ylab("Price (USD per mt)") +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(name = "Demand curves: ",
                     breaks = c("Current", "Future", "Extreme"), 
                     values = c("#009966", "#3333CC", "#ff3366")) +
  scale_size_manual(name = "",
                    breaks = c("Supply"),
                    values = c(1.5),
                    labels = c("Supply curves")) +
  scale_y_continuous(label=comma) +
  # geom_hline(yintercept = pval0, lty = "dashed", color = "black") +
  # annotate("text", x = 300, y = pval0 + 75, label = paste0("Current weighted average global price = $", format(round(pval0), nsmall=0, big.mark=","), " / mt"), size = 5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "top",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 16),
        legend.box = "vertical")
## -----------------------------------
## find intersecting points
## ------------------------------------

find_sd_point_indiv <- function(scen, sdf, ddf) {
  
  ## define the price and sector curve
  demand_scen <- scen$dval
  supply_scen <- scen$sscen
  prod_sector <- scen$sector_val
  
  ## supply df
  scdf <- sdf %>%
    filter(scenario == supply_scen,
           sector == prod_sector) %>%
    mutate(x = meat_yr,
           y = price)
  
  ## demand df
  dcdf <- ddf %>%
    filter(sector == prod_sector,
           time == demand_scen) %>%
    mutate(x = quantity,
           y = price)
  
  intersect <- try({curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)})
  if (inherits(intersect, "try-error")){
    intersect <- list(y = 0, x = 0)
  } else {
    intersect <- curve_intersect(scdf, dcdf, empirical = TRUE, domain = NULL)
  }
  
  
  output <- tibble(scenario = supply_scen,
                   sector = prod_sector,
                   time = demand_scen,
                   price = intersect$y,
                   meat_yr = intersect$x)
  
}

## make df of all intersection points
demand_vec <- as.list(unique(indiv_demand$time))
# demand_vec <- as.list(c("extreme"))
supply_vec <- as.list(unique(indiv_supply$scenario))
# supply_vec <- as.list(c("Scenario 2"))
sector_names <- as.list(unique(indiv_supply$sector))
# sector_names <- as.list(c("Capture fisheries"))

low_vals <- cross(list(dval = demand_vec,
                              sscen = supply_vec,
                              sector_val = sector_names)) %>%
  map(find_sd_point_indiv,
      sdf = indiv_supply, 
      ddf = indiv_demand) %>%
  bind_rows()

### 
### do it to it
final_low_df <- low_vals %>%
  rename(demand = time) %>%
  group_by(scenario, demand) %>%
  mutate(total_prod = sum(meat_yr) / 1e6) %>%
  ungroup() %>%
  mutate(rel_prod = (meat_yr / 1e6) / total_prod,
         meat_yr_mmt = meat_yr / 1e6) %>%
  group_by(scenario, demand) %>%
  mutate(tot_meat_mmt = sum(meat_yr_mmt)) %>%
  ungroup()

## low df
low_df <- low_vals %>%
  filter(scenario == "Scenario 2",
         time == "2050") %>%
  mutate(meat_yr_mmt = meat_yr / 1e6)

## intersect is greater than 20k
low_ff_supply <- indiv_supply2 %>%
  filter(scenario == "Scenario 2",
         price == max(price),
         sector == "Finfish mariculture") %>%
  select(meat_yr) %>%
  mutate(meat_yr_mmt = meat_yr / 1e6) %>%
  select(meat_yr_mmt) %>%
  as.numeric()

## low df2 
low_df2 <- low_df %>%
  mutate(meat_yr_mmt = ifelse(sector == "Finfish mariculture", low_ff_supply, meat_yr_mmt)) %>%
  group_by(scenario) %>%
  mutate(total_meat = sum(meat_yr_mmt)) %>%
  ungroup()


sum_low <- sum(low_df$meat_yr_mmt)
