## Tracey Mangin
## December 5, 2019
## Mariculture and aggregate supply curves 

library(tidyverse)
library(janitor)
library(scales)
library("wesanderson")
library(plotly)
library(viridis)
library(cowplot)

# Directories
supdatadir <- "aquaculture_v2/output"

## save path for figures
savepath <- "~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/"

## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/"


# Read data
# data <- read.csv(file.path(supdatadir, "ocean_supply_curve_scenarios1-4.csv"))
data <- read.csv(file.path(supdatadir, "ocean_supply_curve_scenarios1-4_feed1.csv"))


## current supply curve
mari_prod <- read_csv("~/GitHub/blue_paper_1/processed_data/current_mari_prod.csv")

## upsides
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)


# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  strip.text.x=element_text(size=6.5),
                  legend.text = element_text(size=7),
                  legend.title = element_text(size=9),
                  legend.position = "bottom",
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Add Scneario title
data <- data %>% 
  mutate(scenario_title=plyr::revalue(scenario, c("Scenario 1"="Scenario 1:\nFeed from by-products only",
                                                  "Scenario 2"="Scenario 2:\nFeed from by-products + whole fish",
                                                  "Scenario 3a"="Scenario 3a:\nFeed from by-products + whole fish\n(+50% reduction in FM/FO demand)",
                                                  "Scenario 3b"="Scenario 3b:\nFeed from by-products + whole fish\n(+75% reduction in FM/FO demand)",
                                                  "Scenario 3c"="Scenario 3c:\nFeed from by-products + whole fish\n(+95% reduction in FM/FO demand)",
                                                  "Scenario 4"="Scenario 4:\nFeed unconstrained\nby capture fisheries"))) %>% 
  filter(price<=10000)

## nature paper scenarios
np_scen_vec <- c("Scenario 2", "Scenario 3a", "Scenario 3c", "Scenario 4")

## get scenarios that we are interested in
data_np <- data %>%
  filter(scenario %in% np_scen_vec) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture")) %>%
  mutate(scen_lab = ifelse(scenario == "Scenario 2", "Policy reforms",
                           ifelse(scenario == "Scenario 3a", "Tech innovation",
                                  ifelse(scenario == "Scenario 3c", "Tech innovation\n(Ambitious)", "Scenario D"))))

data_np$sector <- factor(data_np$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Capture fisheries"))

## check capture numbers, just in case
capt_prod <- data_np %>%
  filter(price == 1296,
         sector == "Capture fisheries")

## Fed mariculture supply curves
## ------------------------------------------------------------------------

## curent production as a dot on graph
## assuming current feed assumptions
## b and c tech

## current production
init_fed_prod <- mari_prod %>%
  filter(sector == "mariculture",
         indicator == "food",
         grp == "fed") %>%
  select(grp, grp_sum) %>%
  unique() %>%
  select(grp_sum) %>%
  as.numeric()
  
## current price
init_price <- 7000 ## current price for salmon

init_finprod <- tibble(scen_lab = "current",
                       price = init_price,
                       meat_yr = init_fed_prod)

init_fed_prod2 <- init_fed_prod / 1e6

init_df_ff <- tibble(price = init_price,
                  total_ed = init_fed_prod2,
                  scenario = "current")

init_df_sup_ff <- tibble(price = seq(0, init_price, 1),
                      total_ed = rep(init_fed_prod2, length(seq(0, init_price, 1))),
                      scen_lab = rep("current", length(seq(0, init_price, 1))))

init_df_dem_ff <- tibble(price = rep(init_price, length(seq(0, init_fed_prod2, 1))),
                      total_ed = (seq(0, init_fed_prod2, 1)),
                      scen_lab = rep("current", length(seq(0, init_fed_prod2, 1))))


# ## fed production, three scenarios with point
# ggplot(init_df_sup_ff, aes(x = total_ed, y = price)) +
#   geom_line(lty = "dashed", color = "black") +
#   geom_line(data = init_df_dem_ff, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5)
# 
# 
# ggplot(data_np %>% filter(sector == "Finfish mariculture",
#                           scenario != "Scenario 4"), aes(x = meat_yr / 1e6, y = price, group = scen_lab, color = scen_lab)) +
#   geom_path(size = 1.5, alpha = 0.9) +
#   geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
#   geom_point(data = init_finprod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "black", inherit.aes = F) +
#   geom_line(data = init_df_sup_ff, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5) +
#   geom_line(data = init_df_dem_ff, aes(x = total_ed, y = price, lty = "dashed"), color = "black", size = 1.5)



## policy reform = gold "#E69F00"
## tech = green #009E73
## tech 2 #004F39


fed_sc_fig <- ggplot(data_np %>% filter(sector == "Finfish mariculture",
                                        scenario != "Scenario 4"), aes(x = meat_yr / 1e6, y = price, group = scen_lab, color = scen_lab)) +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_finprod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "black", inherit.aes = F) +
  geom_line(data = init_df_sup_ff, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5) +
  geom_line(data = init_df_dem_ff, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5) +
  ylab(" ") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation\n(Ambitious)" = "#004F39")) +
  scale_y_continuous(label=comma,
                     limits = c(3000,7500),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 210)) +
  # ylab("Price (USD per mt)") +
  xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        title = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.4, 0.8),
        # legend.position = "none",
        legend.text = element_text(size = 18),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  paste0(savepath, "finfish_sc.png"), fed_sc_fig, width = 8, height = 8, units = "in", dpi = 300)

# test <- ggplot(data_np %>% filter(sector == "Finfish mariculture", scenario == "Scenario 3c"), aes(x = meat_yr / 1e6, y = price, group = scenario, color = scenario)) +
#   geom_path(size = 1, alpha = 0.9) +
#   geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") 

## max production under scenario A
scen_a <- data_np %>%
  filter(sector == "Finfish mariculture",
         scenario == "Scenario 2")

## max production under current feed assumptions
max(scen_a$meat_yr)

## difference in current vs potential
pot_cp <- scen_a %>% filter(price == init_price) %>%
  select(meat_yr) %>%
  as.numeric()

pot_cp - init_fed_prod

## production potential at current prices
tech_prod_pot <- data_np %>% filter(price == init_price, sector == "Finfish mariculture") %>%
  mutate(meat_year_mmt = meat_yr / 1e6) %>%
  mutate(curr_prod = init_fed_prod,
         delta_perc = (meat_yr - curr_prod) / curr_prod * 100)


## Unfed mariculture supply curves
## ------------------------------------------------------------------------

## curent production as a dot on graph
## assuming current feed assumptions
## b and c tech

## current production
init_unfed_prod <- mari_prod %>%
  filter(sector == "mariculture",
         sub_cat == "molluscs",
         indicator == "food") %>%
  select(sub_cat, val) %>%
  unique() %>%
  select(val) %>%
  as.numeric()

## current price
init_price_uf <- 1700 ## current price for mussels

init_biv_prod <- tibble(scen_lab = "current",
                       price = init_price_uf,
                       meat_yr = init_unfed_prod)

init_df_sup_b <- tibble(price = seq(0, init_price_uf, 1),
                         total_ed = rep(init_unfed_prod, length(seq(0, init_price_uf, 1))),
                         scen_lab = rep("current", length(seq(0, init_price_uf, 1))))

init_df_dem_b <- tibble(price = rep(init_price_uf, length(seq(0, init_unfed_prod, 1))),
                         total_ed = (seq(0, init_unfed_prod, 1)),
                         scen_lab = rep("current", length(seq(0, init_unfed_prod, 1))))


## fed production, three scenarios with point

unfed_sc_fig <- ggplot(data_np %>% filter(sector == "Bivalve mariculture",
                                        scenario == "Scenario 2"), aes(x = meat_yr / 1e6, y = price, group = scen_lab, color = scen_lab)) +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_biv_prod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "black", inherit.aes = F) +
  geom_line(data = init_df_sup_b, aes(x = total_ed / 1e6, y = price), lty = "dashed", color = "black", size = 1.5) +
  geom_line(data = init_df_dem_b, aes(x = total_ed / 1e6, y = price), lty = "dashed", color = "black", size = 1.5) +
  xlab(" ") +
  ylab(" ") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation (Ambitious)" = "#004F39")) +
  scale_y_continuous(label=comma,
                     limits = c(500,2700),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 110)) +
  # ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        title = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  paste0(savepath, "bivalve_sc.png"), unfed_sc_fig, width = 8, height = 8, units = "in", dpi = 300)


## cowplot save
sc_figs <- plot_grid(wc_ed_sc_fig, fed_sc_fig, unfed_sc_fig, labels = c("A", "B", "C"), ncol = 3)

ggsave(filename =  paste0(savepath, "sector_sup_curves.png"), sc_figs, width = 15, height = 8, units = "in", dpi = 300)




## production potential under current price
scen_a_unfed <- data_np %>%
  filter(sector == "Bivalve mariculture",
         scenario == "Scenario 2")

## difference in current vs potential
pot_biv <- scen_a_unfed %>% filter(price == init_price_uf) %>%
  select(meat_yr) %>%
  as.numeric()

pot_biv - init_unfed_prod
pot_biv / init_unfed_prod

## production potential at current prices
tech_prod_pot <- data_np %>% filter(price == init_price, sector == "Finfish mariculture") %>%
  mutate(meat_year_mmt = meat_yr / 1e6) %>%
  mutate(curr_prod = init_fed_prod,
         delta_perc = (meat_yr - curr_prod) / curr_prod * 100)

## Aggregate supply curves, scenarios A-D
## ------------------------------------------------------------------------

## caluclute weighted median price of seafood
##----------------------------------------------------

## current (2012) harvest
## filter for starting year (2012)
upsides0 <- upsides %>%
  filter(Year == 2012)

## Make verison with all stock AND weighted mean price today
wm_price0 <- weighted.mean(upsides0$Price, upsides0$Catch)

upsides0_adj <- upsides0 %>%
  mutate(Price = ifelse(SciName == "Engraulis ringens", 250, Price))

wm_price_adj <- weighted.mean(upsides0_adj$Price, upsides0_adj$Catch)

sum_ups_h <- sum(upsides0$Catch) / 0.78 ## total harvest scaled to represent globe

harv_fed <- mari_prod %>% filter(cat == 'mariculture', indicator == "value", grp == "fed") %>%
  select(grp_sum) %>%
  unique() %>%
  as.numeric()

harv_unfed <- mari_prod %>% filter(cat == 'mariculture', indicator == "value", sub_cat == "molluscs") %>%
  select(grp_sum) %>%
  unique() %>%
  as.numeric()

price_df <- tibble(sector = c("Caputre fisheries", "Bivalve mariculture", "Finfish mariculture"),
                   price = c(wm_price_adj, init_price_uf, init_price),
                   production = c(sum_ups_h, harv_unfed, harv_fed))

sf_price <- weighted.mean(price_df$price, price_df$production)

## agg line
agg_sc_df <- data_np %>%
  group_by(scenario, scen_lab, price) %>%
  summarise(meat_yr = sum(meat_yr)) %>%
  ungroup()



## fig
agg_sc <- ggplot(data_np %>% filter(scen_lab != "Scenario D"), aes(y = meat_yr / 1e6, x = price, fill = sector), alpha = 0.9) +
  geom_area(stat = "identity") +
  # geom_vline(xintercept = sf_price, lty = 2) +
  facet_wrap(~scen_lab, ncol = 3, scales = "fixed") +
  coord_flip() +
  # geom_area(stat="identity") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma, limits = c(0,10000)) +
  scale_fill_viridis(discrete=TRUE) +
  geom_line(data = agg_sc_df %>% filter(scen_lab != "Scenario D"), aes(y = meat_yr / 1e6, x = price), color = "black", size = 2.5, inherit.aes = FALSE) +
  xlab("Price (USD per mt)") + 
  ylab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

ggsave(filename =  paste0(savepath, "agg_sc.png"), agg_sc, width = 8, height = 5, units = "in", dpi = 300)

## at global mean price
## ----------------------

prod2 <- data_np %>%
  filter(price == round(sf_price))





