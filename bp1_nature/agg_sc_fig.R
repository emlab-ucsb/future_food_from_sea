## Tracey Mangin
## March 11, 2020
## aggregate supply curve for SI


library(tidyverse)
library(scales)
library(viridis)

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

## supply curves for main text
data <- read_rds(paste0(bp_path, "outputs/subset_supply_curves.rds"))

## initial production and price
init_pq_df <- read_csv(paste0(bp_path, "outputs/init_pq_df.csv"))


## Aggregate supply curves, scenarios A-D
## ------------------------------------------------------------------------
np_scen_vec <- c("Scenario 3 - byproducts + directed", "Scenario 4a - tech advance", "Scenario 4c - tech advance")

## get scenarios that we are interested in
data_np <- data %>%
  filter(aq_scen %in% np_scen_vec,
         !sector %in% c("perf_substitutes"),
         price <= 10000) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 


# ## agg df
# data_ind <- data %>%
#   mutate(scenario_title=plyr::revalue(scenario, c("Scenario 1"="Scenario 1:\nFeed from by-products only",
#                                                   "Scenario 2"="Scenario 2:\nFeed from by-products + whole fish",
#                                                   "Scenario 3a"="Scenario 3a:\nFeed from by-products + whole fish\n(+50% reduction in FM/FO demand)",
#                                                   "Scenario 3b"="Scenario 3b:\nFeed from by-products + whole fish\n(+75% reduction in FM/FO demand)",
#                                                   "Scenario 3c"="Scenario 3c:\nFeed from by-products + whole fish\n(+95% reduction in FM/FO demand)",
#                                                   "Scenario 4"="Scenario 4:\nFeed unconstrained\nby capture fisheries"))) %>% 
#   filter(price <= 10000,
#          scenario %in% np_scen_vec) %>%
#   mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
#          scen_lab = ifelse(scenario == "Scenario 2", "Policy reforms",
#                            ifelse(scenario == "Scenario 3a", "Tech innovation",
#                                   ifelse(scenario == "Scenario 3c", "Tech innovation (Ambitious)", "Scenario D")))) %>%
#   filter(sector != "Aggregate",
#          inl_scen == "constant_cap")

## get scenarios that we are interested in
data_perf_sub <- data %>%
  filter(aq_scen %in% np_scen_vec,
         sector == c("perf_substitutes"),
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
  facet_wrap(~scen_lab, ncol = 3, scales = "fixed") +
  coord_flip() +
  # geom_area(stat="identity") +
  scale_y_continuous(labels = comma) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  # scale_x_continuous(labels = comma, limits = c(0,10000 / sf_price)) +
  scale_fill_viridis(discrete=TRUE) +
  geom_line(data = data_perf_sub, aes(y = meat_yr / 1e6, x = price), color = "black", size = 1, inherit.aes = FALSE) +
  xlab("Price per mt") + 
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

# ggsave(filename =  paste0(bp_path, "figures/test.png"), agg_sc, width = 8, height = 5, units = "in", dpi = 300)


#################################################
### REMAKE WITHOUT INLAND FISHERIES
################################################


## Aggregate supply curves, scenarios A-D
## ------------------------------------------------------------------------
np_scen_vec <- c("Scenario 3 - byproducts + directed", "Scenario 4a - tech advance", "Scenario 4c - tech advance")

## get scenarios that we are interested in
data_indiv_sea <- data %>%
  filter(aq_scen %in% np_scen_vec,
         !sector %in% c("perf_substitutes", "Inland"),
         price <= 10000) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture"),
         sector = ifelse(sector == "Capture fisheries", "Marine wild fisheries",
                         ifelse(sector == "Inland", "Inland fisheries", sector)),
         scen_lab = ifelse(aq_scen == "Scenario 3 - byproducts + directed", "Policy reforms",
                           ifelse(aq_scen == "Scenario 4a - tech advance", "Tech innovation",
                                  ifelse(aq_scen == "Scenario 4c - tech advance", "Tech innovation (Ambitious)", aq_scen)))) 


## get scenarios that we are interested in
data_agg_sea <- data_indiv_sea %>%
  select(sector, scen_lab, price, meat_yr) %>%
  group_by(scen_lab, price) %>%
  mutate(meat_yr = sum(meat_yr)) %>%
  ungroup()


data_indiv_sea$sector <- factor(data_indiv_sea$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Marine wild fisheries"))
data_agg_sea$sector <- factor(data_agg_sea$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Marine wild fisheries"))


## fig
agg_sc2 <- ggplot(data_indiv_sea, aes(y = meat_yr / 1e6, x = price, fill = sector), alpha = 0.9) +
  geom_area(stat = "identity") +
  # geom_vline(xintercept = sf_price, lty = 2) +
  facet_wrap(~scen_lab, ncol = 3, scales = "fixed") +
  coord_flip() +
  # geom_area(stat="identity") +
  scale_y_continuous(labels = comma) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  # scale_x_continuous(labels = comma, limits = c(0,10000 / sf_price)) +
  scale_fill_viridis(discrete=TRUE) +
  geom_line(data = data_agg_sea, aes(y = meat_yr / 1e6, x = price), color = "black", size = 1.5, inherit.aes = FALSE) +
  xlab("Price per mt") + 
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

# ggsave(filename =  paste0(bp_path, "figures/si_1a.png"), agg_sc2, width = 8, height = 5, units = "in", dpi = 300)
