## Tracey Mangin
## December 16, 2019
## America Runs on Dunkin' Donut Figures

## Future food from the sea
## this script creates fig 5 in the main text and relevant information

library(tidyverse)
library(scales)

## read in csv
## note to user: change path and read in file saved in sector_sp_outputs.R
final_indiv_df <- read_csv("sector_sp_prod_final.csv") %>%
  filter(sector != "Inland fisheries") %>%
  select(scenario:meat_yr)

## initial production and price
## note to user: change path, load file created in init_conditions.R
init_pq_df <- read_csv("init_pq_df.csv")

## initial production and price, harvest
## note to user: change path, load file created in init_conditions.R
init_pq_df <- read_csv(paste0(bp_path, "outputs/init_pq_df.csv"))



donut_df <- final_indiv_df %>%
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


donut_df2 <- init_pq_df %>%
  mutate(scenario = "Tech innovation (Ambitious)") %>%
  filter(sector %in% c("marine_capture", "finfish_mariculture", "bivalve_mariculture")) %>%
  mutate(sector = ifelse(sector == "marine_capture","Marine wild fisheries",
                         ifelse(sector == "inland_production", "Inland fisheries",
                                ifelse(sector == "finfish_mariculture", "Finfish mariculture", "Bivalve mariculture")))) %>%
  mutate(demand = "Initial Production",
         meat_yr_mmt = quantity / 1e6) %>%
  group_by(scenario) %>%
  mutate(total_meat = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  mutate(rel_prod = meat_yr_mmt / total_meat) %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod)

# donut_df2 <- tibble(scenario = rep("Scenario 3c", 4),
#                     sector = c("Marine wild fisheries", "Bivalve mariculture", "Finfish mariculture", "Inland fisheries"),
#                     demand = rep("Initial Production", 4),
#                     meat_yr_mmt = c(51.2, 2.8, 7.6),
#                     rel_prod = c(51.2 / 61.6, 2.8/61.6, 7.6/ 61.6))


donut_df3 <- rbind(donut_df, donut_df2) 

donut_df3$sector <- factor(donut_df3$sector,
                           levels = c("Marine wild fisheries", "Finfish mariculture", "Bivalve mariculture"))

donut_df4 <- donut_df3 %>%
  mutate(demand = ifelse(demand == "current", "b 2050 under current demand",
                         ifelse(demand == "future", "c 2050 under future demand",
                                ifelse(demand == "extreme", "d 2050 under extreme demand", "a Initial production")))) %>%
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
                           levels = c("a Initial production", "b 2050 under current demand", "c 2050 under future demand", "d 2050 under extreme demand"))


donut_fig <- ggplot(donut_df4, aes(x = 1.7, y=rel_prod, fill=sector, width = tot_prod / max_prod)) +
  geom_bar(position = 'fill', stat = 'identity', alpha = 1)  +
  # geom_label(x=2.3, aes(y=centers, label=label), fill = "none", color = "white", size=5, inherit.aes = F) +
  geom_text(x = 2.5, aes(y=centers, label = label), size = 3) +
  geom_text(x = .5, y=.8, size = 3,aes(label=center_text)) +
  facet_wrap(~ demand) + 
  xlim(0.5, 2.5) +
  coord_polar(theta = 'y') + 
  scale_fill_manual(values = c("Marine wild fisheries" = "#0072B2",
                               "Bivalve mariculture" = "#009E73",
                               "Finfish mariculture" = "#E69F00",
                               "Inland fisheries" = "#D55E00")) +
  labs(x=NULL, y=NULL) +
  # guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 8, hjust = 0),
        strip.background = element_rect(color = "white", fill = "white"))

## note to user -- change path and save figure 5 in main text
ggsave(filename = "fig5.pdf", donut_fig, width = 120, height = 120, units = "mm", dpi = 600)



## compare outputs to initial
init_total_prod <- donut_df4 %>%
  filter(demand == "Initial Production") %>%
  select(init_tot_prod = tot_prod) %>%
  unique() %>%
  as.numeric()

compare_df <- final_indiv_df %>%
  group_by(demand) %>%
  mutate(meat_yr_mmt = meat_yr / 1e6,
         total_prod = sum(meat_yr_mmt)) %>%
  ungroup() %>%
  select(demand, total_prod) %>%
  unique() %>%
  mutate(diff = total_prod - init_total_prod,
         prod_vs_init = (total_prod - init_total_prod) / init_total_prod)
