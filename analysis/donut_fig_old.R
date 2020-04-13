## Tracey Mangin
## December 16, 2019
## America Runs on Dunkin' Donut Figures

library(tidyverse)
library(scales)

## save path for figures
savepath <- "~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/"

## read in csv
final_indiv_df <- read.csv("bp1_nature/results/sector_sp_prod_final.csv")


donut_df <- final_indiv_df %>%
  mutate(meat_yr_mmt = meat_yr / 1e6) %>%
  arrange(scenario, demand, sector) %>%
  filter(scenario == "Scenario 3c") %>%
  group_by(scenario, demand) %>%
  mutate(ymax = cumsum(rel_prod),
         ymin = c(0, head(ymax, n=-1))) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod) %>%
  mutate(meat_yr_mmt = ifelse(sector == "Wild fisheries" & demand == "extreme", 62.1, meat_yr_mmt)) %>%
  group_by(scenario, demand) %>%
  mutate(adj_sum = sum(meat_yr_mmt),
         rel_prod = meat_yr_mmt / adj_sum) %>%
  ungroup() %>%
  select(scenario, sector, demand, meat_yr_mmt, rel_prod)

donut_df2 <- tibble(scenario = rep("Scenario 3c", 3),
                    sector = c("Wild fisheries", "Bivalve mariculture", "Finfish mariculture"),
                    demand = rep("Initial production", 3),
                    meat_yr_mmt = c(51.2, 2.8, 7.6),
                    rel_prod = c(51.2 / 61.6, 2.8/61.6, 7.6/ 61.6))

donut_df3 <- rbind(donut_df, donut_df2) %>%
  mutate(demand = ifelse(demand == "current", "Current",
                         ifelse(demand == "2050", "Future",
                                ifelse(demand == "extreme", "Extreme", "Initial production")))) %>%
  mutate(meat_yr_mmt = ifelse(sector == "Wild fisheries" & demand == "Extreme", 62.1, meat_yr_mmt),
         label = paste0(round(rel_prod * 100), "%")) %>%
  group_by(demand) %>%
  arrange(desc(sector)) %>%
  mutate(cumrp = cumsum(rel_prod),
         centers = cumrp - rel_prod / 2,
         tot_prod = sum(meat_yr_mmt)) %>%
  ungroup() 

max_prod <- max(donut_df3$tot_prod)

donut_df3$demand <- factor(donut_df3$demand,
                           levels = c("Initial production", "Current", "Future", "Extreme"))

donut_fig <- ggplot(donut_df3, aes(x=2, y=rel_prod, fill=sector, width = tot_prod / max_prod)) +
  geom_bar(position = 'fill', stat = 'identity', alpha = 0.9)  +
  # geom_label(x=2.3, aes(y=centers, label=label), fill = "none", color = "white", size=5, inherit.aes = F) +
  geom_text(x = 2.5, aes(y=centers, label = label), size = 6) +
  facet_wrap(~ demand) + 
  xlim(0.5, 2.5) +
  coord_polar(theta = 'y') + 
  scale_fill_manual(values = c("Wild fisheries" = "darkblue",
                               "Bivalve mariculture" = "#024b30",
                               "Finfish mariculture" = "darkred")) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


ggsave(filename =  paste0(savepath, "donut_fig.png"), donut_fig, width = 10, height = 10, units = "in", dpi = 300)
