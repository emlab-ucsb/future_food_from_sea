## Tracey Mangin
## March 16, 2020
## Make sensitivity plot

library(tidyverse)
library(ggridges)

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

## read in the sensitivity analysis outputs
sa_df <- read_csv(paste0(bp_path, "outputs/ss_analysis/all_si_outputs.csv"))

## capture
capture_df <- sa_df %>%
  filter(sub_scen == "separate_sectors",
         sector == "Marine wild fisheries") %>%
  select(cap_scen, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

capture_df$x <- paste0(1:nrow(capture_df), "-capture")

## inland 
inland_df <- sa_df %>%
  filter(sub_scen == "separate_sectors",
         sector == "Inland fisheries") %>%
  select(inl_scen, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

inland_df$x <- paste0(1:nrow(inland_df), "-inland")

## bivalve 
bivalve_df <- sa_df %>%
  filter(sub_scen == "separate_sectors",
         sector == "Bivalve mariculture") %>%
  select(aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>%
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

bivalve_df$x <- paste0(1:nrow(bivalve_df), "-bivalve")

## finfish
finfish_df <- sa_df %>%
  filter(sub_scen == "separate_sectors",
         sector == "Finfish mariculture") %>%
  select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>%
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

finfish_df$x <- paste0(1:nrow(finfish_df), "-finfish")

## aggregate
total_ss_df <- sa_df %>%
  filter(sub_scen == "separate_sectors",
         sector == "Total") %>%
  select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

total_ss_df$x <- paste0(1:nrow(total_ss_df), "-total")

## rbind
ss_df <- rbind(total_ss_df, finfish_df, bivalve_df, inland_df, capture_df)

## perfect substitute scenario
## --------------------------------------------
ps_df <- sa_df %>%
  filter(sub_scen == "perfect_substitutes")

## capture perf sub
capture_df_ps <- ps_df %>%
  filter(sector == "Marine wild fisheries") %>%
  select(cap_scen, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

capture_df_ps$x <- paste0(1:nrow(capture_df_ps), "-ps-capture")

## inland 
inland_df_ps <- ps_df %>%
  filter(sector == "Inland fisheries") %>%
  select(inl_scen, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

inland_df_ps$x <- paste0(1:nrow(inland_df_ps), "-ps-inland")

## bivalve 
bivalve_df_ps <- ps_df %>%
  filter(sector == "Bivalve mariculture") %>%
  select(aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>%
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

bivalve_df_ps$x <- paste0(1:nrow(bivalve_df_ps), "-ps-bivalve")

## finfish
finfish_df_ps <- ps_df %>%
  filter(sector == "Finfish mariculture") %>%
  select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>%
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

finfish_df_ps$x <- paste0(1:nrow(finfish_df_ps), "-ps-finfish")

## aggregate
total_ss_df_ps <- ps_df %>%
  filter(sector == "Total") %>%
  select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr) %>%
  unique() %>% 
  select(sub_scen, demand_scen, sector, price, meat_yr, meat_mmt_yr)

total_ss_df_ps$x <- paste0(1:nrow(total_ss_df_ps), "-ps-total")

## rbind
ps_df <- rbind(total_ss_df_ps, finfish_df_ps, bivalve_df_ps, inland_df_ps, capture_df_ps)


## rbind
ss_all <- rbind(ps_df, ss_df) %>%
  mutate(sub_scen = ifelse(sub_scen == "separate_sectors", "Independent", "Substitutes"))


## ggridges
splot <- ggplot(ss_all, aes(x = meat_mmt_yr, y = sector, color = sub_scen, point_color = sub_scen, fill = sub_scen)) +
  geom_density_ridges(jittered_points = TRUE, 
                      scale = .95, 
                      rel_min_height = .01,
                      point_shape = "|",
                      point_size = 1,
                      size = 0.25,
                      position = position_points_jitter(height = 0),
                      alpha = 0.6) +
   # scale_y_discrete(expand = c(0, 0)) +
   scale_x_continuous(expand = c(0, 0), name = "Food production (mmt)", limits = c(0,350)) +
   scale_fill_manual(values = c("#D55E0050", "#0072B250")) +
   scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
   scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
   coord_cartesian(clip = "off") +
  
   guides(fill = guide_legend(title="Demand", override.aes = list(fill = c("#D55E00A0", "#0072B2A0"),
          color = NA, point_color = NA))) +
          theme_ridges(center = TRUE) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

ggsave(filename =  paste0(bp_path, "figures/si_ridge.png"), splot, width = 10, height = 6, units = "in", dpi = 300)

# ## get rid of stuff below zero
# 
# ggplot(ss_all, aes(meat_mmt_yr, sector, color = sub_scen, point_color = sub_scen, fill = sub_scen, height = ..density..)) +
#   geom_density_ridges(stat = "binline", 
#                       binwidth = 1,
#                       draw_baseline = F,
#                       scale = 0.95,
#                       size = 0.5,
#                       alpha = 0.6) +
#   scale_x_continuous(expand = c(0, 0), name = "Food production (mmt)") +
#   scale_fill_manual(values = c("#D55E0050", "#0072B250")) +
#   scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
#   scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
#   coord_cartesian(clip = "off") +
#   guides(fill = guide_legend(title="Demand", override.aes = list(fill = c("#D55E00A0", "#0072B2A0"),
#                                                                  color = NA, point_color = NA))) +
#   theme_ridges(center = TRUE) +
#   theme(axis.title.y = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "top")



## get the mean for each group
## ---------------------------------------------------

mean_df <- ss_all %>%
  group_by(sub_scen, sector) %>%
  summarise(mean_food_prod = mean(meat_mmt_yr)) %>%
  ungroup()

write_csv(mean_df, paste0(bp_path, "text_tables/mean_ridges.csv"))




# Plot= ggplot(data=DFtmp, aes(x = Production, y = Sector, color = as.factor(PerfSubstitutes), point_color = as.factor(PerfSubstitutes), fill = as.factor(PerfSubstitutes))) +
#   geom_density_ridges(
#     jittered_points = TRUE, scale = .95, rel_min_height = .01,
#     point_shape = "|", point_size = 3, size = 0.25,
#     position = position_points_jitter(height = 0)
#   ) +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0), name = "Production (mmt)") +
#   scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Independent", "Substitutes")) +
#   scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
#   scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
#   coord_cartesian(clip = "off") +
#   guides(fill = guide_legend(
#     title="Demand",
#     override.aes = list(
#       fill = c("#D55E00A0", "#0072B2A0"),
#       color = NA, point_color = NA)
#   )
#   ) +
#   theme_ridges(center = TRUE)