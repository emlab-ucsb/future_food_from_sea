## Tracey Mangin
## December 5, 2019
## Capture fishery supply curves for BP1

library(tidyverse)
library(janitor)
library(scales)
library("wesanderson")
library(plotly)

# ## save path for figures
# savepath <- "~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/"

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"


## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/"

## read in relevant outputs/processed data
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F) 

## conversion factors
# conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv.csv")
conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv_all.csv")

## supply curves
mc_supply <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/harvest_x_price_x_conversion.csv") %>%
  select(-X1)


## rational production
# production_df_adj <- readRDS(paste0(pathstart, "Outputs/Capture/data/production_df_adj.rds"))

# ## fcurrent production
# fcurr_sc <- readRDS("~/Box/SFG Centralized Resources/Projects/Blue paper/nature/processed_data/fcurr_production.rds")
# 
# # ## Fcurrent production (max price == 5000)
# # f0_prod_df_adj <- readRDS(paste0(pathstart, "Outputs/Capture/data/f0_prod_df_adj.rds"))
# 
# ## projection
# projection_df <- readRDS(paste0(pathstart, "Outputs/Capture/data/projection_outputs.rds"))

# ## steady state
# steadystate_df <- readRDS(paste0(pathstart, "Outputs/Capture/data/steadystate_df.rds"))

## scale capture fishery production
prod_df <- read_csv(paste0(bp_path, "outputs/upside_fao_scale_df.csv"))

## scale capture fishery production
cf_scale_val <- prod_df %>%
  select(rel_catch) %>%
  as.numeric()

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

# ## functions
# calc_glob_h <- function(hval, inflation) {
#   global_h <- hval / inflation
#   
# }

## Calculate the current (2012) average price of wild fishery landings
upsides0 <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) 

wm_price0 <- weighted.mean(upsides0$Price, upsides0$Catch)

upsides0_adj <- upsides0 %>%
  mutate(Price = ifelse(SciName == "Engraulis ringens", 250, Price))

wm_price_adj <- weighted.mean(upsides0_adj$Price, upsides0_adj$Catch)

## find the multiple to ge the correct scaling

## 2012 food prod (FAO, see historic prod)
upsides_conv <- upsides0 %>%
  select(id_orig = IdOrig, SpeciesCat, Catch) %>%
  left_join(conv_vals) %>%
  mutate(scl_c = Catch / cf_scale_val,
         red_c = scl_c * red_scale_mult,
         food = red_c * convert_mult)


## Create supply curves for three scenarios: Fcurrent, Rational reform, subsidized reform
## focus on price <= 10000
## scale everything up for global coverage
## ---------------------------------------------------------------------------------------

## 2012 food

# 
# ## upsides conversion factors
# upsides_conv <- upsides0 %>%
#   select(id_orig = IdOrig, SpeciesCat) %>%
#   left_join(conv_vals) %>%
#   mutate(ISSCAAP_Group = ifelse(SpeciesCat == 75, "Horseshoe crabs and other arachnoids", ISSCAAP_Group),
#          seafood_type = ifelse(SpeciesCat == 75, "not food", seafood_type),
#          conv_mult = ifelse(SpeciesCat == 75, 0, 1 / convert_val))

## fcurrent supply curve
finit_sc_df <- fcurr_sc %>%
  filter(price <= 10000) %>%
  select(id_orig, price, MSY, harvest, adj_harvest) %>%
  left_join(upsides_conv) %>%
  mutate(scl_harvest = calc_glob_h(adj_harvest, cf_scale_val),
         scl_red = scl_harvest * red_scale_mult,
         ed_meat = scl_red * conv_mult) %>%
  group_by(price) %>%
  summarise(total_h = sum(scl_harvest),
            total_avail = sum(scl_red),
            total_ed = sum(ed_meat)) %>%
  mutate(scenario = "F current")
  
## rational supply curve
rational_sc_df <- production_df_adj %>%
  filter(price <= 10000) %>%
  select(id_orig, price, MSY, harvest, adj_harvest) %>%
  left_join(upsides_conv) %>%
  mutate(scl_harvest = calc_glob_h(adj_harvest, cf_scale_val),
         scl_red = scl_harvest * red_scale_mult,
         ed_meat = scl_red * conv_mult) %>%
  group_by(price) %>%
  summarise(total_h = sum(scl_harvest),
            total_avail = sum(scl_red),
            total_ed = sum(ed_meat)) %>%
  mutate(scenario = "Rational reform")  

## FMSY
fmsy_sc_df <- upsides0 %>%
  select(id_orig = IdOrig, SpeciesCat, MSY) %>%
  left_join(upsides_conv) %>%
  mutate(scl_msy = calc_glob_h(MSY, cf_scale_val),
         scl_red = scl_msy * red_scale_mult,
         ed_meat = scl_red * conv_mult)

fmsy_sc_df2 <- tibble(price = seq(0, 10000, 1),
                      total_h = rep(sum(fmsy_sc_df$scl_msy), length(seq(0, 10000, 1))),
                      total_avail = rep(sum(fmsy_sc_df$scl_red), length(seq(0, 10000, 1))),
                      total_ed = rep(sum(fmsy_sc_df$ed_meat), length(seq(0, 10000, 1))),
                      scenario = rep("FMSY", length(seq(0, 10000, 1))))

ss_fmsy_sc_df <- steadystate_df %>%
  select(id_orig, f_policy, hbreak, h_ss) %>%
  filter(f_policy == "FMSY") %>%
  left_join(upsides_conv) %>%
  mutate(scl_hbreak = calc_glob_h(hbreak, cf_scale_val),
         ed_meat = scl_hbreak * conv_mult)

## why different?
fmsy_break <- steadystate_df %>%
  mutate(diff = h_ss - hbreak,
         pdiff = diff / hbreak)
## some go to zero (parameterization issues?)
## some are slow growing

## combine capture dataframes
# wc_supply <- rbind(finit_sc_df, rational_sc_df, fmsy_sc_df2)
# 
# saveRDS(wc_supply, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/wild_fisheries_agg_sup.rds")

wc_supply <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/wild_fisheries_agg_sup.rds") %>%
  mutate(scenario = ifelse(scenario == "FMSY", "MSY", scenario))


## ggplot supply curves
color_palette <- c("#8bbdd9", "#298fca", "#094c72")

wc_supply$scenario <- factor(wc_supply$scenario, levels = c("F current", "Rational reform", "MSY"))

cons0_w <- round(51.2)
p0 <- round(wm_price_adj)

init_df <- tibble(price = p0,
                  total_ed = cons0_w,
                  scenario = "current")

init_df_sup <- tibble(price = seq(0, p0, 1),
                  total_ed = rep(cons0_w, length(seq(0, p0, 1))),
                  scenario = rep("current", length(seq(0, p0, 1))))

init_df_dem <- tibble(price = rep(p0, length(seq(0, cons0_w, 1))),
                      total_ed = (seq(0, cons0_w, 1)),
                      scenario = rep("current", length(seq(0, cons0_w, 1))))



wc_ed_sc_fig <- ggplot(wc_supply, aes(x = total_ed / 1e6, y = price, group = scenario, color = scenario)) +
  geom_line(size = 2, alpha = 0.9) +
  geom_point(data = init_df, aes(x = total_ed, y = price), color = "black", size = 5) +
  geom_line(data = init_df_sup, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5) +
  geom_line(data = init_df_dem, aes(x = total_ed, y = price), lty = "dashed", color = "black", size = 1.5) +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # geom_vline(xintercept = cons0_w, lty = "dashed", size = 1) +
  scale_color_manual(values = color_palette) +
  xlab(" ") +
  scale_y_continuous(label=comma,
                     limits = c(0,2600),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 70)) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        title = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.3, 0.8),
        legend.text = element_text(size = 18),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5)) 
# +
#   guides(color = guide_legend(override.aes=list(fill=NA)))

ggsave(filename =  paste0(savepath, "capture_ed_sc.png"), wc_ed_sc_fig, width = 9, height = 9, units = "in", dpi = 300)

## production at current prices for the three scenarios
wild_prod_vals <- wc_supply %>% filter(price == round(wm_price_adj))


## timing of reform, current prices
## --------------------------------------------------

reform_df <- production_df_adj %>%
  filter(price == round(wm_price_adj)) %>%s
  select(id_orig, f_policy, adj_harvest)

proj_df_cp <- reform_df %>%
  left_join(projection_df) %>%
  mutate(adj_proj_h = ifelse(profit < 0, 0, harvest))

proj_df_cp_agg <- proj_df_cp %>%
  left_join(upsides_conv) %>%
  mutate(scl_up = adj_proj_h / 0.78,
         scl_red = scl_up * (1 - 0.18),
         scl_food = scl_red * conv_mult) %>%
  group_by(time) %>%
  summarise(sum_food = sum(scl_food)) %>% 
  ungroup()
 
ggplot(proj_df_cp_agg, aes(x = time, y = sum_food / 1e6)) +
  geom_path(size = 2)




