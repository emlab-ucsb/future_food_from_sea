## Tracey Mangin
## November 14, 2019
## Production potential - first cut

library(tidyverse)
library(janitor)
library(scales)
library(plotly)


## source functions
functions <- list.files(here::here("capture_functions"))

walk(functions, ~ here::here("capture_functions", .x) %>% source()) # load local functions

## save path for figures
# savepath <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/Figures/"

## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/"

## read in relevant outputs/processed data
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F) %>%
  filter(Year == 2012)

conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv.csv")

ss_outputs <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/ss_outputs.csv")

production_df_adj <- readRDS(paste0(pathstart, "Outputs/Capture/data/production_df_adj.rds"))

fcurr_sc <- readRDS("~/Box/SFG Centralized Resources/Projects/Blue paper/nature/processed_data/fcurr_production.rds")

# projection_df <- readRDS(paste0(pathstart, "Outputs/Capture/data/projection_outputs.rds"))
# steadystate_df <- readRDS(paste0(pathstart, "Outputs/Capture/data/steadystate_df.rds"))


## Capture fisheries, MSY
## --------------------------------------------------------------

## scale capture fishery production
perc2reduction <- 0.18

## join with upsides to calculate edible food at MSY
upsides_ed = upsides %>%
  left_join(conv_vals) %>%
  mutate(scl_val = MSY / 0.78,
         msy_food = scl_val / convert_val)



## MSY
sum(upsides_ed$scl_val)
sum(upsides_ed$msy_food, na.rm = T)

## Capture fisheries, rational production (Fcurrent and Frational with improvements)
## -----------------------------------------------------------------

## get production under different price values
## -------------------------------------------------------------------------------------

# price_vals <- as.list(seq(0, 20000, 1))
# 
# production_df <- map(price_vals,
#                      calc_prod_options,
#                      input_df = ss_outputs) %>%
#   bind_rows() 
# 
# ## reformat
# pi_df <- ss_outputs %>%
#   select(id_orig:harvest, extract_cost, MSY, mcost_mt)
# 
# prod_cur_df <-  production_df %>%
#   select(id_orig:F_current) %>%
#   mutate(choice_pi = pmax(0, F_current),
#          f_policy = "F_current") %>%
#   left_join(pi_df) %>%
#   mutate(profit = (harvest * price) - (mcost_mt * harvest) - extract_cost,
#          choice_policy = ifelse(F_current != choice_pi, "no_prod", "F_current"),
#          adj_harvest = ifelse(choice_policy == "no_prod", 0, harvest),
#          adj_profit = ifelse(choice_policy == "no_prod", 0, profit))
# 
# 
# saveRDS(prod_cur_df, "~/Box/SFG Centralized Resources/Projects/Blue paper/nature/processed_data/fcurr_production.rds")




## first, create supply curve that is only Fcurrent or no production
convert_luc <- upsides_ed %>%
  select(id_orig = IdOrig, SpeciesCat, ISSCAAP_Group:msy_food)

prod_cur_df2 <- fcurr_sc %>%
  filter(price < 10000) %>%
  left_join(convert_luc) %>%
  mutate(food_mt = adj_harvest / convert_val) %>%
  group_by(price) %>%
  summarise(total_h = sum(adj_harvest),
            total_food = sum(food_mt, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(scen = "rational_current")
  
## supply
fcurr_supply <- ggplot(prod_cur_df2, aes(x = total_food / 1e6, y = price)) +
  geom_line(size = 1) +
  xlim(0, 65) +
  # geom_hline(yintercept = wm_price_adj, size = 0.5, color = "grey") +
  # geom_vline(xintercept = msy_val / 1e6, size = 0.5, lty = 2, color = "grey") +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  scale_y_continuous(label=comma) +
  # ggtitle("Global capture fishery harvest in steady-state (78% of reported landings)") +
  ylab("Price (USD)") +
  xlab("Total food (million mt)") +
  # annotate("text", x = 30, y = wm_price0, label = paste0("Current weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 3) +
  # annotate("text", x =100, y = 2500, label = paste0("Max production under FMSY = ", format(round(msy_val / 1e6, 1), trim = TRUE), " mmt"), size = 3, angle = 90) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 14))

## now add rational production with FMSY option
prod_df2 <- production_df_adj %>%
  filter(price < 10000) %>%
  left_join(convert_luc) %>%
  xlim(0, 65) +
  mutate(food_mt = adj_harvest / convert_val) %>%
  group_by(price) %>%
  summarise(total_h = sum(adj_harvest),
            total_food = sum(food_mt, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(scen = "rational")

rational_supply <- fcurr_supply +
  geom_line(data = prod_df2, aes(x = total_food / 1e6, y = price), size = 1, color = "blue")


