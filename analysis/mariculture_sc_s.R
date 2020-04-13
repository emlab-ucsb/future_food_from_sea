## Tracey Mangin
## March 19, 2020
## Mariculture supply curve scenarios

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

## supply df
supply_df <- read_rds(paste0(bp_path, "outputs/all_supply_curves.rds")) 

## initial production and price
init_pq_df <- read_csv(paste0(bp_path, "outputs/init_pq_df.csv")) %>%
  mutate(price = round(price))

ffp <- 4408
bp <- 1700

ffq <- 6766051
bpq <- 2861707

## aq species = color
## facet grid technology ~ cost
## dot for current production

supply_df2 <- supply_df %>%
  filter(sector %in% c("Finfish aquaculture", "Bivalve aquaculture"),
         inl_scen == "constant_cap") %>%
  mutate(mp = ifelse(sector == "Finfish aquaculture", ffp, bp)) %>%
  filter(price == mp) %>%
  mutate(q_init = ifelse(sector == "Finfish aquaculture", ffq, bpq),
         q_diff = (meat_yr / 1e6) - (q_init / 1e6))

fig_df1 <- supply_df %>%
  filter(sector %in% c("Finfish aquaculture"),
         inl_scen == "constant_cap")

         
ggplot(fig_df1 %>% filter(cap_scen == "original_inputs", aq_scen == "Scenario 4c - tech advance"), aes(x = meat_yr / 1e6, y = price, color = as.factor(aq_spp))) +
  geom_path(size = 0.75, alpha = 0.6) +
  facet_grid(aq_cost_scalar ~ aq_scen) 
  