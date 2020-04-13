## Tracey Mangin
## projection of rational reform

library(tidyverse)

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

## read in projection df
projection_df <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/projection_outputs.rds")

## conversion factors
conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv_all.csv")

## get fao scale value
fao_scale_df <- read_csv(paste0(bp_path, "outputs/upside_fao_scale_df.csv"))

init_pq_df <- read_csv(paste0(bp_path, "outputs/init_pq_df.csv"))

## upsides
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)

upsides0 <- upsides %>%
  filter(Year == 2012)

## scale capture fishery production
cf_scale_val <- fao_scale_df %>%
  select(rel_catch) %>%
  as.numeric()

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18


## production rational reform
rf_df <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/production_df.rds") %>%
  filter(price == 1308)

rf_df2 <- rf_df %>%
  select(id_orig, f_policy) %>%
  left_join(projection_df) %>%
  mutate(SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  group_by(time, seafood_type, convert_mult) %>%
  summarise(harvest = sum(harvest)) %>%
  ungroup() %>%
  mutate(scl_harvest = harvest / cf_scale_val,
         red_harvest = scl_harvest * red_scale_mult,
         food_mt = red_harvest * convert_mult) %>%
  group_by(time) %>%
  summarise(tot_food = sum(food_mt)) %>%
  ungroup()

## projection fmsy
fmsy <- projection_df %>%
  filter(f_policy == "FMSY") %>%
  mutate(SpeciesCat = upsides0$SpeciesCat[match(id_orig, upsides0$IdOrig)]) %>%
  left_join(conv_vals) %>%
  group_by(time, seafood_type, convert_mult) %>%
  summarise(harvest = sum(harvest)) %>%
  ungroup() %>%
  mutate(scl_harvest = harvest / cf_scale_val,
         red_harvest = scl_harvest * red_scale_mult,
         food_mt = red_harvest * convert_mult) %>%
  group_by(time) %>%
  summarise(tot_food = sum(food_mt)) %>%
  ungroup()




  
