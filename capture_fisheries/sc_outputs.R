## Tracey Mangin
## May 31, 2019
## Create capture outputs for use in creating aggregate supply curve

## attach librariess
library(tidyverse)
library(viridis)
# library("wesanderson")
# library(janitor)
# library(scales)
# library(plotly)
# library(rgdal)

## bp 
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"


# save path for figures
savepath <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/"

## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/"

## upsides msy
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)

## conversion factors
conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv_all.csv")

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

## get fao scale value
fao_scale_df <- read_csv(paste0(bp_path, "outputs/upside_fao_scale_df.csv"))

## scale value
cf_scale_val <- fao_scale_df %>%
  select(rel_catch) %>%
  as.numeric()

## upsides 2012
upsides0 <- upsides %>%
  mutate(BMSY = Biomass / BvBmsy) %>%
  filter(Year == 2012,
         BMSY > MSY) %>%
  left_join(conv_vals) %>%
  mutate(msy_scl = MSY / cf_scale_val,
         msy_food = msy_scl * red_scale_mult * convert_mult)

sum(upsides0$msy_food)


## read in relevant outputs/processed data
prod_df <- readRDS(paste0(pathstart, "production_df_smry.rds"))
prod_s1_df <- readRDS(paste0(pathstart, "production_s1_smry.rds"))
prod_s2_df <- readRDS(paste0(pathstart, "production_s2_smry.rds"))
prod_s3_df <- readRDS(paste0(pathstart, "production_s3_smry.rds"))

prod_fmsy_df <- readRDS(paste0(pathstart, "fmsy_prod_smry.rds")) %>%
  mutate(scenario = "FMSY_orig_inputs")

prod_f0_df <- readRDS(paste0(pathstart, "f0_prod_smry.rds")) %>%
  mutate(scenario = "F0_orig_inputs")

## combine, summarise at price level
sc_capture_df <- rbind(prod_df, prod_s1_df, prod_s2_df, prod_s3_df, prod_fmsy_df, prod_f0_df) %>%
  group_by(scenario, seafood_type, convert_val, price) %>%
  summarise(ss_h = sum(total_ss_h),
            h2050 = sum(total_2050_h),
            h2050scl = sum(total_2050_h_scl),
            h2050red = sum(total_2050_h_red),
            food2050 = sum(total_2050_food)) %>%
  ungroup()


sc_capture_df_sum <- sc_capture_df %>%
  group_by(scenario, price) %>%
  summarise(totfood = sum(food2050, na.rm = T)) %>%
  ungroup() 

diff_df <- sc_capture_df_sum %>%
  pivot_wider(names_from = scenario, values_from = totfood) %>%
  mutate(diff = improved_tech - original_inputs)

## 
sc_si <- ggplot(sc_capture_df_sum %>% filter(price <= 5000,
                                             !scenario %in% c("FMSY_orig_inputs", "F0_orig_inputs")), 
                aes(y = price, x = totfood / 1e6, color = scenario)) +
  geom_path(size = 1, alpha = 0.9) +
  ylab("Price (USD per mt)") +
  xlab("Edible production (mmt") +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_color_viridis_d(option = "C",
                      breaks = c("cs_price", "improved_tech", "improved_tech_cs", "original_inputs"),
                      labels = c("Increased management cost",
                                 "Improved tech",
                                 "Improved tech and increased management cost",
                                 "Original scenario")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))


ggsave(filename =  paste0(bp_path, "figures/si_capture.png"), sc_si, width = 10, height = 8, units = "in", dpi = 300)


## scenario, price, sector, capture, conversion_grp, conversion_value, mt
sc_capture_df2 <- sc_capture_df %>%
  mutate(sector = "capture") %>%
  select(scenario, sector, seafood_type, convert_val, price, mt = h2050scl)



## save file for mariculture
write.csv(sc_capture_df2, paste0(savepath, "data/final/harvest_x_price_x_conversion.csv"))
write.csv(sc_capture_df2, paste0(bp_path, "outputs/harvest_x_price_x_conversion.csv"))



