## Tracey Mangin
## May 31, 2019
## Future of Food From the Sea

## create aggregated supply curves

## Make sure to run files in init_cond folder before proceeding.

## This is STEP 3 for the marine capture fishery analysis.


## attach librariess
library(tidyverse)
library(viridis)

## upsides msy
## note to user: update the path and load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F)

## conversion factors
## note to user: update the path and load conversion file created in init_cond/edible_conv.R script.
conv_vals <- read_csv("isscaap_food_conv_all.csv")

## scale for reduction fisheries
red_scale_mult <- 1 - 0.18

## get fao scale value
## note to user: update the path and load conversion file created in init_cond/init_conditions.R script.
fao_scale_df <- read_csv("upside_fao_scale_df.csv")

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
## note to user: these are produced in STEP 2 of the marine capture analysis (capture_sc_run.R)
## update path and load files created during STEP 2
prod_df <- readRDS("production_df_smry.rds")
prod_s1_df <- readRDS("production_s1_smry.rds")
prod_s2_df <- readRDS("production_s2_smry.rds")
prod_s3_df <- readRDS("production_s3_smry.rds")


prod_fmsy_df <- readRDS("fmsy_prod_smry.rds") %>%
  mutate(scenario = "FMSY_orig_inputs")

prod_f0_df <- readRDS("f0_prod_smry.rds") %>%
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

## note to user: this figure is produced for the SI. Update path and save.
ggsave(filename =  "si_capture.png", sc_si, width = 10, height = 8, units = "in", dpi = 300)


## scenario, price, sector, capture, conversion_grp, conversion_value, mt
sc_capture_df2 <- sc_capture_df %>%
  mutate(sector = "capture") %>%
  select(scenario, sector, seafood_type, convert_val, price, mt = h2050scl)



## save file for mariculture analysis -- this file is used to create file with supply curves
## from both mariculture and marine capture.
## update the path and save
write.csv(sc_capture_df2, "harvest_x_price_x_conversion.csv")

## run "aquaculture" scripts next

