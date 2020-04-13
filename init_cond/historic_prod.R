## Tracey Mangin
## December 9 2019
## Historical and current production

## This is STEP 2 in the init_cond folder. 
## run edible_conv.R first to create required file.

library(tidyverse)
library(viridis)
library(scales)
library(cowplot)
library(janitor)
library(countrycode)

## note to user: update the path and load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F)

## note to user: update the path and load file created in edible_cov.R file
conv_vals <- read_csv("isscaap_food_conv_all.csv")

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
fao_gp <- read.csv("GlobalProduction_2019.1.0/TS_FI_PRODUCTION.csv", stringsAsFactors = F)

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
fao_source <- read.csv("GlobalProduction_2019.1.0/CL_FI_PRODUCTION_SOURCE.csv", stringsAsFactors = F) %>%
  select(SOURCE = Identifier, Code, Name_En) %>%
  mutate(Code = ifelse(Code == "MARINE", "MARICULTURE",
                       ifelse(Code == "FRESHWATER", "FRESHWATER_AQ",
                              ifelse(Code == "BRACKISHWATER", "BRACKISHWATER_AQ", Code))),
         prod_type = ifelse(Code == "CAPTURE", "capture", "aquaculture"))

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
fao_sp <- read.csv("GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F) %>%
  select(SPECIES = X3Alpha_Code, ISSCAAP_Group, CPC_Class, CPC_Group)

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
fao_area <- read.csv("GlobalProduction_2019.1.0/CL_FI_WATERAREA_GROUPS.csv", stringsAsFactors = F) %>%
  select(AREA = Code, Name_en) %>%
  mutate(location = ifelse(grepl("Inland", Name_en) == TRUE, "inland", "marine")) %>%
  select(-Name_en)

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
fao_country <- read.csv("GlobalProduction_2019.1.0/CL_FI_COUNTRY_GROUPS.csv", stringsAsFactors = F) %>%
  select(COUNTRY = UN_Code, iso3 = ISO3_Code, country_name = Name_En)

## note to user: update the path and load FAO data contained in GlobalProduction_2019.1.0 folder.
sp <- read.csv("GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F) 

## read price databases
## note to user: update the path and load price data from ffts/init_cond_data folder
prices <- read_csv("ffts/init_cond_data folder/Exvessel price comparison national data.csv")
prices_est <- read_csv("ffts/init_cond_data folder/Exvessel Price Database.csv")

## scientfic name
sci_name <- sp %>%
  select(alpha_code = X3Alpha_Code, sci_name = Scientific_Name, name_en = Name_En)


## hisotrical food production from the sea & land
## ------------------------------------------------------------

## freshwater isscaap
freshwater <- c(11, 12, 13, 41, 51)

## scale capture fishery production
perc2reduction <- 0.18

## start with FAO

fao_na <- fao_gp %>%
  left_join(fao_sp) %>%
  left_join(fao_source) %>%
  left_join(fao_area) %>%
  # filter(location != "inland") %>%
  filter(!CPC_Class %in% c("Natural sponges of aquatic animal origin",
                           "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose")) %>% ## remove freshwater and cpc classes that are not food
  left_join(conv_vals) %>%
  filter(is.na(convert_mult == TRUE)) %>%
  select(ISSCAAP_Group) %>%
  unique()

remove_vec <- fao_na$ISSCAAP_Group

fao_convers <- fao_gp %>%
  left_join(fao_sp) %>%
  left_join(fao_source) %>%
  left_join(fao_area) %>%
  # filter(location != "inland") %>%
  filter(!CPC_Class %in% c("Natural sponges of aquatic animal origin",
                           "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose"),
         !ISSCAAP_Group %in% remove_vec) %>% ## remove freshwater and cpc classes that are not food
  left_join(conv_vals) %>%
  mutate(fresh = ifelse(Code == "CAPTURE" & SpeciesCat %in% freshwater, 1, 0))
  
fresh_capt <- fao_convers %>%
  filter(fresh == 1) %>%
  select(ISSCAAP_Group, CPC_Class, Code, Name_En) %>%
  unique()
## leaving in 

# fao_convers <- fao_convers %>% filter(fresh != 1)

## annual production (food equivalent)
plant_vec <- c(91, 92, 93, 94)

fed_mari <- c()

unfed_mari <- c()

## unique combinations
combo_df <- fao_convers %>%
  select(SOURCE, Code, Name_En, prod_type, location) %>%
  unique()

fe_prod <- fao_convers %>%
  rename(alpha_code = SPECIES) %>%
  left_join(sci_name) %>%
  left_join(fao_country) %>%
  mutate(sp_type = ifelse(SpeciesCat %in% plant_vec, "aquatic_plant", "aquatic_animal")) %>%
         # prod_type_lab = ifelse(prod_type == "capture", "Wild fisheries", "Aquaculture"),
         # prod_type_lab = ifelse(prod_type == "aquaculture" & SpeciesCat %in% plant_vec, "Mariculture: plants",
         #              ifelse(lab == "mariculture" & !SpeciesCat %in% plant_vec, "Mariculture: animals", lab))) %>%
  select(year = YEAR, quantity = QUANTITY, isscaap = ISSCAAP_Group,
         code = Code, code_descrip = Name_En, prod_type, location, species_cat = SpeciesCat, seafood_type,
         convert_mult, description, sp_type, sci_name, name_en, iso3, country_name) %>%
  # mutate(mari_lab = ifelse(species_cat %in% fed_mari & Code %in% c("MARINE, BRACKISHWATER"), "fed",
  #                          ifelse(species_cat %in% unfed_mari & Code %in% c("MARINE, BRACKISHWATER"), "unfed", "capture"))) %>%
  mutate(q_scl_red = ifelse(prod_type == "capture" & location == "marine" & sp_type == "aquatic_animal", quantity * (1 - perc2reduction), quantity),
         food_mt = q_scl_red * convert_mult)

## upate path, save file for future use.
write_csv(fe_prod, "historical_prod_x_sp.csv")


# ## land based production
# lb_prod_df <- fe_prod %>%
#   select(year, prod_type, location, sp_type, isscaap, species_cat, sci_name, name_en, harvest = quantity, food_mt) %>%
#   
#   
# write_csv(lb_prod_df, "land_based_production.csv")

fe_prod_summary <- fe_prod %>%
  select(year, isscaap:sp_type, harvest = quantity, q_scl_red, food_mt) %>%
  pivot_longer(harvest:food_mt, names_to = "measurement", values_to = "quantity") %>%
  group_by(year, prod_type, location, sp_type, measurement) %>%
  summarise(total_q = sum(quantity)) %>%
  ungroup() 

## upate path, save file for future use.
write_csv(fe_prod_summary, "historical_prod.csv")


inland_cap_df <- fe_prod_summary %>%
  filter(location == "inland",
         prod_type == "capture",
         sp_type == "aquatic_animal",
         measurement != "q_scl_red") %>%
  pivot_wider(names_from = measurement, values_from = total_q) %>%
  select(year:sp_type, harvest, food_mt)

## upate path, save file for future use.
write_csv(inland_cap_df, "historical_inland_capture.csv")



hist_food_df <- fe_prod_summary %>%
  filter(sp_type != "aquatic_plant",
        measurement != "q_scl_red") %>%
  mutate(cat = paste(location, prod_type, sep = "-"),
         lab = ifelse(measurement == "harvest", "Harvest (live-weight production)", "Food (edible meat production)"),
         prod_lab = ifelse(cat == "marine-capture", "Marine capture",
                           ifelse(cat == "marine-aquaculture", "Mariculture",
                                  ifelse(cat == "inland-capture", "Inland capture", "Inland aquaculture"))))
  

hist_food_df$lab <- factor(hist_food_df$lab, levels = c("Harvest (live-weight production)", "Food (edible meat production)"))

## figure
hist_food_fig <- ggplot(hist_food_df, aes(x = year, y = total_q / 1e6, group = cat, fill = prod_lab)) +
  geom_area(color = "white") +
  facet_wrap(~lab, ncol = 2) +
  scale_fill_manual(values = c("Marine capture" = "#0033cc", 
                               "Mariculture" = "#668cff",
                               "Inland capture" = "#29a3a3",
                               "Inland aquaculture" = "#85e0e0")) +
  ylab("Global production (mmt)") +
  # ggtitle("Historical food from the sea") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        plot.title = element_blank(),
        legend.position = c(0.75, 0.85),
        legend.text = element_text(size = 22),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        plot.margin = margin(10, 20, 10, 10))

## note to user: figure created for SI -- change path and save.
ggsave("si_histprod.png", hist_food_fig, width = 12, height = 8, units = "in", dpi = 300)



## figure
hist_food_fig2 <- ggplot(hist_food_df %>% filter(prod_lab %in% c("Marine capture", "Mariculture")), aes(x = year, y = total_q / 1e6, group = cat, fill = prod_lab)) +
  geom_area() +
  facet_wrap(~lab, ncol = 2) +
  scale_fill_manual(values = c("Marine capture" = "#33638DFF", 
                               "Mariculture" = "#20A387FF")) +
  ylab("Global production (mmt)") +
  # ggtitle("Historical food from the sea") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        plot.title = element_blank(),
        legend.position = c(0.75, 0.85),
        legend.text = element_text(size = 22),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        plot.margin = margin(10, 20, 10, 10))


## note to user: figure created for main text -- change path and save.
ggsave("figures/fig1.png", hist_food_fig2, width = 12, height = 8, units = "in", dpi = 300)



## intital mariculture production (2017)
init_prod <- fe_prod %>%
  filter(prod_type == "aquaculture",
         location == "marine",
         sp_type == "aquatic_animal") %>%
  group_by(year, species_cat, isscaap) %>%
  summarise(total_h = sum(quantity),
            total_food = sum(food_mt)) %>%
  ungroup()
  
sp_df <- init_prod %>%
  select(species_cat, isscaap) %>%
  unique()

finfish_vec <- c(11, 12, 13, 22, 23, 25, 31, 32, 33, 36, 37, 39, 34, 21, 24)
crustacea_vec <- c(42, 43, 45, 47, 41)
mollusc_vec <- c(52, 53, 54, 55, 56, 58, 51)
other_vec <- c(57, 76, 77 )

init_prod2 <- init_prod %>%
  mutate(sp_group = ifelse(species_cat %in% finfish_vec, "finfish",
                           ifelse(species_cat %in% crustacea_vec, "crustacea",
                                  ifelse(species_cat %in% mollusc_vec, "mollusc", "other_animal")))) %>%
  group_by(year, sp_group) %>%
  summarise(harvest_mmt = sum(total_h) / 1e6,
            food_mmt = sum(total_food) / 1e6) %>%
  mutate(feed_grp = ifelse(sp_group == "mollusc", "unfed", "fed")) 

## note to user: change path and save for later use.
write_csv(init_prod2, "init_mari_prod.csv")
