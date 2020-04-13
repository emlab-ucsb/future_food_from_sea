## Tracey Mangin
## December 9 2019
## Historical and current production

library(tidyverse)
library(viridis)
library(scales)
library(cowplot)
library(janitor)
library(countrycode)

## bp path
bp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/"

upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)

conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv_all.csv")

fao_gp <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/TS_FI_PRODUCTION.csv", stringsAsFactors = F)

fao_source <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/CL_FI_PRODUCTION_SOURCE.csv", stringsAsFactors = F) %>%
  select(SOURCE = Identifier, Code, Name_En) %>%
  mutate(Code = ifelse(Code == "MARINE", "MARICULTURE",
                       ifelse(Code == "FRESHWATER", "FRESHWATER_AQ",
                              ifelse(Code == "BRACKISHWATER", "BRACKISHWATER_AQ", Code))),
         prod_type = ifelse(Code == "CAPTURE", "capture", "aquaculture"))

fao_sp <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F) %>%
  select(SPECIES = X3Alpha_Code, ISSCAAP_Group, CPC_Class, CPC_Group)

fao_area <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/CL_FI_WATERAREA_GROUPS.csv", stringsAsFactors = F) %>%
  select(AREA = Code, Name_en) %>%
  mutate(location = ifelse(grepl("Inland", Name_en) == TRUE, "inland", "marine")) %>%
  select(-Name_en)

fao_country <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/CL_FI_COUNTRY_GROUPS.csv", stringsAsFactors = F) %>%
  select(COUNTRY = UN_Code, iso3 = ISO3_Code, country_name = Name_En)

sp <- read.csv("~/Box/SFG Centralized Resources/Projects/Blue paper/data/GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F) 

## read price databases
prices <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/Exvessel price comparison national data.csv")
prices_est <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/Exvessel Price Database.csv")

## scientfic name
sci_name <- sp %>%
  select(alpha_code = X3Alpha_Code, sci_name = Scientific_Name, name_en = Name_En)

## upsides
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)


# ## Chris Free already merged these
# fao_merged <- readRDS("~/Box/SFG Centralized Resources/Projects/Blue paper/data/1950_2017_fao_landings_data.Rds")

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
## for now, leaving in 

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

write_csv(fe_prod, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/outputs/historical_prod_x_sp.csv")



# ## land based production
# lb_prod_df <- fe_prod %>%
#   select(year, prod_type, location, sp_type, isscaap, species_cat, sci_name, name_en, harvest = quantity, food_mt) %>%
#   
#   
# write_csv(lb_prod_df, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/land_based_production.csv")

fe_prod_summary <- fe_prod %>%
  select(year, isscaap:sp_type, harvest = quantity, q_scl_red, food_mt) %>%
  pivot_longer(harvest:food_mt, names_to = "measurement", values_to = "quantity") %>%
  group_by(year, prod_type, location, sp_type, measurement) %>%
  summarise(total_q = sum(quantity)) %>%
  ungroup() 

write_csv(fe_prod_summary, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/outputs/historical_prod.csv")


inland_cap_df <- fe_prod_summary %>%
  filter(location == "inland",
         prod_type == "capture",
         sp_type == "aquatic_animal",
         measurement != "q_scl_red") %>%
  pivot_wider(names_from = measurement, values_from = total_q) %>%
  select(year:sp_type, harvest, food_mt)

write_csv(inland_cap_df, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/outputs/historical_inland_capture.csv")


# ## land-based aquaculture break down by food type
# aq_breakd <- fe_prod %>%
#   filter(
#     # prod_type == "aquaculture",
#          location == "inland") %>%
#   group_by(year, seafood_type) %>%
#   summarise(total_h = sum(quantity),
#             total_food = sum(food_mt)) %>%
#   ungroup() %>%
#   group_by(year) %>%
#   mutate(sum_food = sum(total_food)) %>%
#   ungroup() %>%
#   mutate(rel_food = total_food / sum_food)

# finfish_vec <- c(11, 12, 13, 22, 23, 25, 31, 32, 33, 36, 37, 39, 34, 21, 24)
# 
# ## ff mariculture production
# ff_mari_prod <- fe_prod %>%
#   filter(location == "marine",
#          prod_type == "aquaculture",
#          sp_type == "aquatic_animal",
#          species_cat %in% finfish_vec) %>%
#   group_by(year, prod_type, name_en, species_cat, sci_name) %>%
#   summarise(total_h = sum(quantity),
#             total_food = sum(food_mt)) %>%
#   ungroup() %>%
#   group_by(year) %>%
#   mutate(sum_food = sum(total_food)) %>%
#   ungroup() %>%
#   mutate(rel_food = total_food / sum_food) %>%
#   filter(year == max(fe_prod$year)) %>%
#   mutate(sp_group = ifelse(str_detect(name_en, pattern = "almon") == TRUE, "salmon",
#                            ifelse(str_detect(name_en, pattern = "Milkfish"), "milkfish",
#                                              ifelse(str_detect(name_en, pattern = "Barramundi"), "barramundi", NA)))) %>%
#   filter(!(is.na(sp_group))) %>%
#   group_by(sp_group) %>%
#   summarise(food_yr = sum(total_food)) %>%
#   ungroup() %>%
#   mutate(price = ifelse(sp_group == "salmon", 6193,
#                         ifelse(sp_group == "milkfish", 1545, 4384)))
# 
# ## use this in init conditions file!! 
# mean_maric_price = weighted.mean(ff_mari_prod$price, ff_mari_prod$food_yr)
# ## 4407.526

# ## land-based break down by species and production area
# lb_breakd <- fe_prod %>%
#   filter(location == "inland") %>%
#   filter(year == 2017) %>%
#   group_by(year, sci_name, name_en, prod_type) %>%
#   summarise(total_h = sum(quantity),
#             total_food = sum(food_mt)) %>%
#   ungroup() %>%
#   group_by(year, prod_type) %>%
#   mutate(sum_food = sum(total_food)) %>%
#   ungroup() %>%
#   mutate(rel_food = total_food / sum_food) %>%
#   group_by(prod_type) %>%
#   arrange(prod_type, -rel_food) %>%
#   mutate(cumul_food = cumsum(rel_food)) %>%
#   ungroup() %>%
#   select(sci_name, name_en, prod_type, rel_food, cumul_food)
# 
# write_csv(lb_breakd, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/land_based_2017.csv")
# 
# 
# ## all species with land based production since 2015
# lb_sp <- fe_prod %>%
#   filter(location == "inland",
#          year >= 2015) %>%
#   select(sci_name, name_en, prod_type) %>%
#   unique()
# 
# write_csv(lb_sp, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/land_based_sp.csv")


# ## aquaculture price
# ## -------------------------------------
# price_year <- 2017
# 
# 
# # Get average prices for each species from national data
# prices2 <- prices %>% 
#   group_by(Species, Country, Year) %>% 
#   summarize(Price = mean(Price, na.rm = T)) %>%
#   mutate(iso3 = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>%
#   filter(Year == price_year)
# 
# # Get estimated prices from Melnychuck et al. for remaining prices
# prices_est2 <- prices_est %>% 
#   select(scientific_name, Year, exvessel) %>% 
#   rename(sci_name = scientific_name, 
#          year = Year,
#          price = exvessel) %>%
#   filter(year == price_year)

## plot historical production. remove aquatic plants. 
## ----------------------------------------------------

# current_prod %>% fe_prod %>%
#   year == max(fe_prod$year)
# 
# 
# prod0_c <- fe_prod %>%
#   filter(year == 2017,
#          lab == "Wild fisheries") %>%
#   select(food_mt) %>%
#   as.numeric()

# prod0_m <- fe_prod %>%
#   filter(year == 2017,
#          !lab == "Wild fisheries") %>%
#   select(food_mt) %>%
#   summarise(total_m = sum(food_mt)) %>%
#   as.numeric()
# 
# prod0_m_animal <- fe_prod %>%
#   filter(year == 2017,
#          lab == "Mariculture: animals") %>%
#   select(food_mt) %>%
#   summarise(total_m = sum(food_mt)) %>%
#   as.numeric()
# 
# prod0_animal <- prod0_c + prod0_m_animal
# 
# ## current food consumption
# ## edwards et al
# 
# tot_meat <- 391
# (prod0_animal/1e6) / tot_meat
# 
# prod0_c / prod0_animal
# 
# 
# fe_prod$lab <- factor(fe_prod$lab, levels =c("Wild fisheries", "Mariculture: animals", "Mariculture: plants"))
# 
# write_csv(fe_prod, "bp1_nature/results/fao_historical_production.csv")
 
# ## breakdown of production by food type, 2012
# prod_2012 <- fe_prod %>%
#   filter(year == 2012,
#          prod_type == "capture",
#          location == "marine",
#          sp_type == "aquatic_animal") %>%
#   group_by(year, seafood_type) %>%
#   summarise(hsum = sum(quantity),
#             fsum = sum(food_mt)) %>%
#   ungroup() %>%
#   group_by(year) %>%
#   mutate(total_h = sum(hsum),
#          total_f = sum(fsum)) %>%
#   ungroup() %>%
#   mutate(rel_h = hsum / total_h,
#          rel_f = fsum / total_f)



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

# ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/hist_food_fig.png", hist_food_fig, width = 12, height = 8, units = "in", dpi = 300)

ggsave(paste0(bp_path, "figures/si_histprod.png"), hist_food_fig, width = 12, height = 8, units = "in", dpi = 300)



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

# ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/nature adaptation/results/figures/hist_food_fig.png", hist_food_fig, width = 12, height = 8, units = "in", dpi = 300)

ggsave(paste0(bp_path, "figures/fig1.png"), hist_food_fig2, width = 12, height = 8, units = "in", dpi = 300)






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

write_csv(init_prod2, paste0(bp_path, "outputs/init_mari_prod.csv"))
