## Tracey Mangin
## November 14, 2019
## Future food from the sea
## Create converions: Landed weight to food weight df

## This is STEP 1 in the init_cond folder.


## libraries
library(tidyverse)

## read in relevant outputs/processed data

## note to user: update the path and load ProjectionData.csv
upsides <- read.csv("/ffts/m_capture_data/ProjectionData.csv", stringsAsFactors = F) %>%
  filter(Year == 2012)

## note to user: update the path and load FAO data from init_cond_data/GlobalProduction_2019.1.0 folder
fao_gp <- read.csv("GlobalProduction_2019.1.0/TS_FI_PRODUCTION.csv", stringsAsFactors = F)

fao_source <- read.csv("GlobalProduction_2019.1.0/CL_FI_PRODUCTION_SOURCE.csv", stringsAsFactors = F) %>%
  select(SOURCE = Identifier, Code, Name_En)

fao_sp <- read.csv("GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F) %>%
  select(SPECIES = X3Alpha_Code, ISSCAAP_Group, CPC_Class, CPC_Group)

sp <- read.csv("GlobalProduction_2019.1.0/CL_FI_SPECIES_GROUPS.csv", stringsAsFactors = F)


## species types from upsides
id_cats <- upsides %>%
  select(ISSCAAP_Group = SpeciesCatName, SpeciesCat) %>%
  unique()

## species types from FAO production
fao_gp2 <- fao_gp %>%
  left_join(fao_sp) %>%
  left_join(fao_source) %>%
  filter(Code != "FRESHWATER", 
         CPC_Class != c("Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose",
                        "Natural sponges of aquatic animal origin")) %>% ## remove freshwater and cpc classes that are not food
  left_join(id_cats) 


## do the names match?
upid <- id_cats %>% select(ISSCAAP_Group) %>% unique()

faoid <- fao_gp2 %>% select(ISSCAAP_Group) %>% unique()

anti_join(upid, faoid) ## all match
anti_join(faoid, upid) ## bunch of weirdos (23)

## na isscaap group
na_gp <- fao_gp2 %>%
  select(ISSCAAP_Group, SpeciesCat) %>%
  unique() %>%
  filter(is.na(SpeciesCat))

## make a dataframe with the missing ISSCAAP group and numbers (used materials available online)
na_df <- data.frame(ISSCAAP_Group = na_gp$ISSCAAP_Group,
                    SpeciesCat = c(58, 39, 22, 13, 12, 92, 72, 74, 94, 91, 93, 81, 41, 46, 62, 63, 73, 61, 82, 83, 71, 64, 51)) %>%
  rbind(id_cats)

fao_gp3 <- fao_gp2 %>%
  select(-SpeciesCat) %>%
  left_join(na_df)

## categorize species cat name by three categories in Edwards et al. 2019
ed_convert_df <- data.frame(seafood_type = c("fish", "crustaceans", "molluscs"),
                            covert_val = c(1.15, 2.80, 6.0),
                            description = c("gutted, head-on", "tail meat, peeled", "meat without shells"))

# cats_df <- id_cats %>%
#   select(SpeciesCatName, SpeciesCat) %>%
#   unique() %>%
#   arrange(SpeciesCat)

fish_vec <- c(11, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 36, 37, 38, 39, 13, 12)
crust_vec <- c(42, 43, 44, 45, 47, 41)
molluscs_vec <- c(52, 53, 54, 55, 56, 57, 58, 51)
plant_vec <- c(91, 92, 93, 94)

freshwater <- c(11, 12, 13, 41, 51)
remove_vec <- c(72, 74, 81, 46, 62, 63, 73, 61, 82, 83, 71, 64)

## #77 --> attribute proportionally based on observed ratio in higher resolved groups (Miscellaneous aquatic invertebrates)
misc_invt_df <- upsides %>%
  filter(Year == 2012,
         SpeciesCat %in% c(crust_vec, molluscs_vec)) %>%
  mutate(seafood_type = ifelse(SpeciesCat %in% crust_vec, "crustaceans", "molluscs")) %>%
  group_by(seafood_type) %>%
  summarise(sum_msy = sum(MSY)) %>%
  ungroup() %>%
  mutate(total_sum = sum(sum_msy),
         rel_msy = sum_msy / total_sum) %>%
  left_join(ed_convert_df) %>%
  select(-description)

val77 <- weighted.mean(misc_invt_df$covert_val, misc_invt_df$rel_msy)

## #76 --> use a mean of the value (Sea-urchins and other echinoderms)
val76 <- mean(ed_convert_df$covert_val)

## remove horseshoe crabs (75), because the two fisheries are in the USA, where they
## are used in the biomedical industry (http://www.asmfc.org/species/horseshoe-crab), not for
## food
val75 <- NA

## add these values
ed_convert_df2 <- data.frame(seafood_type = c("horseshoe", "echinoderms", "misc_invt", "plant"),
                             covert_val = c(val75, val76, val77, 10),
                             description = c("Horseshoe crabs and other arachnoids", "Sea-urchins and other echinoderms", "Miscellaneous aquatic invertebrates", "Aquatic plants"))

ed_convert_df_all <- rbind(ed_convert_df, ed_convert_df2)


## 
hs_vec <- c(75)
sea_u_vec <- c(76)
misc_invt_vec <- c(77)

## add conversions to id_cats
id_cats2 <- id_cats %>%
  rbind(na_df) %>%
  unique() %>%
  filter(!SpeciesCat %in% remove_vec) %>%
  mutate(seafood_type = ifelse(SpeciesCat %in% hs_vec, "horseshoe",
                               ifelse(SpeciesCat %in% sea_u_vec, "echinoderms",
                                      ifelse(SpeciesCat %in% misc_invt_vec, "misc_invt",
                                             ifelse(SpeciesCat %in% fish_vec, "fish",
                                                    ifelse(SpeciesCat %in% plant_vec, "plant", 
                                                           ifelse(SpeciesCat %in% crust_vec, "crustaceans", "molluscs"))))))) %>%
  left_join(ed_convert_df_all) %>%
  filter(seafood_type != "horseshoe") %>%
  select(-description) %>%
  rename(convert_val = covert_val)

## make one with all ISSCAAP categories
convert_vals_all <- fao_gp3 %>%
  filter(!SpeciesCat %in% remove_vec) %>%
  select(ISSCAAP_Group, SpeciesCat) %>%
  mutate(seafood_type = ifelse(SpeciesCat %in% hs_vec, "horseshoe",
                               ifelse(SpeciesCat %in% sea_u_vec, "echinoderms",
                                      ifelse(SpeciesCat %in% misc_invt_vec, "misc_invt",
                                             ifelse(SpeciesCat %in% fish_vec, "fish",
                                                    ifelse(SpeciesCat %in% plant_vec, "plant", 
                                                           ifelse(SpeciesCat %in% crust_vec, "crustaceans", "molluscs"))))))) %>%
  left_join(ed_convert_df_all) %>%
  rename(convert_val = covert_val) %>%
  mutate(convert_mult = 1 / convert_val) %>%
  select(ISSCAAP_Group:convert_val, convert_mult, description) %>%
  mutate(convert_mult = ifelse(SpeciesCat == 75, 0, convert_mult)) %>%
  unique()
  


## save conversion table with all species
## note to user: update path, save csv. This will be used later.
write_csv(convert_vals_all, "isscaap_food_conv_all.csv")

