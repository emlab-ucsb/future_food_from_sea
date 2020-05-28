#Estimating Data for inland_supply_info.csv
import pandas as pd

#Load data
dat = pd.read_csv('land_based_production.csv')
#Filter
#Filter out aquatic plants
fish_dat = dat.loc[dat.sp_type == 'aquatic_animal']
fish_dat = fish_dat.loc[fish_dat.year == 2017]
#filter for inland production
l_dat = fish_dat.loc[fish_dat.location == 'inland']

#Segment aquaculture from capture
aq_ldat = l_dat.loc[l_dat.prod_type == 'aquaculture']
cap_ldat = l_dat.loc[l_dat.prod_type == 'capture']

#Parameters as inputs to to inland_supply_info

#carp aquaculture
carp_aq = aq_ldat.loc[aq_ldat.species_cat == 11]
tot_carp_aq_food = sum(carp_aq.food_mt)
tot_carp_aw_harvest = sum(carp_aq.harvest)
#carp capture
carp_capt = cap_ldat.loc[cap_ldat.species_cat == 11]
tot_carp_capt_food = sum(carp_capt.food_mt)

#ratio of harvest to food: carp
carp_rat = sum(carp_aq.food_mt) / sum(carp_aq.harvest)

#tilapia aquaculture
tilap_aq = aq_ldat.loc[aq_ldat.species_cat == 12]
sum(tilap_aq.food_mt)
#tilapia capture
tilap_capt = cap_ldat.loc[cap_ldat.species_cat == 12]
sum(tilap_capt.food_mt)

#ratio of harvest to food: carp
tilap_rat = sum(tilap_aq.food_mt) / sum(tilap_aq.harvest)



#catfish aquaculture
catfish_aq = aq_ldat.loc[aq_ldat.name_en.str.contains('catfish')]
sum(catfish_aq.food_mt)
#catfish capture
catfish_capt = cap_ldat.loc[cap_ldat.name_en.str.contains('catfish')]
sum(catfish_capt.food_mt)

#ratio of harvest to food: catfish
catfish_rat = sum(catfish_aq.food_mt) / sum(catfish_aq.harvest)


#other aquaculture
oth_aq = aq_ldat.loc[aq_ldat.species_cat != 11]
oth_aq = oth_aq.loc[oth_aq.species_cat !=12]
oth_aq = oth_aq[~oth_aq.name_en.str.contains('catfish')]
sum(oth_aq.food_mt)
oth_rat = sum(oth_aq.food_mt) / sum(oth_aq.harvest)
#other capture
oth_capt = cap_ldat.loc[cap_ldat.species_cat != 11]
oth_capt = oth_capt.loc[oth_capt.species_cat !=12]
oth_capt = oth_capt[~oth_capt.name_en.str.contains('catfish')]
sum(oth_capt.food_mt)

#ratio of harvest to food: other
oth_capt_rat = sum(oth_capt.food_mt) / sum(oth_capt.harvest)




