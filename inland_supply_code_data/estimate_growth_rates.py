#estimating growth parameters for inland_supply_dat.csv
import pandas as pd

#%%
#Load data
dat = pd.read_csv('land_based_production.csv')

#%%
#Filter out aquatic plants
fish_dat = dat.loc[dat.sp_type == 'aquatic_animal']

fish_dat = fish_dat.loc[(fish_dat.year == 2017) | (fish_dat.year == 2013)]

#%%
#filter for inland production
l_dat = fish_dat.loc[fish_dat.location == 'inland']

#%%
#Segment aquaculture from capture
aq_ldat = l_dat.loc[l_dat.prod_type == 'aquaculture']
cap_ldat = l_dat.loc[l_dat.prod_type == 'capture']


#%%
#estimate carp growth
#aquacuture
carp_aq = aq_ldat.loc[aq_ldat.species_cat == 11]
carp_aq_growth = carp_aq.groupby(['year']).sum()
(carp_aq_growth.iloc[1].food_mt - carp_aq_growth.iloc[0].food_mt) / carp_aq_growth.iloc[0].food_mt / 10
#capture
carp_capt = cap_ldat.loc[cap_ldat.species_cat == 11]
carp_capt_growth = carp_capt.groupby(['year']).sum()
(carp_capt_growth.iloc[1].food_mt - carp_capt_growth.iloc[0].food_mt) / carp_capt_growth.iloc[0].food_mt / 10

#estimate tilapia growth
#aquaculture
tilap_aq = aq_ldat.loc[aq_ldat.species_cat == 12]
tilap_aq_growth = tilap_aq.groupby(['year']).sum()
(tilap_aq_growth.iloc[1].food_mt - tilap_aq_growth.iloc[0].food_mt) / tilap_aq_growth.iloc[0].food_mt / 10
#capture
tilap_capt = cap_ldat.loc[cap_ldat.species_cat == 12]
tilap_capt_growth = tilap_capt.groupby(['year']).sum()
(tilap_capt_growth.iloc[1].food_mt - tilap_capt_growth.iloc[0].food_mt) / tilap_capt_growth.iloc[0].food_mt / 10

#estimate catfish growth
#aquaculture
catfish_aq = aq_ldat.loc[aq_ldat.name_en.str.contains('catfish')]
catfish_aq_growth = catfish_aq.groupby(['year']).sum()
(catfish_aq_growth.iloc[1].food_mt - catfish_aq_growth.iloc[0].food_mt) / catfish_aq_growth.iloc[0].food_mt / 10
#capture
catfish_capt = cap_ldat.loc[cap_ldat.name_en.str.contains('catfish')]
catfish_capt_growth = catfish_capt.groupby(['year']).sum()
(catfish_capt_growth.iloc[1].food_mt - catfish_capt_growth.iloc[0].food_mt) / catfish_capt_growth.iloc[0].food_mt / 10


#ither growth
#aquaculture
oth_aq = aq_ldat.loc[aq_ldat.species_cat != 11]
oth_aq = oth_aq.loc[oth_aq.species_cat !=12]
oth_aq = oth_aq[~oth_aq.name_en.str.contains('catfish')]
oth_aq_growth = oth_aq.groupby(['year']).sum()
(oth_aq_growth.iloc[1].food_mt - oth_aq_growth.iloc[0].food_mt) / oth_aq_growth.iloc[0].food_mt / 10
#capture
oth_capt = cap_ldat.loc[cap_ldat.species_cat != 11]
oth_capt = oth_capt.loc[oth_capt.species_cat !=12]
oth_capt = oth_capt[~oth_capt.name_en.str.contains('catfish')]
oth_capt_growth = oth_capt.groupby(['year']).sum()
(oth_capt_growth.iloc[1].food_mt - oth_capt_growth.iloc[0].food_mt) / oth_capt_growth.iloc[0].food_mt / 10


