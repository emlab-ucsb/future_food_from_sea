
import pandas as pd
import statsmodels.api as sm
from statsmodels.iolib.summary2 import summary_col
import os
import sys
import numpy as np
import matplotlib.pyplot as plt




def supply_fcn(price, demand_multiplier, elasticity):
    quant = demand_multiplier * (price ** elasticity)
    return(quant)

def generate_inland_supply_data_high_elast():
    supply_dat = pd.DataFrame() 
    supply_dat['prices'] = range(0,20000,1)

    supply_dat['carp_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=208521.7215, elasticity=0.6))
    supply_dat['carp_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=2532.511557, elasticity=0.8))
    supply_dat['tilap_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=847948.7751, elasticity=0.24))
    supply_dat['tilap_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=192857.5493, elasticity=0.18))
    supply_dat['catfish_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=967.7305076, elasticity=1.08))
    supply_dat['catfish_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=36850.74826, elasticity=0.28))
    supply_dat['oth_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=752.8225015, elasticity=1.18))
    supply_dat['oth_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=62334.47232, elasticity=0.62))

    #Aggregate Inland Supply Data
    supply_dat['inland_aq'] = supply_dat['carp_aq'] + supply_dat['tilap_aq'] + supply_dat['catfish_aq'] + supply_dat['oth_aq']
    supply_dat['inland_capt'] = supply_dat['carp_capt'] + supply_dat['tilap_capt'] + supply_dat['catfish_capt'] + supply_dat['oth_capt']
    #Total inland supply for price responsiveness
    supply_dat['total_inland_supply'] = supply_dat['inland_aq'] + supply_dat['inland_capt']


    #generate product suply dat for year over year growth
    supply_dat['carp_aq_2050'] = supply_dat['carp_aq'] * (1 + 0.015)**23
    supply_dat['carp_capt_2050'] = supply_dat['carp_capt'] * (1 + 0.0169)**23
    supply_dat['tilap_aq_2050'] = supply_dat['tilap_aq'] * (1 + 0.0245)**23
    supply_dat['tilap_capt_2050'] = supply_dat['tilap_capt'] * (1 + 0.02)**23
    supply_dat['catfish_aq_2050'] = supply_dat['catfish_aq'] * (1 + 0.0196)**23
    supply_dat['catfish_capt_2050'] = supply_dat['catfish_capt'] * (1 + 0.012)**23
    supply_dat['oth_aq_2050'] = supply_dat['oth_aq'] * (1 + 0.017)**23
    supply_dat['oth_capt_2050'] = supply_dat['oth_capt'] * (1 + 0.007)**23
    
    #Generate toatl supply with growth
    supply_dat['inland_aq_2050'] = supply_dat['carp_aq_2050'] + supply_dat['tilap_aq_2050'] + supply_dat['catfish_aq_2050'] + supply_dat['oth_aq_2050']
    supply_dat['inland_capt_2050'] = supply_dat['carp_capt_2050'] + supply_dat['tilap_capt_2050'] + supply_dat['catfish_capt_2050'] + supply_dat['oth_capt_2050']
    supply_dat['total_inland_supply_2050'] = supply_dat['inland_aq_2050'] + supply_dat['inland_capt_2050']

    #Generate total supply of capture (constant)
    supply_dat['carp_capt_const'] = 1469134
    supply_dat['tilap_capt_const'] = 726428
    supply_dat['catfish_capt_const'] = 332754
    supply_dat['oth_capt_const'] = 7308516
    supply_dat['inland_capt_const'] = supply_dat['carp_capt_const'] + supply_dat['tilap_capt_const'] + supply_dat['catfish_capt_const'] + supply_dat['oth_capt_const']
    
    ##generate totals for scenarios
    #inalnd supply 2050 with aq growth and constant capture
    supply_dat['total_inland_supply_2050_constcapt'] = supply_dat['inland_aq_2050'] + supply_dat['inland_capt_const']
    #inland suply no growth with constant capture
    supply_dat['total_inland_supply_const_capt'] = supply_dat['inland_aq'] + supply_dat['inland_capt_const']
    #inland supply with growth in aq and price responsive capture
    supply_dat['total_inland_supply_2050_prc'] = supply_dat['inland_capt'] + supply_dat['inland_aq_2050']
    
    #save data
    supply_dat.to_csv('compiled_data/inland_supply_dat_high_elast.csv')


if __name__ == '__main__':
    generate_inland_supply_data_high_elast()