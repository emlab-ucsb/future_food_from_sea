# -*- coding: utf-8 -*-
"""
Created on Tue Mar 10 15:24:47 2020

@author: jmaier
"""

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

def generate_inland_supply_data_low_elast():
    supply_dat = pd.DataFrame() 
    supply_dat['prices'] = range(0,20000,1)

    #generate price responsive Inland Supply Data
    supply_dat['carp_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=2267083.635, elasticity=0.3))
    supply_dat['carp_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=60996.71167, elasticity=0.4))
    supply_dat['tilap_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=2052769.746, elasticity=0.12))
    supply_dat['tilap_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=374295.5033, elasticity=0.09))
    supply_dat['catfish_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=67428.84794, elasticity=0.54))
    supply_dat['catfish_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=110734.9714, elasticity=0.14))
    supply_dat['oth_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=70093.17295, elasticity=0.64))
    supply_dat['oth_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=674961.1013, elasticity=0.31))


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
    supply_dat.to_csv('compiled_data/inland_supply_dat_low_elast.csv')


if __name__ == '__main__':
    generate_inland_supply_data_low_elast()