
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

    
def generate_inland_supply_data_high_growth():
    supply_dat = pd.DataFrame() 
    supply_dat['prices'] = range(0,20000,1)

    #generate price responsive Inland Supply Data
    supply_dat['carp_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=19179.40198, elasticity=0.9))
    supply_dat['carp_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=105.1468942, elasticity=1.2))
    supply_dat['tilap_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=350266.8171, elasticity=0.36))
    supply_dat['tilap_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=99370.77519, elasticity=0.27))
    supply_dat['catfish_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=13.88874886, elasticity=1.62))
    supply_dat['catfish_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=12263.3133, elasticity=0.42))
    supply_dat['oth_aq'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=8.085548062, elasticity=1.77))
    supply_dat['oth_capt'] = supply_dat.prices.apply(lambda x: supply_fcn(x, demand_multiplier=5756.756104, elasticity=0.93))


    #Aggregate Inland Supply Data
    supply_dat['inland_aq'] = supply_dat['carp_aq'] + supply_dat['tilap_aq'] + supply_dat['catfish_aq'] + supply_dat['oth_aq']
    supply_dat['inland_capt'] = supply_dat['carp_capt'] + supply_dat['tilap_capt'] + supply_dat['catfish_capt'] + supply_dat['oth_capt']
    #Total inland supply for price responsiveness
    supply_dat['total_inland_supply'] = supply_dat['inland_aq'] + supply_dat['inland_capt']


    #generate product suply dat for year over year growth
    supply_dat['carp_aq_2050'] = supply_dat['carp_aq'] * (1 + 0.03)**23
    supply_dat['carp_capt_2050'] = supply_dat['carp_capt'] * (1 + 0.0338)**23
    supply_dat['tilap_aq_2050'] = supply_dat['tilap_aq'] * (1 + 0.049)**23
    supply_dat['tilap_capt_2050'] = supply_dat['tilap_capt'] * (1 + 0.04)**23
    supply_dat['catfish_aq_2050'] = supply_dat['catfish_aq'] * (1 + 0.0392)**23
    supply_dat['catfish_capt_2050'] = supply_dat['catfish_capt'] * (1 + 0.024)**23
    supply_dat['oth_aq_2050'] = supply_dat['oth_aq'] * (1 + 0.034)**23
    supply_dat['oth_capt_2050'] = supply_dat['oth_capt'] * (1 + 0.014)**23
    
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
    supply_dat.to_csv('compiled_data/inland_supply_dat_high_growth.csv')


if __name__ == '__main__':
    generate_inland_supply_data_high_growth()