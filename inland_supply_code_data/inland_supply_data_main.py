from create_baseline_inland_supply_data import *
from create_low_elasticity_scenario_inland_supply_data import *
from create_high_elasticity_scenario_inland_supply_data import *
from create_high_growth_scenario_inland_supply_data import *
from create_baseline_inland_supply_data_harvest import *
from create_low_elasticity_scenario_inland_supply_data_harvest import *
from create_high_elasticity_scenario_inland_supply_data_harvest import *
from create_high_growth_scenario_inland_supply_data_harvest import *

def create_supply_data():
    generate_inland_supply_data_baseline()
    generate_inland_supply_data_low_elast()
    generate_inland_supply_data_high_elast()
    generate_inland_supply_data_high_growth()
    
def create_supply_data_harvest():
    generate_inland_supply_data_baseline_harvest()
    generate_inland_supply_data_low_elast_harvest()
    generate_inland_supply_data_high_elast_harvest()
    generate_inland_supply_data_high_growth_harvest()
        
    
if __name__ == '__main__':
    create_supply_data()
    create_supply_data_harvest()