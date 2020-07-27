# future_food_from_sea
Scripts and other files relevant to the future of food from the sea study are detailed below.

## External data

All external data can be downloaded from the study’s Dryad repository. You may need to set your browser to allow pop-ups. Dryad may ask you for your email address in order to send you the files in our repository -- please note that you should receive the email in about five minutes but that this email has been known to end up in spam folders.

| File | Description | Reference |
| ----- | ----- | ----- |
| TS_FI_PRODUCTION.csv | FAO global production by country, area, source (e.g., aquaculture, capture), species, year | FAO. 2019. Fishery and Aquaculture Statistics. Global production by production source 1950-2017 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2019. [www.fao.org/fishery/statistics/software/fishstatj/en](www.fao.org/fishery/statistics/software/fishstatj/en) |
| CL_FI_PRODUCTION_SOURCE.csv | Descriptions and labels for “SOURCE” codes | FAO. 2019. Fishery and Aquaculture Statistics. Global production by production source 1950-2017 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2019. [www.fao.org/fishery/statistics/software/fishstatj/en](www.fao.org/fishery/statistics/software/fishstatj/en) |
| CL_FI_SPECIES_GROUPS.csv | Descriptions and labels for “SPECIES” codes | FAO. 2019. Fishery and Aquaculture Statistics. Global production by production source 1950-2017 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2019. [www.fao.org/fishery/statistics/software/fishstatj/en](www.fao.org/fishery/statistics/software/fishstatj/en) |
| CL_FI_WATERAREA_GROUPS.csv | Descriptions and labels for “AREA” codes (used in this study to distinguish between inland and marine fisheries) | FAO. 2019. Fishery and Aquaculture Statistics. Global production by production source 1950-2017 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2019. [www.fao.org/fishery/statistics/software/fishstatj/en](www.fao.org/fishery/statistics/software/fishstatj/en) |
| CL_FI_COUNTRY_GROUPS.csv | Descriptions and labels for “COUNTRY” codes | FAO. 2019. Fishery and Aquaculture Statistics. Global production by production source 1950-2017 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2019. [www.fao.org/fishery/statistics/software/fishstatj/en](www.fao.org/fishery/statistics/software/fishstatj/en) |
| FAOSTAT_data_3-10-2020.csv | FAOSTAT food balance sheets data (world) | FAO. FAOSTAT. (2020). [http://www.fao.org/faostat/en/#data/FBS](http://www.fao.org/faostat/en/#data/FBS) |
| ProjectionData.csv | Dataset of global capture fisheries included in Costello et al. 2016 study. Our study uses the same set of global fisheries in our projections. We use the fishery-level parameters for the year 2012 in our study. | Costello, C., Ovando, D., Clavelle, T., Strauss, C.K., Hilborn, R., Melnychuk, M.C., Branch, T.A., Gaines, S.D., Szuwalski, C.S., Cabral, R.B. and Rader, D.N., 2016. Global fishery prospects under contrasting management regimes. *Proceedings of the National Academy of Sciences*, 113(18), pp.5125-5129. |
| Outputs_rec_method_2018.csv | Data regarding the cost of fisheries management based on country and type of management | Mangin, T., Costello, C., Anderson, J., Arnason, R., Elliott, M., Gaines, S.D., Hilborn, R., Peterson, E. and Sumaila, R., 2018. Are fishery management upgrades worth the cost?. *PLOS One*, 13(9). |
| Outputs_mean_method_2018.csv | Data regarding the cost of fisheries management based on country and type of management | Mangin, T., Costello, C., Anderson, J., Arnason, R., Elliott, M., Gaines, S.D., Hilborn, R., Peterson, E. and Sumaila, R., 2018. Are fishery management upgrades worth the cost?. *PLOS One*, 13(9) |
| cai_2017.pdf | This document contains information that we used to construct demand curves. | Cai, J. & Leung, P. Short-term projection of global fish demand and supply gaps. Food and Agriculture Organization of the United Nations, 2017. |
| Fish_Phi_All_Constraints_95LT2.tiff, Bivalve_Phi_All_Constraints_95LT1.tiff, suitableDepthRaster.tif, Topo30_projected.tif, OHICountriesLatLong.tif, regionData.csv" | Data regarding environmental suitability of ocean grid cells for offshore marine bivalve and finfish aquaculture from Gentry et al. (2017) and download from this data repository: [https://knb.ecoinformatics.org/view/doi:10.5063/F1CF9N69](https://knb.ecoinformatics.org/view/doi:10.5063/F1CF9N69) | Gentry, R.R., Froehlich, H.E., Grimm, D., Kareiva, P., Parke, M., Rust, M., Gaines, S.D., Halpern, B.S. (2017) Mapping the global potential for marine aquaculture. *Nature Ecology & Evolution* 1(9) 1317-1324. |

## Data prep and initial conditions: Part I (folder: `init_cond`)

**`edible_conv.R`**

This script creates a table that we use to match each fishery with a conversion factor that allows for converting between live weight and edible animal protein. Conversions are based on those published in Edwards et al. 2019. We use this table to convert species-level historical landings as reported by the FAO as well as our projected harvests into edible food equivalents. 

Input files: 
- `TS_FI_PRODUCTION.csv`- global fishery production (historic, 1950-2017)
- `CL_FI_PRODUCTION_SOURCE.csv` - descriptions and labels for “SOURCE” codes used in `TS_FI_PRODUCTION.csv`
- `CL_FI_SPECIES_GROUPS.csv` - descriptions and labels for “SPECIES” codes used in `TS_FI_PRODUCTION.csv`
- `ProjectionData.csv` - fishery-level data (e.g., parameters)

Output file:
- `isscaap_food_conv_all.csv` - table to convert harvest to food equivalents

**`historic_prod.R`**

This script examines historical production from FAO data and organizes data for calculating initial production values. We filter the FAO historic production data for species harvested in marine environments. We also filter out plants and species that are not generally consumed as food (e.g., natural sponges, turtles, whales). Then we use the `isscaap_food_conv_all.csv` file produced above to match conversion values with production, matching by ISSCAAP category. This script creates Fig. 1.

Input files: 
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `isscaap_food_conv_all.csv` (produced in `edible_conv.R`)
- `TS_FI_PRODUCTION.csv` - global fishery production (historic, 1950-2017)
- `CL_FI_PRODUCTION_SOURCE.csv` - descriptions and labels for “SOURCE” codes used in `TS_FI_PRODUCTION.csv`
- `CL_FI_SPECIES_GROUPS.csv` - descriptions and labels for “SPECIES” codes used in `TS_FI_PRODUCTION.csv`
- `CL_FI_WATERAREA_GROUPS.csv` - descriptions and labels for “AREA” codes used in `TS_FI_PRODUCTION.csv`
- `CL_FI_COUNTRY_GROUPS.csv` - descriptions and labels for “COUNTRY” codes used in `TS_FI_PRODUCTION.csv`

Output files:
- `historical_prod_x_sp.csv` - historic production by species, with food equivalents
- `land_based_production.csv`- historic land-based production, with food equivalents
- `historical_prod.csv` - historic production, summarised by species group
- `init_mari_prod.csv` - initial mariculture production
- Fig. 1: (object is called `hist_food_fig2`)

## Prep inland fishery information (folder: `inland_supply_code_data`)

**`inland_supply_data.py`**

This script computes the inland supply curves for the four inland product groups (carp, tilapia, catfish, and other) for inland capture and land-based aquaculture. The script generates eight supply curves, with four scenarios for both inland food production and inland harvest quantity (4 x2). The four scenarios are as follows: (1) a baseline scenario, generated by `generate_inland_supply_baseline.py`; (2) low price elasticity scenario generated by `generate_inland_supply_low_elast.py`; (3) a high price elasticity scenario generated by `generate_inland_supply_high_elast.py`; (4) and a high growth scenario generated by `generate_inland_supply_high_growth.py`. 

All supply curves are generated by horizontal summation of the supply from the four product categories listed above. The raw data input required is aggregated harvest data for each of the four product categories, generated from FAO FishstatJ, as well as price and price elasticity information discussed in the supplemental information document. The necessary input information is aggregated and is available in the `inland_supply_code_data/raw_data` folder under the name `inland_suppy_info.csv`. 

The generation of the information contained in `inland_supply_info.csv` is also included in this folder. `Inland_supply_info.csv` contains aggregate growth and production data from inland supply as described in the supplemental information SI section 1.4. The scripts for the generation of the necessary production and growth parameters are `estimate_supply_parameters.py` and `estimate_growth_rates.py`, respectively. Both files require historical production data from inland supply, available as `land_based_production.csv`. Necessary parameters aggregated from the literature were added to `inland_supply_info.csv` manually.

Input file: 
- `land_based_production.csv` (from `historic_prod.R`)
- `Inland_supply_info.csv` (produced partially from `land_based_production.csv`)

Output files (all files contain projections for future land-based fisheries production):
- `inland_supply_dat.csv`
- `inland_supply_dat_low_elast.csv`
- `inland_supply_dat_high_elast.csv`
- `inland_supply_dat_high_growth.csv`
- `inland_supply_dat_harvest.csv`
- `inland_supply_dat_low_elast_harvest.csv`
- `inland_supply_dat_high_elast_harvest.csv`
- `inland_supply_dat_high_growth_harvest.csv`

## Data prep and initial conditions: Part II (folder: `init_cond`)

**`init_conditions.R`**

This script computes the current status for different food sectors and calculates other relevant indicators used throughout the text. The first part compares the most recent landings (and food equivalent quantities) from Costello et al. 2016 to the most recent values from the FAO. This information is used to scale projected harvests from the Costello et al. fisheries to represent global landings. Next we calculate initial marine capture fishery production, followed by initial mariculture production (broken down by fed and unfed), initial inland fishery production (broken down by capture and aquaculture), and initial meat production -- all calculated in terms of live weight and edible equivalents. Next we determine initial price values for the relevant categories in our study. Finally, we create data files with relevant initial condition information that will be used throughout the analyses.

Input files: 
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `isscaap_food_conv_all.csv` (produced in `edible_conv.R`)
- `historical_prod.csv` (produced in `historical_prod.R`)
- `historical_prod_x_sp.csv` (produced in `historical_prod.R`)
- `inland_supply_info.csv` (produced in python scripts) 
- `FAOSTAT_data_3-10-2020.csv` - FAOSTAT food balance sheets data (world)
- `init_mari_prod.csv` (produced in `historical_prod.R`)

Output files:
- `upside_fao_scale_df.csv` - table with values used to scale harvest outputs
- `init_pq_df.csv` - initial conditions, price and quantity (food)
- `init_pq_dfh.csv` - initital conditoins, price and quantity (harvest)

## Marine capture fisheries analysis (folder: `capture_fisheries`)

**`capture_run.R`**

This script runs the marine capture fisheries projection model, which projects future harvest, cost, revenue, and biomass over time under two management scenarios: FMSY, the fishing pressure that eventually leads to maximum sustainable yield, and F current, the current fishing pressure as parameterized by the global fishery database (Costello et al. 2016). It also determines steady-state harvest (the harvest that eventually will happen each year) for each fishery under the two harvest policies. We combine these outputs with management cost information. This script calls functions in the `capture_functions` folder.

Input files: 
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `Outputs_rec_method_2018.csv` - management cost data by country
- `Outputs_mean_method_2018.csv`- management cost data by country

Output files:
- `projection_outputs.rds` - fishery projection outputs
- `ss_outputs_scenarios.csv` - fishery steady-state outputs

**`capture_sc_run.R`**

This script uses the results from `capture_run.R` to create marine capture fishery supply curves, or production at a range of different prices. Production only occurs if it is profitable, and the fishery management policy (FMSY or F current) that results in the greatest steady-state profits is the option chosen, given a certain price. We include a number of sensitivity analyses and calculate associated supply curves.

Input files: 
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `projection_outputs.rds` (from `capture_run.R`)
- `ss_outputs_scenarios.csv` (from `capture_run.R`)
- `isscaap_food_conv_all.csv` (from `edible_conv.R`)
- `upside_fao_scale_df.csv` (from `init_conditions.R`)

Output files:
- `production_df.rds` - fishery-level supply curve outputs
- `production_df_smry.rds` - fishery-level supply curve outputs (summarized by price)
- `production_df_s1.rds` - fishery-level supply curve outputs (sensitivity analysis 1)
- `production_s1_smry.rds`- fishery-level supply curve outputs (sensitivity analysis 1, summarized by price)
- `production_df_s2.rds` - fishery-level supply curve outputs (sensitivity analysis 2)
- `production_s2_smry.rds` - fishery-level supply curve outputs (sensitivity analysis 2, summarized by price)
- `production_df_s3.rds` - fishery-level supply curve outputs (sensitivity analysis 3)
- `production_s3_smry.rds` - fishery-level supply curve outputs (sensitivity analysis 3, summarized by price)
- `fmsy_prod_df.rds` - fishery-level supply curve outputs for FMSY
- `fmsy_prod_smry.rds` - fishery-level supply curve outputs for FMSY (summarized by price)
- `f0_prod_df.rds` - fishery-level supply curve outputs for F0
- `f0_prod_smry.rds` - fishery-level supply curve outputs for F0 (summarized by price)

**`sc_outputs.R`**

This script combines the supply curve outputs into one data file.

Script: 
- `sc_outputs.R` (folder = `capture_fisheries`)

Input files: 
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `isscaap_food_conv_all.csv` (from `edible_conv.R`)
- `upside_fao_scale_df.csv` (from `init_conditions.R`)
- `production_df_smry.rds` (from `capture_sc_run.R`)
- `production_s1_smry.rds` (from `capture_sc_run.R`)
- `production_s2_smry.rds` (from `capture_sc_run.R`)
- `production_s3_smry.rds` (from `capture_sc_run.R`)
- `fmsy_prod_smry.rds` (from `capture_sc_run.R`)

Output file:
- `harvest_x_price_x_conversion.csv` - marine capture fishery supply curve outputs combined

## Mariculture analysis (folder: `aquaculture`)

**`Step1_format_gentry_data.R`**

This script formats the finfish and bivalve aquaculture habitat suitability maps from Gentry et al. 2017 for use in the present analysis.

Input files:
- `Fish_Phi_All_Constraints_95LT2.tiff`  - raster with finfish growth potentials (phis)
- `Bivalve_Phi_All_Constraints_95LT1.tiff` - raster with bivalve growth potentials (phis)
- `suitableDepthRaster.tif` - raster marking cells within depth limits
- `Topo30_projected.tif` - raster describing ocean depth
- `OHICountriesLatLong.tif` - raster delineating country EEZs

Output files:
- `Fish_layers.tif` - contains the number of suitable cells, the phi value, the years to harvest size, and the annual yield per cell (mt) for finfish mariculture
- `Bivalve_layers.tif` - contains the number of suitable cells, the phi value, the years to harvest size, and the annual yield per cell (# indivs) for bivalve mariculture
- `Farm_layers.tif` - contains the area of farms in each ocean grid cell and the depth of farms in each ocean grid cell
- `Farm_eez.tif` - contains the identify of the EEZ each farm falls inside

**`Step2_aqua_production.R`**

This script assembles the costs associated with the aquaculture production potential estimated by Gentry et al. 2017 including upfront capital costs (e.g. equipment) and annual operating costs (e.g. fuel, labor, feed, and equipment maintenance). The costs are estimated for every cell of production potential.

Input files:
- `Fish_layers.tif` (from `Step1_format_gentry_data.R`)
- `Bivalve_layers.tif` (from `Step1_format_gentry_data.R`)
- `Farm_layers.tif` (from `Step1_format_gentry_data.R`)
- `Farm_eez.tif` (from `Step1_format_gentry_data.R`)
- `regionData.csv` -- data frame with EEZ and other jurisdictional attributes for each ocean cell included in the analysis


Output file:
- `Aqua_production_results.csv` - contains the finfish and bivalves production potential of each cell of ocean as well as the component costs for producing that potential

**`Step3_format_wc_scurve_for_merge.R`**

This script prepares the results of the capture fisheries analysis for use in the mariculture analysis where capture fisheries production gets used to produce aquaculture feed.

Input file:
- `Harvest_x_price_x_conversion.csv` (from `sc_outputs.R` in capture analysis) - contains the capture fisheries supply curve with production at price for each capture fisheries scenario and commercial food group

Output file:
- `wc_supply_curve.Rds` - a cleaned version of the capture fisheries supply curve for use in the aquaculture analysis

**`Step4_visualize_feed_constraints.R`**

This script calculates the amount of capture fisheries production converted to fish meal and fish oil and the proportion of FM/FO directed to aquaculture in each of the aquaculture scenarios.

Input file:
- `wc_supply_curve.Rds` (from `Step3_format_wc_scurve_for_merge.R`)

Output file:
- `Wc_feed_availability_scen2-4.csv` - the FM/FO and feed production at each price for each combination of capture fisheries and aquaculture scenarios

**`Step5_aqua_costs.R`**

This script calculates the costs, revenues, and profits associated with the production potential mapped by Gentry et al. 2017 and thus identifies cells where aquaculture production would be profitable.

Input file:
- `Aqua_production_results.csv` (from `Step2_aqua_production.R`) - maps suitable sites for finfish and bivalve aquaculture as estimated by Gentry et al. 2017 and the capital and operating costs associated with that production potential

Output file:
- `aqua_cost_per_production_results.Rds` - provides the costs, revenues, and profits associated with aquaculture production

**`Step6_supply_curve_scenario1_unconstrained.R`**

This script constructs the aquaculture supply curve for the scenario in which aquaculture production potential is not limited by capture fisheries production.

Input file:
- `aqua_cost_per_production_results.Rds` (from `Step5_aqua_costs.R`) - contains maps of aquaculture production potential as well as the costs, revenues, and profits associated with this production

Output files:
- A results file for each scenario where scenarios are defined by each combination of species and cost scalar

**`Step6_supply_curve_scenario2_byproducts_only.R`**

This script constructs the aquaculture supply curve for the scenario in which aquaculture production potential is limited by the availability of feed from only the by-products of capture fisheries.

Input files:
- `aqua_cost_per_production_results.Rds` (from `Step5_aqua_costs.R`) - contains maps of aquaculture production potential as well as the costs, revenues, and profits associated with this production
- `Wc_feed_availability_scen2-4.csv` (from `Step4_visualize_feed_constraints.R`) - describes the constraints on feed availability from capture fisheries

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, and cost scalar

**`Step6_supply_curve_scenario3_whole_fish_and_byproducts_only.R`**

This script constructs the aquaculture supply curve for the scenario in which aquaculture production potential is limited by the availability of feed from whole fish from directed reduction fisheries and the by-products of non-reduction capture fisheries.

Input files:
- `aqua_cost_per_production_results.Rds` (from `Step5_aqua_costs.R`) - contains maps of aquaculture production potential as well as the costs, revenues, and profits associated with this production
- `Wc_feed_availability_scen2-4.csv` (from `Step4_visualize_feed_constraints.R`) - describes the constraints on feed availability from capture fisheries

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, and cost scalar

**`Step6_supply_curve_scenario4_tech_advance.R`**

This script constructs the aquaculture supply curve for the scenario in which aquaculture production potential is limited by the availability from only the by-products of capture fisheries but technological advances in feed reduce the dependence on ingredients from capture fisheries.

Input files:
- `aqua_cost_per_production_results.Rds` (from `Step5_aqua_costs.R`) - contains maps of aquaculture production potential as well as the costs, revenues, and profits associated with this production
- `Wc_feed_availability_scen2-4.csv` (from `Step4_visualize_feed_constraints.R`) - describes the constraints on feed availability from capture fisheries

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, cost scalar, and technology advance

## Combining mariculture and marine capture fishery outputs (folder: `aquaculture`)

**`Step7_merge_supply_curves_scenario1_unconstrained.R`**

This script merges the capture fisheries supply curve with the aquaculture supply curve from the scenario in which aquaculture production potential is not limited by capture fisheries production.

Input files:
- `wc_supply_curve.Rds` (from `Step3_format_wc_scurve_for_merge.R`)
- All of the outputs from `Step6_supply_curve_scenario1_unconstrained.R`

Output files:
- A results file for each scenario where scenarios are defined by each combination of species and cost scalar

**`Step7_merge_supply_curves_scenario2_byproducts_only.R`**

This script merges the capture fisheries supply curve with the aquaculture supply curve from the scenario in which aquaculture production potential is limited by the availability of feed from only the by-products of capture fisheries.

Input files:
- `wc_supply_curve.Rds` (from `Step3_format_wc_scurve_for_merge.R`)
- All of the outputs from `Step6_supply_curve_scenario2_byproducts_only.R`

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, and cost scalar

**`Step7_merge_supply_curves_scenario3_byproducts_and_directed.R`**

This script merges the capture fisheries supply curve with the aquaculture supply curve from the scenario in which aquaculture production potential is limited by the availability of feed from whole fish from directed reduction fisheries and the by-products of non-reduction capture fisheries.

Input files:
- `wc_supply_curve.Rds` (from `Step3_format_wc_scurve_for_merge.R`)
- All of the outputs from `Step6_supply_curve_scenario3_byproducts_and_directed.R`

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, and cost scalar

**`Step7_merge_supply_curves_scenario4_tech_advance.R`**

This script merges the capture fisheries supply curve with the aquaculture supply curve from the scenario in which aquaculture production potential is limited by the availability from only the by-products of capture fisheries but technological advances in feed reduce dependence on ingredients from capture fisheries.

Input files:
- `wc_supply_curve.Rds` (from `Step3_format_wc_scurve_for_merge.R`)
- All of the outputs from `Step6_supply_curve_scenario4_tech_advance.R`

Output files:
- A results file for each scenario where scenarios are defined by each combination of capture fisheries scenario, species, cost scalar, and technology advance

**`Step8_assemble_merged_supply_curves.R`**

This script assembles the merged supply curves from each feed scenario.

Input files:
- All of the merged supply curve files produced by the scripts beginning with “Step7”

Output file:
- `WC_AQ_supply_curve_merged_all_scenarios.Rds`

## Combining marine and inland supply curves (folder: `analysis`)

**`all_supply_curves.R`**

This script combines the supply curves for all fishery sectors considered in this study (i.e., mariculture, marine capture fisheries, and inland fisheries) and all scenarios outlined in the study and supplemental information. 

Input files:
- `WC_AQ_supply_curve_merged_all_scenarios.Rds` (from `Step8_assemble_merged_supply_curves.R`)
- `inland_supply_dat.csv` (from python scripts)
- `inland_supply_dat_harvest.csv` (from python scripts)
- `inland_supply_dat_low_elast.csv` (from python scripts)
- `inland_supply_dat_low_elast_harvest.csv` (from python scripts)
- `inland_supply_dat_high_elast.csv` (from python scripts)
- `inland_supply_dat_high_elast_harvest.csv` (from python scripts)
- `inland_supply_dat_high growth.csv` (from python scripts)
- `inland_supply_dat_high growth_harvest.csv` (from python scripts)

Output files:
- `all_supply_curves.rds` - all supply curves combined
- `subset_supply_curves.rds` - subset of all supply curves combined (for main text analysis)

## Create demand curves (folder: `demand`)

**`demand_curves.R`**

This script creates demand curves for the three marine food sectors (marine capture, finfish mariculture, bivalve mariculture). We consider a number of different scenarios.  

Input files:
- `init_pq_df.csv` (from `init_conditions.R`)
- `cai_2017.pdf` (in repo, report containing parameters)

Output files:
- `demand_curves_final.rds` - demand curves outputs

## Perform analyses with all outputs (folder: `analysis`)

**`fig2_conceptual.R`**

This script creates Fig. 2, which is a conceptualization of different supply scenarios for capture fisheries and mariculture. 

Output file:
- `fig2_conceptual.png`

**`sc_combined.R`**

This script creates Fig. 3 and computes relevant values in our study, including 2050 production at initial prices for different supply curve scenarios.

Input files:
- `subset_supply_curves.rds` (from `all_supply_curves.R`)
- `init_pq_df.csv` (from `init_conditions.R`)
- `init_pq_dfh.csv` (from `init_conditions.R`)
- `ProjectionData.csv` - fishery-level data (e.g., parameters)
- `Projection_outputs.rds` (from `capture_run.R`)
- `isscaap_food_conv_all.csv` (from `edible_conv.R`)
- `upside_fao_scale_df.csv` (from `init_conditions.R`)
- `harvest_x_price_x_conversion.csv` (from `sc_outputs.R`)

Output file:
- `fig3.png`

**`sector_sp_outputs.R`**

This script creates Fig. 4 and calculates related values that appear in our study, including the intersections points for supply and demand curves.

Input files:
- `subset_supply_curves.rds` (from `all_supply_curves.R`)
- `init_pq_df.csv` (from `init_conditions.R`)
- `init_pq_dfh.csv` (from `init_conditions.R`)
- `demand_curves_final.rds` (from `demand_cruves.R`)

Output files:
- `sector_sp_prod_final.csv` - intersection points
- `fig4.png`

**`donut_fig.R`**

This script creates Fig. 5 and calculates related values that appear in our study.

Input files:
- `sector_sp_prod_final.csv` (from `sector_sp_outputs.R`)
- `init_pq_df.csv` (from `init_conditions.R`)

Output file:
- `fig5.png`

**`demand_figs.R`**

This script creates figures in the supplementary materials.

Input files:
- `subset_supply_curves.rds` (from `all_supply_curves.R`)
- `init_pq_df.csv` (from `init_conditions.R`)
- `init_pq_dfh.csv` (from `init_conditoins.R`)
- `demand_curves_final.rds` (from `demand_cruves.R`)
- `sector_sp_prod_final.csv` (from `sector_sp_outputs.R`)
- `harvest_x_price_x_conversion.csv` (from `sc_outputs.R`)

Output files:
- `si_1_updated.png`
- `si_donut.png`
- `Si_prod_all_sources.csv` - intersection points
- `si_prod_all_sources_harvest.csv` - intersection points

**`find_intersection.R`**

This script finds intersection points between all supply and demand curves. This information is used in supplementary materials. 

Input files:
- `all_supply_curves.rds` (from `all_supply_curves.R`)
- `demand_curves_final.rds` (from `demand_cruves.R`)

Output files:
- `id_df.rds` - table of ids
- `bivalve_intersects.rds` - intersection points
- `ff_intersects.rds` - intersection points
- `inland_intersects.rds` - intersection points
- `capture_intersects.rds` - intersection points
- `ps_all_intersects.rds` - intersection points

**`sensitivity_analysis.R`**

This script organizes information for sensitivity analyses.

Input files:
- `id_df.rds` (from `find_intersection.R`)
- `demand_curves_final.rds` (from `demand_cruves.R`)
- `all_supply_curves.rds` (from `all_supply_curves.R`)
- `bivalve_intersects.rds` (from `find_intersection.R`)
- `ff_intersects.rds` (from `find_intersection.R`)
- `inland_intersects.rds` (from `find_intersection.R`)
- `capture_intersects.rds` (from `find_intersection.R`)
- `ps_all_intersects.rds` (from `find_intersection.R`)

Output file:
- `all_si_outputs.csv` - outputs for SI

**`sensitivity_Fig.R`**

This script runs a sensitivity analysis with the range of scenarios that we test in our study.

Input file:
- `all_si_outputs.csv` (from `sensitivity_analysis.R`)

Output files:
- `si_ridge.png`
- `mean_ridges.csv`





