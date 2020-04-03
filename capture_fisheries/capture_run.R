## Tracey Mangin
## April 9, 2019
## Ocean Protein Supply
## Run file

## attach librariess
library(tidyverse)
library(janitor)
library(purrr)
library(scales)
library(plotly)
library(furrr)

## save path
savepath <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/public-repo/"

## source functions
functions <- list.files(here::here("capture_functions"))

walk(functions, ~ here::here("capture_functions", .x) %>% source()) # load local functions

## define some parameters
proj_length <- 200
beta <- 1.3
dd <- 0 # discount rate
delta  <- 1 / ( 1 + dd) # discount factor
tol <- 1
interval <- 3

## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/"

## prep upsides data
## ----------------------------------------------------------------------

## load the upsides data
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F)

## filter for starting year (2012)
upsides0 <- upsides %>%
  filter(Year == 2012)

## make input df (extract what you need)
input_df <- upsides0 %>%
  select(IdOrig, Country, SciName, SpeciesCat, RegionFAO, Dbase, CatchShare, Biomass, Profits, BvBmsy, FvFmsy, MSY, g, k, phi, Price, c) %>%
  clean_names(case = "snake") %>%
  rename(BvBmsy = bv_bmsy,
         FvFmsy = fv_fmsy)

## set f so that it maxes out
input_df_adj <- input_df %>%
  mutate(maxf = (phi + 1) / phi,
         FvFmsy = pmin(maxf, FvFmsy))

## prep management cost data
## ----------------------------------------------------------------------_

## load the management cost data
mcostr <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/mgmt_cost/Outputs_rec_method_2018.csv")
mcostm <- read_csv("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Data/capture_fisheries/mgmt_cost/Outputs_mean_method_2018.csv")

## select relevant columns (also, never make a dataframe like these again)
mcost_rec <- mcostr %>%
  select(country, scaledTC_r, scaledTC_m, currentCostMT_r, currentCostMT_m, currentCostMT, 
         countryConstant_S, costMT_OC, costMT_OA, costMT_CS) %>%
  mutate(approach = "recent")

mcost_mean <- mcostm %>%
  select(country, scaledTC_r, scaledTC_m, currentCostMT_r, currentCostMT_m, currentCostMT, 
         countryConstant_S, costMT_OC, costMT_OA, costMT_CS) %>%
  mutate(approach = "mean")

## make df that contains the cost per mt of management for each type of management, for each country.
mcost_rec2 <- mcost_rec %>%
  select(country, approach, currentCostMT, countryConstant_S, costMT_OA, costMT_OC, costMT_CS)

mcost_mean2 <- mcost_mean %>%
  select(country, approach, currentCostMT, countryConstant_S, costMT_OA, costMT_OC, costMT_CS)

mcostdf <- rbind(mcost_rec2, mcost_mean2) %>%
  rename(curr_cost_mt = currentCostMT,
         country_s = countryConstant_S)


## Find steady-state b for each fishery under each policy
## -----------------------------------------------------------------

ss_b_df0 <- input_df %>%
  select(id_orig, FvFmsy, phi, msy) %>%
  mutate(f_policy = "F_current",
         maxf = (phi + 1) / phi,
         FvFmsy = pmin(maxf, FvFmsy)) %>%
  select(id_orig, phi, msy, f_policy, FvFmsy)

ss_b_df_fmsy <- ss_b_df0 %>%
  mutate(FvFmsy = 1,
         f_policy = "FMSY") %>%
  select(id_orig, phi, msy, f_policy, FvFmsy)

ss_b_df <- rbind(ss_b_df0, ss_b_df_fmsy) %>%
  mutate(ss_b = calc_ss_b(phi_val = phi, fval = FvFmsy),
         ss_h = calc_h(f = FvFmsy, b = ss_b, MSY = msy))

write_csv(ss_b_df, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/steadys_b_h.csv")

## run the projection under FMSY and F current
## ----------------------------------------------------------------------

## run the projection model under two policies (Fcurrent and FMSY)
policy_vec <- c("FMSY", "F_current")

outputs <- cross(list(f_policy = policy_vec,
                      stockid = input_df$id_orig)) %>%
  map(project_forward,
      inputs = input_df_adj,
      projection_length = proj_length,
      delta = delta) %>%
  bind_rows()

saveRDS(outputs, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/projection_outputs.rds")

## save the new version as of march 2020 separately for now
# saveRDS(outputs, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/projection_outputs2.rds")


## using the projections, find when each stock reaches steady-state under each policy
## ---------------------------------------------------------------------

## Find the year that fisheries reach steady-state
steadystate_df <- cross(list(f_policy = policy_vec,
                             stockid = input_df$id_orig)) %>%
  map(find_ss_yr,
      ss_df = ss_b_df,
      output_df = outputs,
      tol_val = tol,
      int_val = interval) %>%
  bind_rows()

saveRDS(steadystate_df, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/steadystate_df.rds")

## check steady state outputs... make sure they are breaking in appropriate spots
sscheck <- steadystate_df %>%
  mutate(hdiff = abs(hbreak - h_ss),
         bdiff = abs(bbreak - b_ss))

## stocks that have MSY values > BMSY have strange results.
filt_msy_bmsy_df <-input_df_adj %>%
  mutate(BMSY = biomass / BvBmsy) %>%
  select(id_orig, msy, BMSY) %>%
  filter(msy > BMSY)

## There are 11 stocks that have MSY > BMSY. 
## Remove for now. 4 percent of MSY
sum(filt_msy_bmsy_df$msy) / sum(upsides0$MSY)

sscheck_filt <- sscheck %>%
  filter(!id_orig %in% filt_msy_bmsy_df$id_orig)

## check steady state outputs... make sure they are breaking in appropriate spots
sscheck2 <- sscheck %>%
  filter(!id_orig %in% filt_msy_bmsy_df$id_orig)

## checked the outputs in the other files (fmsy_test and fcurrent_test)


## update scaling number
upsides0_filt <- upsides0 %>%
  filter(!IdOrig %in% filt_msy_bmsy_df$id_orig)

sum(upsides0$Catch)


## Assign management costs
## ---------------------------------------------------------------------
mcostdf2 <- mcostdf %>%
  filter(approach == "recent") %>%
  select(country, cost_oc_mt_r = costMT_OC, cost_oa_mt_r = costMT_OA, cost_cs_mt_r = costMT_CS) %>%
  filter(country != "Other nei") %>%
  gather(policy, mcost_mt, cost_oc_mt_r:cost_cs_mt_r) %>%
  mutate(policy = ifelse(policy == "cost_oa_mt_r", "open_access",
                         ifelse(policy == "cost_oc_mt_r", "catch_control", "catch_share")))

avg_cost_df <- mcostdf2 %>%
  group_by(policy) %>%
  summarise(mean_mcost_mt = mean(mcost_mt, na.rm = T))

avg_oa <- avg_cost_df %>%
  filter(policy == "open_access") %>%
  select(mean_mcost_mt) %>%
  as.numeric()

avg_oc <- avg_cost_df %>%
  filter(policy == "catch_control") %>%
  select(mean_mcost_mt) %>%
  as.numeric()

avg_cs <- avg_cost_df %>%
  filter(policy == "catch_share") %>%
  select(mean_mcost_mt) %>%
  as.numeric()

## add the appropriate management cost values to the outputs.
## add sensitivity scenarios for revision

## vectors
sscenarios_vec <- c(rep("original_inputs", nrow(ss_b_df)), rep("improved_tech", nrow(ss_b_df)), rep("cs_price", nrow(ss_b_df)), rep("improved_tech_cs", nrow(ss_b_df)))

## sensitivity scenarios
supply_scenarios_df <- rbind(ss_b_df, ss_b_df, ss_b_df, ss_b_df)

supply_scenarios_df$scenario <- sscenarios_vec

supply_scenarios_df2 <- supply_scenarios_df %>%
  select(id_orig, scenario, f_policy, msy, FvFmsy:ss_h) %>%
  mutate(country = input_df$country[match(id_orig, input_df$id_orig)],
         dbase = input_df$dbase[match(id_orig, input_df$id_orig)],
         catch_share = input_df$catch_share[match(id_orig, input_df$id_orig)],
         g = input_df$g[match(id_orig, input_df$id_orig)],
         c = input_df$c[match(id_orig, input_df$id_orig)],
         c = ifelse(scenario %in% c("improved_tech", "improved_tech_cs"), c * 0.8, c)) %>%
  select(id_orig, country, dbase, catch_share, scenario, g, c, msy, f_policy:ss_h) %>%
  mutate(mc_pol = ifelse(catch_share == 1, "catch_share",
                         ifelse(catch_share == 1 & dbase == "RAM", "catch_control",
                                ifelse(f_policy == "F_current" & catch_share == 0 & dbase != "RAM", "open_access",
                                       ifelse(f_policy == "FMSY" & scenario %in% c("cs_price", "improved_tech_cs"), "catch_share", "catch_control")))))

## add the management cost
mcostdf3 <- mcostdf2 %>%
  rename(mc_pol = policy)

ss_outputs_scenarios <- supply_scenarios_df2 %>%
  left_join(mcostdf3) %>%
  ## apply the average cost per mt under each management
  mutate(mcost_mt = ifelse(is.na(mcost_mt) & mc_pol == "open_access", avg_oa,
                           ifelse(is.na(mcost_mt) & mc_pol == "catch_control", avg_oc, 
                                  ifelse(is.na(mcost_mt) & mc_pol == "catch_share", avg_cs, mcost_mt))),
         price = input_df$price[match(id_orig, input_df$id_orig)],
         beta = beta)

write_csv(ss_outputs_scenarios, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/final/ss_outputs_scenarios.csv")

# ## Add management costs
# outputs2 <- outputs %>%
#   mutate(country = input_df$country[match(id_orig, input_df$id_orig)],
#          dbase = input_df$dbase[match(id_orig, input_df$id_orig)],
#          catch_share = input_df$catch_share[match(id_orig, input_df$id_orig)],
#          policy = ifelse(catch_share == 1, "catch_share",
#                          ifelse(catch_share == 0 & dbase == "RAM", "catch_control",
#                                 ifelse(f_policy == "current" & catch_share == 0 & dbase != "RAM", "open_access", "catch_control")))) %>%
#   left_join(mcostdf2) %>%
#   ## for now, apply the average cost per mt under each management
#   mutate(mcost_mt = ifelse(is.na(mcost_mt) & policy == "open_access", avg_oa,
#                            ifelse(is.na(mcost_mt) & policy == "catch_control", avg_oc, 
#                                   ifelse(is.na(mcost_mt) & policy == "catch_share", avg_cs, mcost_mt))))

# ## make steady-state ouputs
# ss_outputs <- steadystate_df %>%
#   rename(time = time_ss) %>%
#   left_join(outputs2) %>%
#   select(id_orig, dbase, catch_share, country, f_policy, policy, tol, g, time, FvFMSY, BvBMSY, b_ss, biomass, h_ss, harvest, revenue, extract_cost, profit, disc_profit, MSY, mcost_mt) %>%
#   mutate(price = input_df$price[match(id_orig, input_df$id_orig)])
# 
# write_csv(ss_outputs, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/ss_outputs.csv")

