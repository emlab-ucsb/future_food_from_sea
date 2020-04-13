## Tracey Mangin
## FMSY testing

## attach librariess
library(tidyverse)
library(janitor)
library(purrr)
library(scales)
library(plotly)

## save path
savepath <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/Figures/"

## source functions
functions <- list.files(here::here("capture_functions"))

walk(functions, ~ here::here("capture_functions", .x) %>% source()) # load local functions

## define some parameters
proj_length <- 1000
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

## save the new version as of march 2020 separately for now
saveRDS(outputs, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/tshoot/long_projection.rds")


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

# saveRDS(steadystate_df, "~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/tshoot/steadystate_df_1000.rds")

fmsy_df <- outputs %>%
  filter(f_policy == "FMSY") %>%
  group_by(id_orig) %>%
  mutate(hnext = lead(harvest)) %>%
  ungroup() %>%
  mutate(diff = hnext - harvest,
         diff_tol = ifelse(abs(diff) <= 1, 0, 1))

fmsy_df2 <- fmsy_df %>%
  filter(time == max(outputs$time) -1) %>%
  select(id_orig, diff_tol) %>%
  filter(diff_tol == 1) 

## check FMSY stocks that do not appear to reach SS
fmsy_fig <- ggplot(fmsy_df, aes(x = time, y = BvBMSY, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fmsy_fig)

## 
fmsy_h_msy <- fmsy_df %>%
  filter(time == 1000,
         BvBMSY < 0.99 | BvBMSY > 1.01)

## check FMSY stocks that do not appear to reach SS
fmsy_fig2 <- ggplot(fmsy_df %>% filter(id_orig %in% fmsy_h_msy$id_orig), aes(x = time, y = BvBMSY, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fmsy_fig2)


## very off   
fmsy_h_msy2 <- fmsy_df %>%
  filter(time == 1000,
         BvBMSY < 0.05 | BvBMSY > 1.5)

## six stocks are not moving towards b = 1 (see above)
filt_msy_bmsy_df <-input_df_adj %>%
  mutate(BMSY = biomass / BvBmsy) %>%
  select(id_orig, msy, BMSY) %>%
  filter(msy > BMSY)

fmsy_test_df <- outputs %>%
  filter(id_orig %in% fmsy_h_msy2$id_orig) %>%
  mutate(MSY_g_BMSY = ifelse(id_orig %in% filt_msy_bmsy_df$id_orig, 1, 0)) %>% ## five of the six have MSY > BMSY
  filter(MSY_g_BMSY == 0)

## MSY for this last one is super duper small... harvest is close (about double 0.5376255 vs 0.224 mt)

## slow pokes
fmsy_conv <- fmsy_df %>%
  filter(id_orig %in% fmsy_h_msy$id_orig,
         !id_orig %in% fmsy_h_msy2$id_orig)

## all have small g values (<= 0.002), and most have MSY < 10

## compare against MSY
fmsy_h_df <- outputs %>%
  filter(f_policy == "FMSY",
         time == max(outputs$time)) %>%
  mutate(h_msy_diff = abs(MSY - harvest)) %>%
  filter(h_msy_diff > 1) %>%
  mutate(MSY_g_BMSY = ifelse(id_orig %in% filt_msy_bmsy_df$id_orig, 1, 0)) %>% ## five have MSY > BMSY
  filter(MSY_g_BMSY == 0)

## plot those that don't get to msy
msy_fig <- ggplot(outputs %>% filter(id_orig %in% fmsy_h_df$id_orig, f_policy == "FMSY"), aes(x = time, y = harvest / MSY, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(msy_fig)

## they are slowly moving towards MSY

## summary -- FMSY policy seems to be working for all stocks except those with MSY > BMSY. Those that do not reach steady-state (MSY & B/BMSY == 1) are moving in that direction.

