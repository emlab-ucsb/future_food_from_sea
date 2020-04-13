## fcurrent test
## run this after fmsy_test (use same data)

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


## open the projection under FMSY and F current
## ----------------------------------------------------------------------
outputs <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/Capture/data/tshoot/long_projection.rds")


fcurr_df <- outputs %>%
  filter(f_policy == "F_current") %>%
  group_by(id_orig) %>%
  mutate(hnext = lead(harvest)) %>%
  ungroup() %>%
  mutate(diff = hnext - harvest,
         diff_tol = ifelse(abs(diff) <= 1, 0, 1))

fcurr_df2 <- fcurr_df %>%
  filter(time == max(outputs$time - 1)) %>%
  select(id_orig, diff_tol) %>%
  filter(diff_tol == 1) 

## one does not reach ss: FAO-SPNWA-ANCHOWA-1990-2010-CHING
filt_msy_bmsy_df <-input_df_adj %>%
  mutate(BMSY = biomass / BvBmsy) %>%
  select(id_orig, msy, BMSY) %>%
  filter(msy > BMSY)

## it has the MSY > BMSY problem

## check Fcurr stocks that do not appear to reach ss biomass
fcurr_fig <- ggplot(fcurr_df, aes(x = time, y = BvBMSY, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_fig)

## add ss_b and ss_h to df
fc_ss <- ss_b_df %>%
  filter(f_policy == "F_current") %>%
  select(id_orig, ss_b, ss_h) 

fcurr_df_ss <- fcurr_df %>%
  left_join(fc_ss)
  
fcurr_b_ss <- fcurr_df_ss %>%
  filter(time == 1000,
         BvBMSY < ss_b - 0.01 | BvBMSY > ss_b + 0.01)
## 85 don't make it within 0.01 b

## check FMSY stocks that do not appear to reach SS biomass
fcurr_bfig <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_b_ss$id_orig), aes(x = time, y = BvBMSY / ss_b, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig)

## 2072-FAO-34-38 has the most extreme difference... low g, low MSY, slowly moving towards "ss"

fcurr_bfig2 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_b_ss$id_orig,
                                             id_orig != "2072-FAO-34-38"), aes(x = time, y = BvBMSY / ss_b, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig2)


## 17415-FAO-41-38 has the another extreme difference... low g, low MSY, slowly moving towards "ss"

fcurr_bfig3 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_b_ss$id_orig,
                                             !id_orig %in% c("2072-FAO-34-38", "17415-FAO-41-38", filt_msy_bmsy_df$id_orig)), ## remove MSY > BMSY too
                      aes(x = time, y = BvBMSY / ss_b, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig3)
## all reach a SS near the value (within 5% of estimate B/BMSY SS) or are appraoching that value except WGSSDS-SOLECS-1970-2011-NEUBAUE, which has g = 0 and msy = 0.224

## example of one that flattens out below ss b/bmsy: SWFSC-SBELLYROCKPCOAST-1950-2005-BRANCH
## no fishing is happening, bmax is actually lower than ss/bmsy
fcurr_b_ss2 <- fcurr_b_ss %>%
  mutate(fval = ifelse(FvFMSY == 0, 1, 0),
         ssb0 = ifelse(round(BvBMSY, digits = 2) == 2.4, 1, 0),
         ss_b_adj = ifelse(fval ==1 & ssb0 == 1, 2.4, ss_b)) %>%
  filter(BvBMSY < ss_b_adj - 0.01 | BvBMSY > ss_b_adj + 0.01)

fcurr_bfig4 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_b_ss2$id_orig, !id_orig %in% c("2072-FAO-34-38", "17415-FAO-41-38", filt_msy_bmsy_df$id_orig, "WGSSDS-SOLECS-1970-2011-NEUBAUE")), ## remove MSY > BMSY too
                      aes(x = time, y = BvBMSY / ss_b, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig4)

## still a bunch that get within about 5% and level off

## compare against h_ss
fcurr_h_ss <- fcurr_df_ss %>%
  filter(!id_orig %in% filt_msy_bmsy_df$id_orig) %>% 
  mutate(rel_h = harvest / ss_h,
         hdiff = abs(harvest - ss_h)) %>%
  filter(time == max(fcurr_df_ss$time)) %>%
  filter(harvest < ss_h - 1 | harvest > ss_h + 1) 

## 33 do not come within 1 mt

fcurr_bfig4 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_h_ss$id_orig, !id_orig %in% c("2072-FAO-34-38", "17415-FAO-41-38", filt_msy_bmsy_df$id_orig, "WGSSDS-SOLECS-1970-2011-NEUBAUE")), ## remove MSY > BMSY too
                      aes(x = time, y = harvest / ss_h, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig4)

## Lumped-Roughhead grenadier-FaoRegion27 is very high... looked into it... slowly on it's way to h_ss (<2mt away)
fcurr_bfig5 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_h_ss$id_orig, !id_orig %in% c("2072-FAO-34-38", 
                                                                                              "17415-FAO-41-38", 
                                                                                              filt_msy_bmsy_df$id_orig, 
                                                                                              "WGSSDS-SOLECS-1970-2011-NEUBAUE",
                                                                                              "Lumped-Roughhead grenadier-FaoRegion27")), ## remove MSY > BMSY too
                      aes(x = time, y = harvest / ss_h, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig5)

## leveling off within 5%
## check NWFSC-WROCKPCOAST-1916-2011-STACHURA -- bmax < estimate ssb -- see how many this is the case for
View(fcurr_h_ss)

fcurr_h_ss2 <- fcurr_h_ss %>%
  filter(round(BvBMSY, digits = 2) != 2.4)
## down to 11 --all 4 that have the max FvFmsy are moving towards 0

fcurr_h_ss3 <- fcurr_h_ss2 %>%
  filter(!id_orig %in% c("3657-FAO-27-34", "14403-FAO-57-38", "14192-FAO-87-38", "Lumped-Roughhead grenadier-FaoRegion21"))

## the two stocks with the > msy values are approaching h_ss
fcurr_h_ss4 <- fcurr_h_ss3 %>%
  filter(!id_orig %in% c("Lumped-Alaska pollock(=Walleye poll.)-FaoRegion61", "184-FAO-47-33"))
## low gs?

ggplot(upsides0 %>% filter(g < 0.1), aes(x = g)) +
  geom_histogram(binwidth = 0.01)

## they are low gs...
fcurr_bfig6 <- ggplot(fcurr_df_ss %>% filter(id_orig %in% fcurr_h_ss4$id_orig, !id_orig %in% c("Lumped-Roughhead grenadier-FaoRegion27", "17415-FAO-41-38")), aes(x = time, y = harvest / ss_h, group = id_orig)) +
  geom_line(size = 0.25, alpha = 0.25)

ggplotly(fcurr_bfig6)
## they are getting there

# ## summary -- Fcurrent policy seems to be working for all stocks except those with MSY > BMSY. Those that do not reach steady-state (ss_b and ss_h) are moving in that direction.



