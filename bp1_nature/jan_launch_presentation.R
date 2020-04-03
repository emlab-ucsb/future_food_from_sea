## Tracey Mangin
## January 6, 2020
## BP1 presentation

library(tidyverse)
library(viridis)
library(scales)
library(ggrepel)

## set pathstart
pathstart <- "~/Box/SFG Centralized Resources/Projects/Ocean Protein/"

## read in relevant outputs/processed data
upsides <- read.csv("~/Box/SFG Centralized Resources/Projects/Upsides/upside-share/ProjectionData.csv", stringsAsFactors = F) 

## conversion factors
conv_vals <- read_csv("bp1_nature/processed_data/isscaap_food_conv.csv")

wc_supply <- readRDS("~/Box/SFG Centralized Resources/Projects/Ocean Protein/Outputs/wild_fisheries_agg_sup.rds") %>%
  mutate(scenario = ifelse(scenario == "FMSY", "MSY", scenario))

## current supply curve
mari_prod <- read_csv("~/GitHub/blue_paper_1/processed_data/current_mari_prod.csv")


supdatadir <- "aquaculture_v2/output"
data <- read.csv(file.path(supdatadir, "ocean_supply_curve_scenarios1-4_feed1.csv"))

# Add Scneario title
data <- data %>% 
  mutate(scenario_title=plyr::revalue(scenario, c("Scenario 1"="Scenario 1:\nFeed from by-products only",
                                                  "Scenario 2"="Scenario 2:\nFeed from by-products + whole fish",
                                                  "Scenario 3a"="Scenario 3a:\nFeed from by-products + whole fish\n(+50% reduction in FM/FO demand)",
                                                  "Scenario 3b"="Scenario 3b:\nFeed from by-products + whole fish\n(+75% reduction in FM/FO demand)",
                                                  "Scenario 3c"="Scenario 3c:\nFeed from by-products + whole fish\n(+95% reduction in FM/FO demand)",
                                                  "Scenario 4"="Scenario 4:\nFeed unconstrained\nby capture fisheries"))) %>% 
  filter(price<=10000)

## nature paper scenarios
np_scen_vec <- c("Scenario 2", "Scenario 3a", "Scenario 3c", "Scenario 4")

## get scenarios that we are interested in
data_np <- data %>%
  filter(scenario %in% np_scen_vec) %>%
  mutate(sector = str_replace(sector, "aquaculture", "mariculture")) %>%
  mutate(scen_lab = ifelse(scenario == "Scenario 2", "Policy reforms",
                           ifelse(scenario == "Scenario 3a", "Tech innovation",
                                  ifelse(scenario == "Scenario 3c", "Tech innovation (Ambitious)", "Scenario D"))))

data_np$sector <- factor(data_np$sector, levels = c("Finfish mariculture", "Bivalve mariculture", "Capture fisheries"))


## figure 1: food from the sea vs. all other food
pig <- 115
poultry <- 110
fish_all <- 98
beef <- 68
ffts <- 62
inland <- fish_all - ffts

food_cats <- tibble(cat = c("Pork", "Poultry", "Beef", "Food from the sea", "Inland seafood"),
                    prod_mmt = c(pig, poultry, beef, ffts, inland)) %>%
  mutate(group = factor(cat, levels = c("Inland seafood", "Food from the sea", "Beef", "Poultry", "Pork")),
         cumulative = cumsum(prod_mmt),
         midpoint = cumulative - prod_mmt / 2,
         label = paste0(round((prod_mmt / sum(food_cats$prod_mmt) * 100)), "%"),
         hl = ifelse(cat == "Food from the sea", "hl", "nhl"))

## pie chart
cat_fig <- ggplot(food_cats, aes(x="", y = prod_mmt, fill = group)) +
  geom_bar(width = 1, stat = "identity", color = "white", alpha = 0.9) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_viridis(option = "plasma", discrete = TRUE) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(y = midpoint, label = label), size = 8) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 18))

ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/food_supply.png", cat_fig, width = 8, height = 8, units = "in", dpi = 300)

## theoretical figs
## -------------------------------------------------------------

N = 10000

DF1 = data.frame(PwOA=seq(.88,4.5,length.out=N)) %>%
  mutate(PwMSY=seq(.5,4.5,length.out=N)) %>%
  mutate(QwM = seq(1,3.422,length.out = N)) %>%
  mutate(QwOA = 10/PwOA - 8/PwOA^2) %>%
  mutate(PwM = -.3/(QwM-3.5) +.6 ) %>%
  mutate(QwMSY = 3.5) %>%
  select(QwOA,PwOA,QwM,PwM,QwMSY,PwMSY)

DF2 = data.frame(Pm1=seq(.88,4.5,length.out=N)) %>%
  mutate(Pm2=seq(.9,4.5,length.out=N)) %>%
  mutate(tmp = seq(1,3.422,length.out = N)) %>%
  mutate(Qm1 = 10/Pm1 - 8/Pm1^2) %>%
  mutate(Pm3 = -.3/(tmp-3.5) +.7 ) %>%
  mutate(Qm2 = 1.4) %>%
  mutate(Qm4 = .7*(tmp-1)+tmp) %>%
  mutate(Qm3 = .1*(tmp-1)+tmp) %>%
  mutate(Pm4 =  -.3/(tmp-3.5) +.7 ) %>%
  select(Qm1,Pm1,Qm2,Pm2,Qm3,Pm3,Qm4,Pm4)

DF_Tracey = bind_cols(DF1,DF2) #This is the dataframe you need


P2a = ggplot(data=DF_Tracey) +
  geom_path(aes(x=QwOA,y=PwOA),size=2) +
  geom_path(aes(x=QwM,y=PwM),size=2,color="blue") +
  geom_line(aes(x=QwMSY,y=PwMSY),size=2,color="green") +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  theme(axis.line.x=element_line(color="black",size=2)) +
  theme(axis.line.y=element_line(color="black",size=2)) +
  theme(axis.title=element_text(color="black",size=20)) +
  xlab("Q") +
  ylab("P")
P2a

P2b = ggplot(data=DF_Tracey) +
  geom_path(aes(x=Qm1,y=Pm1),size=2) +
  geom_line(aes(x=Qm2,y=Pm2),size=2,color="green") +
  geom_path(aes(x=Qm3,y=Pm3),size=2,color="blue") +
  geom_path(aes(x=Qm4,y=Pm4),size=2,color="red") +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  theme(axis.line.x=element_line(color="black",size=2)) +
  theme(axis.line.y=element_line(color="black",size=2)) +
  theme(axis.title=element_text(color="black",size=20)) +
  xlab("Q") +
  ylab("P")
P2b

# ggsave("P2a.png",P2a)
# ggsave("P2b.png",P2b)

## organize the data
## -------------------------------

wild1 <- DF_Tracey %>%
  select(quantity = QwOA, price = PwOA) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwOA")

wild2 <- DF_Tracey %>%
  select(quantity = QwM, price = PwM) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwM")

wild3 <- DF_Tracey %>%
  select(quantity = QwMSY, price = PwMSY) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwMSY")

m1 <- DF_Tracey %>%
  select(quantity = Qm1, price = Pm1) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm1")

m2 <- DF_Tracey %>%
  select(quantity = Qm2, price = Pm2) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm2")

m3 <- DF_Tracey %>%
  select(quantity = Qm3, price = Pm3) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm3")


m4 <- DF_Tracey %>%
  select(quantity = Qm4, price = Pm4) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm4")

all_curves <- rbind(wild1, wild2, wild3, m1, m2, m3, m4)

all_curves$sector <- factor(all_curves$sector, levels = c("Wild fisheries", "Mariculture"))

fig2_wild <- all_curves %>%
  filter(sector == "Wild fisheries") %>%
  rename(management = scenario)

fig2_mar <- all_curves %>%
  filter(sector == "Mariculture")

##
wild_labs <- fig2_wild %>%
  group_by(management) %>%
  filter(price == max(price)) %>%
  ungroup()

wild_labs2 <- data.frame(lab = c('Open-access', 'Rational reform', 'MSY'))

wild_labs3 <- cbind(wild_labs, wild_labs2) %>%
  select(-management) %>%
  mutate(quantity = quantity - 0.04)

## conceptual fig, wild

wild_fig1 <- ggplot(data = fig2_wild %>% filter(management == "SwOA"), aes(x = quantity, y = price, group = management, color = management)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = wild_labs3 %>% filter(lab == "Open-access"), aes(x = quantity, y = price, label = lab), 
                  # size = 7, hjust = 0, inherit.aes = F) +
  scale_color_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                     values = c("SwOA" = "black",
                                "SwM" = "#5BA1E5",
                                "SwMSY" = "#000080"),
                     labels = c("SwOA" = "Open-access",
                                 "SwM" = "Rational reform",
                                 "SwMSY" = "MSY")) +
  xlim(1, 3.5) +
  ylim(0.5, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20))


ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/wild_concept1.png", wild_fig1, width = 8, height = 10, units = "in", dpi = 300)


wild_fig2 <- ggplot(data = fig2_wild %>% filter(management != "SwMSY"), aes(x = quantity, y = price, group = management, color = management)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = wild_labs3 %>% filter(lab == "Open-access"), aes(x = quantity, y = price, label = lab), 
  # size = 7, hjust = 0, inherit.aes = F) +
  scale_color_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                     values = c("SwOA" = "black",
                                "SwM" = "#5BA1E5",
                                "SwMSY" = "#000080"),
                     labels = c("SwOA" = "Open-access",
                                "SwM" = "Rational reform",
                                "SwMSY" = "MSY")) +
  xlim(1, 3.5) +
  ylim(0.5, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20))


ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/wild_concept2.png", wild_fig2, width = 8, height = 10, units = "in", dpi = 300)

wild_fig3 <- ggplot(data = fig2_wild, aes(x = quantity, y = price, group = management, color = management)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = wild_labs3 %>% filter(lab == "Open-access"), aes(x = quantity, y = price, label = lab), 
  # size = 7, hjust = 0, inherit.aes = F) +
  scale_color_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                     values = c("SwOA" = "black",
                                "SwM" = "#5BA1E5",
                                "SwMSY" = "#000080"),
                     labels = c("SwOA" = "Open-access",
                                "SwM" = "Rational reform",
                                "SwMSY" = "MSY")) +
  xlim(1, 3.5) +
  ylim(0.5, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20))


ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/wild_concept3.png", wild_fig3, width = 8, height = 10, units = "in", dpi = 300)

## conceptual figs, no sector
## conceptual fig, wild

oa_curve <- ggplot(data = fig2_wild %>% filter(management == "SwOA"), aes(x = quantity, y = price, group = management, color = management)) +
  geom_path(alpha = 0.9, size = 2) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = wild_labs3 %>% filter(lab == "Open-access"), aes(x = quantity, y = price, label = lab), 
  # size = 7, hjust = 0, inherit.aes = F) +
  scale_color_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                     values = c("SwOA" = "black",
                                "SwM" = "#5BA1E5",
                                "SwMSY" = "#000080"),
                     labels = c("SwOA" = "Open-access",
                                "SwM" = "Rational reform",
                                "SwMSY" = "MSY")) +
  xlim(1, 3.5) +
  ylim(0.5, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 30),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 30),
        strip.text = element_text(size = 30))


ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/oa_curve.png", oa_curve, width = 9, height = 12, units = "in", dpi = 300)


s_curve <- ggplot(data = fig2_wild %>% filter(management != "SwMSY"), aes(x = quantity, y = price, group = management, color = management)) +
  geom_path(alpha = 0.9, size = 2) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = wild_labs3 %>% filter(lab == "Open-access"), aes(x = quantity, y = price, label = lab), 
  # size = 7, hjust = 0, inherit.aes = F) +
  scale_color_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                     values = c("SwOA" = "black",
                                "SwM" = "#5BA1E5",
                                "SwMSY" = "#000080"),
                     labels = c("SwOA" = "Open-access",
                                "SwM" = "Rational reform",
                                "SwMSY" = "MSY")) +
  xlim(1, 3.5) +
  ylim(0.5, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 30),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 30),
        strip.text = element_text(size = 30))


ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/s_curve.png", s_curve, width = 9, height = 12, units = "in", dpi = 300)


## conceptual mariculture figs

mar_labs <- fig2_mar %>%
  group_by(scenario) %>%
  filter(price == max(price)) %>%
  ungroup() 

mar_labs2 <- data.frame(lab = c('Open-access', 'Policy restrictions', 'Sustainable\npolicy reform', 'Technological advances'))

mar_labs3 <- cbind(mar_labs, mar_labs2) %>%
  select(-scenario)

all_labs <- rbind(mar_labs3, wild_labs3)



unfed1 <- ggplot(data = fig2_mar %>% filter(scenario %in% c("Sm1")), aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = mar_labs3 %>% filter(lab != "Technological advances"), aes(x = quantity, y = price, label = lab), 
  #                 size = 7, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "black",
                                "Sm2" = "#ff748c",
                                "Sm3" = "#E69F00",
                                "Sm4" = "#009E73"),
                     labels = c("Sm1" = "Open-access",
                                "Sm2" = "Restrictions",
                                "Sm3" = "Sustainable policy reform",
                                "Sm4" = "Technological advances")) +
  xlim(1, 3.67) +
  ylim(0.82, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20))

ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/mar_concept1.png", unfed1, width = 8, height = 10, units = "in", dpi = 300)




unfed2 <- ggplot(data = fig2_mar %>% filter(scenario %in% c("Sm1", "Sm2")), aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = mar_labs3 %>% filter(lab != "Technological advances"), aes(x = quantity, y = price, label = lab), 
  #                 size = 7, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "black",
                                "Sm2" = "#ff748c",
                                "Sm3" = "#E69F00",
                                "Sm4" = "#009E73"),
                     labels = c("Sm1" = "Open-access",
                                "Sm2" = "Restrictions",
                                "Sm3" = "Sustainable policy reform",
                                "Sm4" = "Technological advances")) +
  xlim(1, 3.67) +
  ylim(0.82, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20))

ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/mar_concept2.png", unfed2, width = 8, height = 10, units = "in", dpi = 300)



unfed3 <- ggplot(data = fig2_mar %>% filter(scenario != "Sm4"), aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = mar_labs3 %>% filter(lab != "Technological advances"), aes(x = quantity, y = price, label = lab), 
  #                 size = 7, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "black",
                                "Sm2" = "#ff748c",
                                "Sm3" = "#E69F00",
                                "Sm4" = "#009E73"),
                     labels = c("Sm1" = "Open-access",
                                "Sm2" = "Restrictions",
                                "Sm3" = "Sustainable policy reform",
                                "Sm4" = "Technological advances")) +
  xlim(1, 3.67) +
  ylim(0.82, 4.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20))

ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/mar_concept3.png", unfed3, width = 8, height = 10, units = "in", dpi = 300)


fed1 <- ggplot(data = fig2_mar, aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 1.5) +
  xlab("Quantity") +
  ylab("Price") +
  # geom_text_repel(data = mar_labs3 %>% filter(lab != "Technological advances"), aes(x = quantity, y = price, label = lab), 
  #                 size = 7, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "black",
                                "Sm2" = "#ff748c",
                                "Sm3" = "#E69F00",
                                "Sm4" = "#009E73"),
                     labels = c("Sm1" = "Open-access",
                                "Sm2" = "Restrictions",
                                "Sm3" = "Sustainable policy reform",
                                "Sm4" = "Technological advances")) +
  xlim(1, 5.12) +
  ylim(0.82, 4.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 20),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20))

ggsave("~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/mar_concept4.png", fed1, width = 8, height = 10, units = "in", dpi = 300)


## supply curves - wild
## -----------------------------------------------------------------

# ## scale capture fishery production
# cf_scale_val <- 0.78
# 
# ## scale for reduction fisheries
# red_scale_mult <- 1 - 0.18
# 
# ## functions
# calc_glob_h <- function(hval, inflation) {
#   global_h <- hval / inflation
#   
# }
# 
## Calculate the current (2012) average cost of wild fishery landings
upsides0 <- upsides %>%
  filter(Year == 2012)

wm_price0 <- weighted.mean(upsides0$Price, upsides0$Catch)

upsides0_adj <- upsides0 %>%
  mutate(Price = ifelse(SciName == "Engraulis ringens", 250, Price))

wm_price_adj <- weighted.mean(upsides0_adj$Price, upsides0_adj$Catch)

## ggplot supply curves
wc_supply$scenario <- factor(wc_supply$scenario, levels = c("F current", "Rational reform", "MSY"))

cons0_w <- round(51.2)
p0 <- round(wm_price_adj)

init_df <- tibble(price = p0,
                  total_ed = cons0_w,
                  scenario = "current")

init_df_sup <- tibble(price = seq(0, p0, 1),
                      total_ed = rep(cons0_w, length(seq(0, p0, 1))),
                      scenario = rep("current", length(seq(0, p0, 1))))

init_df_dem <- tibble(price = rep(p0, length(seq(0, cons0_w, 1))),
                      total_ed = (seq(0, cons0_w, 1)),
                      scenario = rep("current", length(seq(0, cons0_w, 1))))

wild_prod_fig <- ggplot(wc_supply, aes(x = total_ed / 1e6, y = price, group = scenario, color = scenario)) +
  geom_line(size = 2, alpha = 0.9) +
  geom_line(data = init_df_sup, aes(x = total_ed, y = price), lty = "dashed", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem, aes(x = total_ed, y = price), lty = "dashed", color = "#767676", size = 1.5) +
  geom_point(data = init_df, aes(x = total_ed, y = price), color = "#767676", size = 5) +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # geom_vline(xintercept = cons0_w, lty = "dashed", size = 1) +
  scale_color_manual(breaks = c("F current", "Rational reform", "MSY"),
                     values = c("F current" = "black",
                                "Rational reform" = "#5BA1E5",
                                "MSY" = "#000080")) +
  xlab("Edible production (mmt)") +
  ggtitle("Potential production") +
  scale_y_continuous(label=comma,
                     limits = c(0,2600),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 70)) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.3, 0.8),
        legend.text = element_text(size = 18),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5)) 


ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/wild_prod_fig.png", wild_prod_fig, width = 9, height = 10, units = "in", dpi = 300)



wild_current <- ggplot() +
  geom_line(size = 2, alpha = 0.9) +
  geom_line(data = init_df_sup, aes(x = total_ed, y = price), lty = "dashed", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem, aes(x = total_ed, y = price), lty = "dashed", color = "#767676", size = 1.5) +
  geom_point(data = init_df, aes(x = total_ed, y = price), color = "#767676", size = 5) +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # geom_vline(xintercept = cons0_w, lty = "dashed", size = 1) +
  scale_color_manual(breaks = c("F current", "Rational reform", "MSY"),
                     values = c("F current" = "black",
                                "Rational reform" = "#5BA1E5",
                                "MSY" = "#000080")) +
  xlab("Edible production (mmt)") +
  ggtitle("Current production") +
  scale_y_continuous(label=comma,
                     limits = c(0,2600),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 70)) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.3, 0.8),
        legend.text = element_text(size = 18),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5)) 

ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/wild_current_fig.png", wild_current, width = 9, height = 10, units = "in", dpi = 300)


## production potential -- fed
## ------------------------------------------------------

## current production
init_fed_prod <- mari_prod %>%
  filter(sector == "mariculture",
         indicator == "food",
         grp == "fed") %>%
  select(grp, grp_sum) %>%
  unique() %>%
  select(grp_sum) %>%
  as.numeric()

## current price
init_price <- 7000 ## current price for salmon

init_finprod <- tibble(scen_lab = "current",
                       price = init_price,
                       meat_yr = init_fed_prod)

init_fed_prod2 <- init_fed_prod / 1e6

init_df_ff <- tibble(price = init_price,
                     total_ed = init_fed_prod2,
                     scenario = "current")

init_df_sup_ff <- tibble(price = seq(0, init_price, 1),
                         total_ed = rep(init_fed_prod2, length(seq(0, init_price, 1))),
                         scen_lab = rep("current", length(seq(0, init_price, 1))))

init_df_dem_ff <- tibble(price = rep(init_price, length(seq(0, init_fed_prod2, 1))),
                         total_ed = (seq(0, init_fed_prod2, 1)),
                         scen_lab = rep("current", length(seq(0, init_fed_prod2, 1))))



fed_prod <- ggplot(data_np %>% filter(sector == "Finfish mariculture",
                                        scenario != "Scenario 4"), aes(x = meat_yr / 1e6, y = price, group = scen_lab, color = scen_lab)) +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_finprod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "#767676", inherit.aes = F) +
  geom_line(data = init_df_sup_ff, aes(x = total_ed, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem_ff, aes(x = total_ed, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  ylab("Price (USD per mt)") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation (Ambitious)" = "#004F39")) +
  ggtitle("Potential production") +
  scale_y_continuous(label=comma,
                     limits = c(3000,7500),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 210)) +
  ylab("Price (USD per mt)") +
  xlab("Edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.8),
        # legend.position = "none",
        legend.text = element_text(size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/fed_prod_potential.png", fed_prod, width = 9, height = 10, units = "in", dpi = 300)


fed_current <- ggplot() +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_finprod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "#767676", inherit.aes = F) +
  geom_line(data = init_df_sup_ff, aes(x = total_ed, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem_ff, aes(x = total_ed, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  ylab("Price (USD per mt)") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation (Ambitious)" = "#004F39")) +
  ggtitle("Potential production") +
  scale_y_continuous(label=comma,
                     limits = c(3000,7500),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 210)) +
  ylab("Price (USD per mt)") +
  xlab("Current production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.8),
        # legend.position = "none",
        legend.text = element_text(size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/fed_current.png", fed_current, width = 9, height = 10, units = "in", dpi = 300)

## supply curves -- mariculture, unfed
## -------------------------------------------------

## current production
init_unfed_prod <- mari_prod %>%
  filter(sector == "mariculture",
         sub_cat == "molluscs",
         indicator == "food") %>%
  select(sub_cat, val) %>%
  unique() %>%
  select(val) %>%
  as.numeric()

## current price
init_price_uf <- 1700 ## current price for mussels

init_biv_prod <- tibble(scen_lab = "current",
                        price = init_price_uf,
                        meat_yr = init_unfed_prod)

init_df_sup_b <- tibble(price = seq(0, init_price_uf, 1),
                        total_ed = rep(init_unfed_prod, length(seq(0, init_price_uf, 1))),
                        scen_lab = rep("current", length(seq(0, init_price_uf, 1))))

init_df_dem_b <- tibble(price = rep(init_price_uf, length(seq(0, init_unfed_prod, 1))),
                        total_ed = (seq(0, init_unfed_prod, 1)),
                        scen_lab = rep("current", length(seq(0, init_unfed_prod, 1))))


## fed production, three scenarios with point

unfed_potential <- ggplot(data_np %>% filter(sector == "Bivalve mariculture",
                                          scenario == "Scenario 2"), aes(x = meat_yr / 1e6, y = price, group = scen_lab, color = scen_lab)) +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_biv_prod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "#767676", inherit.aes = F) +
  geom_line(data = init_df_sup_b, aes(x = total_ed / 1e6, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem_b, aes(x = total_ed / 1e6, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  ylab("Price (USD per mt)") +
  xlab("Edible production (mmt)") +
  ggtitle("Potential production") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation (Ambitious)" = "#004F39")) +
  scale_y_continuous(label=comma,
                     limits = c(500,2700),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 110)) +
  # ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.8),
        # legend.position = "none",
        legend.text = element_text(size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/unfed_potential.png", unfed_potential, width = 9, height = 10, units = "in", dpi = 300)


unfed_current <- ggplot() +
  geom_path(size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 0.1, lty = 1, color = "black") +
  geom_point(data = init_biv_prod, aes(x = meat_yr / 1e6, y = price), size = 5, color = "#767676", inherit.aes = F) +
  geom_line(data = init_df_sup_b, aes(x = total_ed / 1e6, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  geom_line(data = init_df_dem_b, aes(x = total_ed / 1e6, y = price), lty = "dotted", color = "#767676", size = 1.5) +
  ylab("Price (USD per mt)") +
  xlab("Edible production (mmt)") +
  ggtitle("Current production") +
  # geom_hline(yintercept = wm_price_adj, lty = "dashed", size = 1) +
  # annotate("text", x = 20, y = wm_price_adj + 200, label = paste0("2012 weighted average global price = $", format(round(wm_price_adj), nsmall=0, big.mark=","), " / MT"), size = 5) +
  scale_color_manual(values = c("Policy reforms" = "#E69F00",
                                "Tech innovation" = "#009E73",
                                "Tech innovation (Ambitious)" = "#004F39")) +
  scale_y_continuous(label=comma,
                     limits = c(500,2700),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 110)) +
  # ylab("Price (USD per mt)") +
  # xlab("Total edible production (mmt)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.8),
        # legend.position = "none",
        legend.text = element_text(size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1.5))

ggsave(filename =  "~/Box/SFG Centralized Resources/Projects/Blue paper/presentation/unfed_current.png", unfed_current, width = 9, height = 10, units = "in", dpi = 300)
