
datadir <- "aquaculture/aquaculture_v2/output"

data <- read.csv(file=file.path(datadir, "ocean_supply_curve_scenarios1-4_feed1.csv"))

scen <- data %>% 
  filter(scenario=="Scenario 3c" & sector=='Finfish aquaculture')

g <- ggplot(scen, aes(x=mt_yr/1e6, y=price)) +
  geom_line() +
  theme_bw()
g

min(scen$mt_yr/1e6)
