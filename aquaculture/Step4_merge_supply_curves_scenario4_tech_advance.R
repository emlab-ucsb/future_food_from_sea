
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)

# Directories
datadir <- "aquaculture/aquaculture_v3/output"
plotdir <- "aquaculture/aquaculture_v3/figures"

# Read data
wc_scurve <- readRDS(file.path(datadir, "wc_supply_curve.Rds"))

# Species
species <- 1:3

# Capture scenarios
cap_scens <- unique(wc_scurve$scenario)

# Tech scenarios
tech_scens <- c("a", "b", "c")

# Catch scalars
catch_scalars <- c(0.75, 1, 1.5, 2)


# Loop through feed price scenarios
#####################################################################################

# Loop
i <- j <- k <- l <- 1
for(i in 1:length(species)){
  for(j in 1:length(cap_scens)){
    for(k in 1:length(tech_scens)){
      for(l in 1:length(catch_scalars)){
        
        # Params
        spp <- species[i]
        cap_scen <- cap_scens[j]
        tech_scen <- tech_scens[k]
        scalar <- catch_scalars[l]
        scalar_format <- format(scalar, nsmall=2)
        print(paste(spp, cap_scen, tech_scen, scalar))
        aq_scen_label <- paste0("Scenario 4", tech_scen, " - tech advance")
        
        # Read AQ bivalve data
        infile <- paste0("AQ_bivalve_w_eez_", scalar_format, "catch_scalar.Rdata")
        load(file.path(datadir, infile))
        
        # Read AQ finfish data
        infile <- paste0("AQ_finfish_species", spp, "_scenario4_tech_advance_", tech_scen, "_", cap_scen, "_", scalar_format, "catch_scalar.Rdata")
        load(file.path(datadir, infile))
    
        # Merge curves
        #####################################################################################
        
        # Percent reductions
        perc2fo_by <- 0.011
        perc2fm_by <- 0.069
        perc2reduction <- 0.18
        perc2byprod <- perc2fo_by + perc2fm_by
        tl2meat_biv <- 1/6
        tl2meat_fin <- 1/1.15
        
        # Format WC curve
        wc_scurve1 <- wc_scurve %>% 
          filter(scenario==cap_scen) %>% 
          rename(cap_scen=scenario, mt_yr_full=wc_mt_yr, meat_yr=wc_meat_yr_scen3) %>% 
          mutate(sector="Capture fisheries",
                 mt_yr=mt_yr_full*(1-perc2reduction)*(1-perc2byprod),
                 aq_spp=spp, 
                 aq_cost_scalar=scalar,
                 aq_scen=aq_scen_label) %>% 
          select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sector, price, mt_yr, meat_yr) %>% 
          ungroup()
        
        # Format AQ-bivalve curve
        baq_scurve <- b_scurve %>% 
          rename(mt_yr=tot_mt_yr) %>% 
          mutate(sector="Bivalve aquaculture",
                 cap_scen=cap_scen,
                 aq_spp=spp, 
                 aq_cost_scalar=scalar,
                 aq_scen=aq_scen_label) %>% 
          select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sector, price, mt_yr) %>% 
          mutate(meat_yr=mt_yr*tl2meat_biv) %>% 
          ungroup()
        
        # Format AQ-finfish curve
        faq_scurve <- f_scurve %>% 
          rename(mt_yr=tot_mt_yr) %>% 
          mutate(sector="Finfish aquaculture",
                 cap_scen=cap_scen,
                 aq_spp=spp, 
                 aq_cost_scalar=scalar,
                 aq_scen=aq_scen_label) %>% 
          select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sector, price, mt_yr) %>% 
          mutate(meat_yr=mt_yr*tl2meat_fin) %>% 
          ungroup()
        
        # Merge data
        data <- rbind(wc_scurve1, baq_scurve, faq_scurve) %>% 
          mutate(scenario=paste(cap_scen, "spp", aq_spp, aq_cost_scalar, aq_scen)) %>% 
          select(scenario, everything())
        
        unique(data$scenario)
        
        # Export
        outfile <- paste0("WC_AQ_supply_curve_merged_spp", spp, "_scenario4_tech_advance_", tech_scen, "_", cap_scen, "_", scalar, "scalar.Rds")
        saveRDS(data, file=file.path(datadir, outfile))
        
        
        # Plot overall curve
        #####################################################################################
        
        # Setup theme
        my_theme <- theme(axis.text=element_text(size=7),
                          axis.title=element_text(size=9),
                          plot.title=element_text(size=11),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"))
        
        # Calculate total
        t_scurve <- data %>%
          group_by(price) %>% 
          summarize(mt_yr=sum(mt_yr, na.rm=T),
                    meat_yr=sum(meat_yr, na.rm=T))
        
        # Plot overall
        g <- ggplot() +
          geom_bar(data=data, aes(y=meat_yr/1e6, x=price, fill=sector), stat="identity") +
          coord_flip() +
          xlab("Price (USD / mt)") + ylab("Edible production (millions of mt)") +
          geom_line(data=t_scurve, aes(y=meat_yr/1e6, x=price), size=0.2) +
          geom_hline(yintercept=470, linetype="dotted", size=0.2) +
          annotate("text", y=475, x=0, label="470 million mt per year\nof animal protein required in 2050", hjust=0, size=1.5) +
          ggtitle("Scenario 4: 50% reduction in FM/FO feed demands\ndue to technological advances") +
          scale_fill_manual(name="Protein source", values=alpha(c("blue", "darkgreen", "red"), 0.8)) +
          theme_bw() + my_theme
        g
        
        # Save
        outfig <- paste0("WC_AQ_supply_curve_merged_spp", spp, "_scenario4_tech_advance_", tech_scen, "_", cap_scen, "_", scalar, "scalar.png")
        ggsave(file=file.path(plotdir, outfig), width=5, height=3, units="in", dpi=600)
        
      }
    }
  }
}


  