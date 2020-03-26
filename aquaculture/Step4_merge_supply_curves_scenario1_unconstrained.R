
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

# Catch scalars
catch_scalars <- c(0.75, 1, 1.5, 2)


# Loop through catch scalars
#####################################################################################

# Loop
i <- j <- 1
for(i in 1:length(species)){
  for(j in 1:length(catch_scalars)){
    
    # Params
    spp <- species[i]
    scalar <- catch_scalars[j]
    scalar_format <- format(scalar, nsmall=2)
    print(paste(spp, scalar))
    
    # Read AQ bivalve data
    infile <- paste0("AQ_bivalve_w_eez_", scalar_format, "catch_scalar.Rdata")
    load(file.path(datadir, infile))
    
    # Read AQ finfish data
    infile <- paste0("AQ_finfish_species", spp, "_scenario1_unconstrained_w_eez_", scalar_format, "catch_scalar.Rdata")
    load(file.path(datadir, infile))
    
    # Merge curves
    #####################################################################################
    
    # Edible meat conversions
    tl2meat_biv <- 1/6
    tl2meat_fin <- 1/1.15
    
    # Format AQ-bivalve curve
    baq_scurve <- b_scurve %>% 
      rename(mt_yr=tot_mt_yr) %>% 
      mutate(sector="Bivalve aquaculture") %>% 
      select(sector, price, mt_yr) %>% 
      mutate(meat_yr=mt_yr*tl2meat_biv)
    
    # Format AQ-finfish curve
    faq_scurve <- f_scurve %>% 
      rename(mt_yr=tot_mt_yr) %>% 
      mutate(sector="Finfish aquaculture") %>% 
      select(sector, price, mt_yr) %>% 
      mutate(meat_yr=mt_yr*tl2meat_fin)
    
    # Merge AQ data
    aq_scurves <- rbind(baq_scurve, faq_scurve) %>% 
      mutate(aq_spp=spp,
             aq_scen="Scenario 1 - unconstrained",
             aq_cost_scalar=scalar) %>% 
      select(aq_spp, aq_scen, aq_cost_scalar, everything()) %>% 
      ungroup()
    
    # Format WC curve
    wc_scurve1 <- wc_scurve %>% 
      rename(cap_scen=scenario, mt_yr=wc_mt_yr, meat_yr=wc_meat_yr_scen4) %>% 
      mutate(sector="Capture fisheries",
             aq_spp=spp,
             aq_scen="Scenario 1 - unconstrained",
             aq_cost_scalar=scalar) %>% 
      dplyr::select(cap_scen, aq_spp, aq_scen, aq_cost_scalar, sector, price, mt_yr, meat_yr) %>% 
      ungroup()
    
    # Expand AQ curves
    cap_scens <- unique(wc_scurve1$cap_scen)
    aq_scurves1 <- purrr::map_df(cap_scens, function(x) {
      aq_scurves_cap <- aq_scurves %>% 
        mutate(cap_scen=x) %>% 
        select(cap_scen, everything())
    })
    
    # Merge capture and aquaculture
    data <- rbind(wc_scurve1, aq_scurves1) %>% 
      mutate(scenario=paste(cap_scen, "spp", aq_spp, aq_cost_scalar, aq_scen)) %>% 
      select(scenario, everything())
    
    # Export
    outfile <- paste0("WC_AQ_supply_curve_merged_spp", spp, "_scenario1_unconstrained_", scalar, "scalar.Rds")
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
      group_by(cap_scen, price) %>% 
      summarize(mt_yr=sum(mt_yr, na.rm=T),
                meat_yr=sum(meat_yr, na.rm=T))
    
    # Plot overall
    g <- ggplot() +
      geom_bar(data=data, aes(y=meat_yr/1e9, x=price, fill=sector), stat="identity") +
      facet_wrap(~cap_scen, ncol=2) +
      coord_flip() +
      xlab("Price (USD / mt)") + ylab("Edible production (billions of mt)") +
      geom_line(data=t_scurve, aes(y=meat_yr/1e9, x=price), size=0.2) +
      geom_hline(yintercept=470*1e6/1e9, linetype="dotted", size=0.2) +
      annotate("text", y=700*1e6/1e9, x=0, label="470 million mt per year\nof animal protein required in 2050", hjust=0, size=1.5) +
      ggtitle("Scenario 1: Finfish AQ unconstrained\nby feed from WC fisheries") +
      scale_fill_manual(name="Protein source", values=alpha(c("blue", "darkgreen", "red"), 0.8)) +
      theme_bw() + my_theme
    #g
    
    # Save
    outfig <- paste0("WC_AQ_supply_curve_merged_spp", spp, "_scenario1_unconstrained_", scalar, "scalar.png")
    ggsave(file=file.path(plotdir, outfig), width=5, height=3, units="in", dpi=600)
    
  }
}


  