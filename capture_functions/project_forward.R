## forward projection

project_forward <- function(scenario, inputs, projection_length, delta) {
  
  ## define the stock id for projection
  stock <- scenario$stockid
  
  ## create empty dataframe
  output_df <- data.frame()
  
  ## filter for stock input data
  stock_inputs <- inputs %>%
    filter(id_orig == stock)
  
  ## define parameters
  f_val <- ifelse(scenario$f_policy == "FMSY", 1, stock_inputs$FvFmsy)
  b0 <- stock_inputs$BvBmsy
  phi <- stock_inputs$phi
  g <- stock_inputs$g
  MSY <- stock_inputs$msy
  p <- stock_inputs$price
  c <- stock_inputs$c
  BMSY <- stock_inputs$biomass / b0
  
  ## empty vectors for storing
  bvec <-  vector() ## biomass (B/BMSY)
  biovec <- vector() ## biomass (MT)
  bvec[1] <- b0 ## set starting biomass
  biovec[1] <- stock_inputs$biomass
  fvec <- vector()
  hvec <- vector() ## harvest
  rvec <- vector() ## revenue
  cvec <- vector() ## extraction cost
  pvec <- vector() ## profit
  dpvec <- vector() ## discounted profit
  
  for(tt in 1:projection_length) {
    
    f_val_tt <- ifelse(tt == 1 & scenario$f_policy == "FMSY", stock_inputs$FvFmsy, f_val)
    
    fvec[tt] <- f_val_tt
    hvec[tt] <- calc_h_proj(f = f_val_tt, b = bvec[tt], MSY = MSY, biomass = biovec[tt], BMSY_val = BMSY, phi = phi, g = g)
    bvec[tt + 1] <- calc_b(b = bvec[tt], g = g, phi = phi, f = f_val_tt)
    biovec[tt + 1] <- bvec[tt + 1] * BMSY
    pvec[tt] <- calc_profit(g = g, phi = phi, p = p, f = f_val_tt, b = bvec[tt], c = c, beta = beta, MSY = MSY)
    rvec[tt] <- calc_rev(p = p, f = f_val_tt, b = bvec[tt], MSY)
    cvec[tt] <- calc_extract_cost(g = g, f = f_val_tt, c = c, beta = beta)
    dpvec[tt] <- (delta ^ (tt)) * pvec[tt]
  }
  
  # print(bvec)
  # print(rvec)
  
  
  output_df_tmp <- data.frame(id_orig = rep(stock, projection_length), 
                              time = 1:projection_length, 
                              f_policy = rep(scenario$f_policy, projection_length),
                              FvFMSY = fvec, 
                              BvBMSY = bvec[1:projection_length],
                              biomass = biovec[1:projection_length],
                              harvest = hvec, 
                              revenue = rvec,
                              extract_cost = cvec, 
                              profit = pvec, 
                              disc_profit = dpvec, 
                              MSY = rep(MSY, projection_length),
                              g = rep(g, projection_length))
  
  output_df <- taRifx::remove.factors(output_df_tmp)
  
  # print(output_df_tmp)
  
  return(output_df)
  
}