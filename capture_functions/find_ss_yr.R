## Find year in which stock reaches steady-state

find_ss_yr <- function(scenario, ss_df, output_df, tol_val, int_val) {
  
  ## define the stock and the scenario
  stock <- scenario$stockid
  policy <- scenario$f_policy
  
  # print(stock)
  # print(policy)
  
  ## filter outputs
  stock_outputs <- output_df %>%
    filter(id_orig == stock,
           f_policy == policy)
  
  stock_ss <- ss_df %>%
    filter(id_orig == stock,
           f_policy == policy)
  
  ## define tolerance
  tol <- tol_val
  ststb <- stock_ss$ss_b
  ststh <- stock_ss$ss_h
  int <- int_val

  hvec <- stock_outputs$harvest
  fvec <- stock_outputs$FvFMSY
  bvec <- stock_outputs$BvBMSY
  
  projection_length <- nrow(stock_outputs)
  
  for(tt in int:projection_length) {
    
      tstart <- tt - (int - 1)
      # h0 <- hvec[tt]
      # hnext <- hvec[tt + 1]
      hvec_sub <- hvec[tstart:tt]
      hvec_diff <- abs(hvec_sub - ststh)
      
      if(all(hvec_diff < tol)) {
        
        tval <- tstart
        bval <- bvec[tval]
        hval <- hvec[tval]
        fval <- fvec[tval]
        
        break
      
    } else if(tt == projection_length) {
      
      tval <- projection_length
      bval <- bvec[projection_length]
      hval <- hvec[projection_length]
      fval <- fvec[projection_length]
      
      
      
      } else {  
      
      tval <- tt
      # h0 <- hvec[tt]
      
     }
    }

  
  
  ss_df_tmp <- data.frame(id_orig = stock, f_policy = policy, FvFMSY = fval, time_ss = tval, hbreak = hval,
                          h_ss = stock_ss$ss_h , bbreak = bval, b_ss = ststb, tol = tol)
  ststs_df <- taRifx::remove.factors(ss_df_tmp)
  
  return(ststs_df)
  
}