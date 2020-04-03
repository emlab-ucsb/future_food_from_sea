## use find_ss_yr (updated version)

# ## find steady-state year
# 
# 
# find_ss_year <- function(scenario, output_df, tol_val) {
#   
#   ## define the stock and the scenario
#   stock <- scenario$stockid
#   policy <- scenario$f_policy
#   
#   ## filter outputs
#   stock_outputs <- output_df %>%
#     filter(id_orig == stock,
#            f_policy == policy)
#   
#   ## define tolerance
#   tol <- tol_val
#   
#   hvec <- stock_outputs$harvest
#   fvec <- stock_outputs$FvFMSY
#   
#   projection_length <- length(hvec)
#   
#   for(tt in 1:projection_length) {
#     
#     if(tt != projection_length) {
#       
#       tval <- tt
#       h0 <- hvec[tt]
#       hnext <- hvec[tt + 1]
#       diff <- hnext - h0
#       diff_abs <- abs(diff)
#       fval <- fvec[tt]
#       
#       if(diff_abs < tol) break
#       
#     } else {  
#       
#       tval <- tt
#       h0 <- hvec[tt]
#       
#     }
#   }
#   
#   
#   ss_df_tmp <- data.frame(id_orig = stock, f_policy = policy, FvFMSY = fval, time_ss = tval, h_ss = h0, tol = tol)
#   ss_df <- taRifx::remove.factors(ss_df_tmp)
#   
#   return(ss_df)
#   
# }
