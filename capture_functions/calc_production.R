## calculate production at a given price value

calc_production <- function(price_val, input_df){
  
  ## profit df
  pi_df <- input_df %>%
    # select(id_orig:harvest, extract_cost, MSY, mcost_mt) %>%
    mutate(price = price_val,
           extract_cost = calc_extract_cost(g = g, f = FvFmsy, c = c, beta = beta),
           profit = (ss_h * price) - (mcost_mt * ss_h) - extract_cost)
  
  ## spread the df, take the maximum profit value between the two policies
  mgmt_df <- pi_df %>%
    select(id_orig, country, scenario, price, f_policy, profit) %>%
    spread(f_policy, profit) %>%
    mutate(choice_pi = pmax(FMSY, F_current),
           choice_policy = ifelse(choice_pi == FMSY, "FMSY", "F_current"))

  ## create df with only rows from the policy that you would choose for that price
  mgmt_df2 <- mgmt_df %>%
    select(id_orig, country, scenario, price, f_policy = choice_policy) %>%
    left_join(pi_df) %>%
    mutate(adj_harvest = ifelse(profit < 0, 0, ss_h),
           adj_profit = ifelse(adj_harvest == 0, 0, profit))
  
}