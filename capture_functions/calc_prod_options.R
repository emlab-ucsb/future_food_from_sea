## calculate production at a given price value and save all outputs

calc_prod_options <- function(price_val, input_df){
  
  ## profit df
  pi_df <- input_df %>%
    select(id_orig:harvest, extract_cost, MSY, mcost_mt) %>%
    mutate(price = price_val,
           profit = (harvest * price) - (mcost_mt * harvest) - extract_cost)
  
  ## spread the df, take the maximum profit value between the two policies
  mgmt_df <- pi_df %>%
    select(id_orig, country, price, f_policy, profit) %>%
    spread(f_policy, profit) %>%
    mutate(choice_pi = pmax(FMSY, F_current),
           choice_policy = ifelse(choice_pi == FMSY, "FMSY", "F_current"))
  
  # ## create df with only rows from the policy that you would choose for that price
  # mgmt_df2 <- mgmt_df %>%
  #   select(id_orig, country, price, f_policy = choice_policy) %>%
  #   left_join(pi_df)

}