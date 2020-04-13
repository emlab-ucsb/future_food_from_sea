## March 7, 2020
## possible scenario combinations

capture_vec <- c("main_text", "tech_improv", "inc_cost", "inc_cost_tech_improv")
mari_sp <- c("sp1", "sp2", "sp3")
mari_cost <- c("25l", "50h", "100h")
policy_vec <- c("current", "improv_tech", "agg_tech", "decouple")
sc_vec <- c("agg", "separate")
# feed_pric <- c(2, 3, 5)
demand_vec <- c("current", "future", "extreme")

test <- expand_grid(capture_scen = capture_vec,
                    mari_sp_scen = mari_sp,
                    mari_scen = mari_cost,
                    policy_scen = policy_vec,
                    sc_scen = sc_vec,
                    feed_scen = feed_pric,
                    demand_scen = demand_vec)