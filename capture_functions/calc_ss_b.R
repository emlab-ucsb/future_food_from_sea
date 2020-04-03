## calculate biological steady state for a given f

## calculate Bvalue from board
calc_ss_b <- function(phi_val, fval) {
  
  b_val <- (phi_val + 1 - phi_val * fval) ^ (1 / phi_val)
  
}
