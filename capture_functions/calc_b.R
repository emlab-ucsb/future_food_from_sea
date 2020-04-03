## biological model

calc_b <- function(b, phi, g, f)
{
  b_next <- b + ((phi + 1) / phi ) * g * b * (1 -  (b ^ phi)  / (phi + 1)) - g * f * b
  bmax <- (phi + 1) ^ (1 / phi) - 0.1
  return(max(0, min(bmax, b_next)))
}