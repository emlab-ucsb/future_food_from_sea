## growth model

calc_b_growth <- function(b, phi)
{
  b_growth <- ((phi + 1) / phi ) * g * b * (1 -  (b ^ phi)  / (phi + 1))
  
}