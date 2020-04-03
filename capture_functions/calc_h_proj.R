## harvest model

calc_h_proj <- function(f, b, MSY, biomass, BMSY_val, phi, g)
{
  
  b_growth <- b + ((phi + 1) / phi ) * g * b * (1 -  (b ^ phi)  / (phi + 1))
  
  harvest <- f * b * MSY
  
  hval <- min(b_growth * BMSY_val, harvest)

  
}
