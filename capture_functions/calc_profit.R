## profit model

calc_profit = function(g, phi, p, f, b, c, beta, MSY)
{
  revenue = p * f * b * MSY
  cost = c * (g * f) ^ beta
  pi =  revenue - cost
  return(pi)}