## calculate extraction cost as a function of fishing effort

calc_extract_cost = function(g, f, c, beta)
{
  extract_cost = c * (g * f) ^ beta
}