#' @returns list with different parameters
#' @export
set_parameters = list(
  f_n_min = 0.016,  # Minimum concentration of N in the harvested DM
  f_n_max = 0.03, # Maximum concentration of N in the harvested DM
  ef_ammonia = 0.02, # Proportion N in fertiliser lost via ammonia volatilisation (for ammonium nitrate)
  ef_loss = 0.2, # Proportion of Nback and Nresid,g,y-1 lost via nitrate leaching and denitrification; needs uncertainty
  f_Navail=0.5, # Proportion of available N  +  fixed N recovered in clover yield (alpha_c) (0-1)
  over_yielding_factor=0.5, # Displacement rate of clover by grass (0-1) (epsilon)
  yield_clover_max = 6000, # maximum clover yield in monoculture (kg DM ha-1 yr-1)
  f_n_clover = 0.04, # Concentration of N in clover dry matter (kg N kg DM-1)
  f_harvested = 0.8
)
