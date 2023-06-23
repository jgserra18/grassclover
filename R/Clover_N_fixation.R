#' clover_harvested_N_fixation
#'
#' @param clover_N_yield N in aboveground clover returned to soil (kg N ha-1 yr-1)
#' @param N_min_clover unclear what this is; soil N to clover? (kg N ha-1 yr-1)
#' @param f_Navail  Proportion of available N  +  fixed N recovered in clover yield (alpha_c) (0-1)
#' @description
#' Computes N fixed by clover
#' @unit kg N ha-1 yr-1
#' @return
#' @export
clover_harvested_N_fixation = function(clover_N_yield,
                                       N_min_clover,
                                       f_Navail=0.5) {

  harv_Nfix = clover_N_yield - N_min_clover * f_Navail
  return(harv_Nfix)
}



#' clover_Ndfa
#'
#' @param clover_N crop N of clover (kg N ha-1 yr-1)
#' @param total_N_fixation total N fixed by clover (kg N ha-1 yr-1); NOT clover_harvested_N_fixation()
#' @description
#' Computes N2 fixed per aboveground N biomass
#' @unit %
#' @return
#' @export
clover_Ndfa = function(clover_N,
                       total_N_fixation) {

  Ndfa = 100 * total_N_fixation / clover_N
  return(Ndfa)
}
