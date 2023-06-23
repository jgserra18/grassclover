#' fraction_clover
#'
#' @param grass_yield_dm grass DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#' @param grass_yield_dm_max grass max value of yield_dm when N supply is not a limiting factor (Yg,max; kg ha-1 yr-1)
#' @param f_displacement #Displacement rate of clover by grass (0-1) (epsilon)
#' @description
#' computes portion of clover occupation
#' @unit %
#' @return
#' @export
fraction_clover = function(grass_yield_dm,
                           grass_yield_dm_max,
                           over_yielding_factor=0.5) {

  f_clover = 1 - (1-over_yielding_factor) * grass_yield_dm / grass_yield_dm_max - over_yielding_factor * (grass_yield_dm/grass_yield_dm_max)^2
  return(f_clover)
}


#' clover_DM_yield
#'
#' @param yield_clover_dm_max maximum clover yield in monoculture (Yc ;kg ha-1 yr-1)
#' @param f_clover  portion of clover occupation  (0-1) (pc)
#' @description
#' computes clover yield based on maximum possible yield and fraction of clover occupation
#' @unit kg DM ha-1 yr-1
#' @return vetor
#' @export
#'
#' @examples clover_DM_yield(6000, 0.3)
clover_DM_yield = function(yield_clover_dm_max=6000,
                           f_clover) {

  DM_yield = yield_clover_dm_max * f_clover
  return(DM_yield)
}


#' clover_N_yield
#'
#' @param yield_clover_max maximum clover yield in monoculture (Yc ;kg ha-1 yr-1)
#' @param f_clover  portion of clover occupation  (0-1) (pc)
#' @param f_n the concentration of N in the harvested material (0-1) (nc; kg kg DM-1 yr-1)
#' @description
#' computes clover N yield (accounts for portion of clover in land)
#' @unit kg N ha-1 yr-1
#' @return clover_N_yield_base(6000, 0.3, 0.04)
#' @export
clover_N_yield_base = function(yield_clover_dm_max=6000,
                               f_clover,
                               f_n_clover = 0.04) {

  N_yield = yield_clover_dm_max*f_clover*f_n_clover
  return(N_yield)
}


#' clover_N_total
#'
#' @param N_yield clover N yield (;kg N ha-1 yr-1)
#' @param f_ab_clover N yield/totl crop N for clover (%)
#' @description
#' computes total N of clover
#' @unit kg N ha-1 yr-1
#' @return
#' @export
#'
#' @examples
clover_N_total = function(N_yield,
                          f_ab_clover) {


  N_clover = N_yield/f_ab_clover
  return(N_clover)
}

#' clover_N_yield_soil_aboveground
#'
#' @param N_yield clover N yield (;kg N ha-1 yr-1)
#' @param f_Navail  Proportion of available N  +  fixed N recovered in clover yield (alpha_c) (0-1)
#' @description
#' computes N in aboveground clover returned to soil
#' @unit kg N ha-1 yr-1
#' @return
#' @export
clover_N_yield_soil_aboveground = function(N_yield,
                                           f_Navail=0.5) {

  N_yield_ab = N_yield/f_Navail
  return(N_yield_ab)
}

