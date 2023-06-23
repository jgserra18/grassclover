require('grassclover')

#' N_yield
#'
#'  @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#'  @param f_n the concentration of N in the harvested material (0-1) (ngs; kg kg DM-1 yr-1)
#' @description
#' computes N uptake in harvested material
#' @unit kg N ha-1 yr-1
#' @return
#' @export

N_yield = function(yield_dm,
                   f_n) {

  N_yield = yield_dm * f_n
  return(N_yield)
}


#' N_conc_harvested
#'
#'  @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#'  @param yield_dm_max max value of yield_dm when N supply is not a limiting factor (Yg,max; kg ha-1 yr-1)
#'  @param f_n_min min value of the N concentration in harvested material where is not harvestabled growth (0-1) (ngs,min)
#'  @param f_n_max max value of the n concentration in harvested material where N is saturated (0-1) (ngs,max)
#'  @description
#'  computes the N concentration in harvested material, somewhere around f_n_min and f_n_max
#'  @unit kg N kg DM-1
#' @return
#' @export
N_conc_harvested = function(yield_dm,
                            yield_dm_max,
                            f_n_min,
                            f_n_max) {

  f_n = ((f_n_max-f_n_min)/yield_dm_max)*yield_dm + f_n_min
  return(f_n)
}


#' N_conc_harvested_fitted
#'
#'  @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#'  @param yield_dm_max max value of yield_dm when N supply is not a limiting factor (Yg,max; kg ha-1 yr-1)
#'  @param f_n_min min value of the N concentration in harvested material where is not harvestabled growth (0-1) (ngs,min)
#' @param lambda quadratic parameter
#' @description
#' computes N concentrastion in harvested material using a quadratic function fitted to the relative yield (yield dm / yield dm max)
#' @unit kg ha-1 yr-1
#' @return
#' @export
#'
#' @examples
N_conc_harvested_fitted = function(yield_dm,
                                   yield_dm_max,
                                   f_n_min = 0.01903, # kg N kg DM-1
                                   lambda = 0.01138) { # kg N kg DM-1

  f_n = lambda * (yield_dm / yield_dm_max)^2 + f_n_min
  return(f_n)
}


#' N_yield_combined
#'
#'  @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#'  @param yield_dm_max max value of yield_dm when N supply is not a limiting factor (Yg,max; kg ha-1 yr-1)
#'  @param f_n_min min value of the N concentration in harvested material where is not harvestabled growth (0-1) (ngs,min)
#'  @param f_n_max max value of the n concentration in harvested material where N is saturated (0-1) (ngs,max)
#'  @description
#'  combines N_yield() and N_conc_harvested()
#'  @unit kg N ha-1 yr-1
#' @return
#' @export
N_yield_combined = function(yield_dm,
                            yield_dm_max,
                            f_n_min,
                            f_n_max) {

  N_yield = ((f_n_max-f_n_min)/yield_dm_max)*yield_dm^2 + f_n_min*yield_dm
  return(N_yield)
}

#' N_yield_soil_aboveground
#'
#' @param N_yield N yield of harvested material (Ngs; kg N ha-1 yr-1)
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @description
#' Computes N entering the soil aboveground; excludes fraction of aboveground production harvested
#' @note
#' f_harvested is defaulted to 80% (mechanical harvesting; this would be lower for grazing)
#' @unit kg N ha-1 yr-1
#' @return
#' @export
N_yield_soil_aboveground = function(N_yield,
                                    f_harvested=0.8) {

  N_yield_ab = N_yield*(1/f_harvested-1)
  return(N_yield_ab)
}

#' total_plant_N_uptake
#'
#' @param N_residues_t N residues input at year t (kg N ha-1 yr-1)
#' @param N_yield N in harvestable yield (kg N ha-1 yr-1)
#' @description
#' computes total N plant uptake based on the residues and harvested N yield
#' @unit kg N ha-1 yr-1
#' @return
#' @export
grass_N_uptake = function(f_shoot,
                          N_yield) {

  #' @param f_shoot fraction of total DM production that is aboveground (??gDM; shoot fraction) (0-1)
  #' @param N_yield N in harvestable yield (kg N ha-1 yr-1)

  tot_N_up = N_yield/f_shoot
  return(tot_N_up)
}
