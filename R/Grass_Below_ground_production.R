
#' total_DM_production
#'
#' @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @param yield_below DM yield of new root material and exudates (Ygr; kg ha-1 yr-1)
#' @description
#' computes total DM production, ie, sum of aboveground and below ground production (Ygtot)
#' @unit kg ha-1 yr-1
#' @return
#' @export
total_DM_production = function(yield_dm,
                               f_harvested=0.8,
                               yield_below) {

  tot_DM = yield_dm/f_harvested + yield_below
  return(tot_DM)
}

#' shoot_fraction
#'
#' @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#' @param total_yield_dm total yield (sum of above- and below ground material); see total_DM_production()
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @description
#' cmputes fraction of the total DM production that is aboveground (??gDM; shoot fraction)
#' @unit % (0-1)
#' @return
#' @export
shoot_fraction = function(yield_dm,
                          total_yield_dm,
                          f_harvested=0.8) {

  #' @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
  #' @param total_yield_dm total yield (sum of above- and below ground material); see total_DM_production()
  #' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
  #' @description
    #' cmputes fraction of the total DM production that is aboveground (??gDM; shoot fraction)
    #' @unit % (0-1)

  f_ab = yield_dm/(f_harvested*total_yield_dm)
  return(f_ab)
}

#' shoot_fraction_fitted
#'
#' @param k_agdm coefficient
#'  @param f_n the concentration of N in the harvested material (0-1) (ngs; kg kg DM-1 yr-1)
#'  @param f_shoot_dm_min shoot fraction when N concentration is extrapolated to 0
#' @description
#' computes fitted shoot fraction using data from (Schenk et al., 1996) and (Gorissen and Cotrufo, 1999)
#' k_agdm and f_shoot_dm_min are the fitted coefficients
#' @return
#' @export
shoot_fraction_fitted = function(k_agdm=0.09792063,
                                 f_n,
                                 f_shoot_dm_min=0.2853004) {

  f_shoot = k_agdm*f_n*100+f_shoot_dm_min
  return(f_shoot)
}

#' belowground_yield_DM
#'
#' @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @param f_shoot fraction of total DM production that is aboveground (??gDM; shoot fraction) (0-1)
#' @description
#' computes belowground production (Ygr)
#' @unit kg ha-1 yr-1
#' @return
#' @export
belowground_yield_DM = function(yield_dm,
                                f_harvested=0.8,
                                f_shoot) {

  bg_yield_dm = (yield_dm/f_harvested)*((1/f_shoot)-1)
  return(bg_yield_dm)
}

#' N_concentration_roots_fitted
#'
#' @param k_gr coefficient
#' @param f_n the concentration of N in the harvested material (0-1) (ngs; kg kg DM-1 yr-1)
#' @param f_gr_min Concentration of N in the root material when the N in shoot is extrapolated to zero (ngrmin, kg N kg DM-1)
#' @description
#' fits N concentration in roots based on fitted data rom Schenk and the Gorissen
#' @source fitted data used data from Schenk and the Gorissen
#' @return
#' @export
N_concentration_roots_fitted = function(k_gr=0.2413266,
                                         f_n,
                                         f_gr_min=0.2257085) {

  N_conc_roots = k_gr*f_n+f_gr_min
  return(N_conc_roots)
}


#' N_input_roots
#'
#' @param yield_dm DM yield of harvested aboveground material (Ygs ;kg ha-1 yr-1)
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @param f_shoot fraction of total DM production that is aboveground (??gDM; shoot fraction) (0-1)
#' @param f_n_roots N concentration in roots (ng; kg N kg DM-1)
#' @description
#' computes the N input from roots (Ngr)
#' @unit kg N ha-1 yr-1
#' @return
#' @export
N_input_roots = function(yield_dm,
                         f_harvested=0.8,
                         f_shoot,
                         f_n_roots) {

  N_in_roots = yield_dm/f_harvested*f_n_roots*(1/f_shoot-1)
  return(N_in_roots)
}
