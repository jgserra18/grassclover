#' clover_N_mineralization
#'
#' @param N_availability  N availability; see plant_N_avaible() (kg N ha-1 yr-1)
#' @param clover_N_total total crop N of clover (kg N ha-1 yr-1)
#' @param grass_N_uptake total grass N uptake (kg N ha-1 yr-1)
#' @description
#' computes N mineralization from clover
#' @unit kg N ha-1 yr-1
#' @return
#' @export
clover_N_mineralization = function(N_availability,
                                   clover_N_total,
                                   grass_N_uptake) {

  N_min = ifelse(N_availability-grass_N_uptake>clover_N_total, clover_N_total, N_availability-grass_N_uptake)
  return(N_min)
}

#' clover_N_residues
#'
#' @param N_yield clover N yield (;kg N ha-1 yr-1)
#' @param f_Navail  Proportion of available N  +  fixed N recovered in clover yield (alpha_c) (0-1)
#' @description
#' computes N in clover residues
#' @unit kg N ha-1 yr-1
#' @return
#' @export
clover_N_residues = function(N_yield,
                             f_Navail=0.5) {

  N_res = (1-f_Navail) * N_yield /f_Navail
  return(N_res)
}
