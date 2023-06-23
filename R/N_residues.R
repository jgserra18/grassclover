#' N_residues_in
#'
#' @param N_in_aboveground N into the soil from aboveground residues; see N_yield_soil_aboveground()
#' @param N_in_roots N into the soil from belowground residues; see N_input_roots()
#' @description
#' computes N into the soil from above- and belowground residues (N_residuesg)
#' @unit kg N ha-1 yr-1
#' @note
#' See Above_ground_production.R and below_ground_production.R
#' @return
#' @export
N_residues_in = function(N_in_aboveground,
                         N_in_roots) {

  N_residues = N_in_aboveground + N_in_roots
  return(N_residues)
}


#' N_residues_availability_current_year
#'
#' @param f_min_t fraction of residues mineralized in the same year; gamma; (0-1)
#' @param N_res_total total N from residues (aboveground + roots)
#' @param N_humus_t (N bound in humus in the current year)
#' @description
#' computes mineral N available for plant uptake from the decomposition of residues in the current year
#' @unit kg N ha-1 yr-1
#' @return
#' @export
#'
#' @examples
N_residues_availability_current_year = function(f_min_t,
                                               N_res_total,
                                               N_humus_t) {

  N_res_avail_t = f_min_t * N_res_total - N_humus_t
  return(N_res_avail_t)
}

#' N_residues_availability_next_year
#'
#' @param f_min_t fraction of residues mineralized in the same year; gamma; (0-1)
#' @param N_res_total total N from residues (aboveground + roots)
#' @param N_humus_t1 (N bound in humus in the current year)
#' @description
#' computes mineral N available for plant uptake from the decomposition of residues in the following year
#' @unit kg N ha-1 yr-1
#' @return
#' @export
#'
#' @examples
N_residues_availability_next_year = function(f_min_t,
                                             N_res_total,
                                             N_humus_t1) {

  N_res_avail_t1 = (1-f_min_t) * N_res_total - N_humus_t
  return(N_res_avail_t1)
}
