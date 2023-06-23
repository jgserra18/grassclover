
#' N_humus_current_year
#'
#' @param f_min_t fraction of residues mineralized in the same year
#' @param yield_dm aboveground yield dm 
#' @param yield_dm_belowground belowground yield dm
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @param N_conversion C:N conversion flows from C-TOOL
#' @description
#' computes N bound in humus from the decomposition of residues at timestep t
#' 
#' @return
#' @export
#'
#' @examples
N_humus_current_year = function(f_min_t,
                                yield_dm,
                                yield_dm_belowground,
                                f_harvested = 0.8,
                                N_conversion = 0.006141) {
  
  N_humus_t = f_min_t * N_conversion * ( (yield_dm*(1/f_harvested-1)+yield_dm_belowground) )
  return(N_humus_t)
}

#' N_humus_next_year
#'
#' @param f_min_t fraction of residues mineralized in the same year
#' @param yield_dm aboveground yield dm 
#' @param yield_dm_belowground belowground yield dm
#' @param f_harvested fraction of aboveground production harvested (??; %) (0-1)
#' @param N_conversion C:N conversion flows from C-TOOL
#' @description
#' computes N bound in humus from the decomposition of residues at timestep t+1
#' 
#' @return
#' @export
#'
#' @examples
N_humus_next_year = function(f_min_t,
                              yield_dm,
                              yield_dm_belowground,
                              f_harvested = 0.8,
                              N_conversion = 0.006141) {
  
  N_humus_t1 = (1-f_min_t) * N_conversion * ( (yield_dm*(1/f_harvested-1)+yield_dm_belowground) )
  return(N_humus_t1)
}
