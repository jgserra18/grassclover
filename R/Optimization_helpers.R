#' generate_random_params
#'
#' @param n length of params
#' @description
#' generates normal dist of residue fractions that always sum to 1
#' @return dataframe with min_t, min_t1 and min back parametrization
#' @export
#'
#' @examples
generate_random_params = function(n=10000) {

  set.seed(123)
  m = matrix(runif(3*n,0,1), ncol=3)
  m = sweep(m, 1, rowSums(m), FUN="/")
  m=as.data.frame(m)
  names(m) = c('min_t','min_t1','min_back')
  m
}


#' calculate_max_yield_DM
#'
#' @param df_obs dataframe of site-specific observations
#' @description
#' computes maximum Yield DM based on a quadratic function
#' @note Done by Jeroen
#' @return maximum yield DM
#' @unit kg DM ha-1 yr-1
#' @export
#'
#' @examples
calculate_max_yield_DM = function(df_obs) {

  quadratic_func = summary(lm(df_obs$value[df_obs$Yield=="Total"]~poly(df_obs$variable[df_obs$Yield=="Total"],2,raw = T)))$coefficients
  c = quadratic_func[1]; b = quadratic_func[2]; a = quadratic_func[3]
  Ymax = c-b*b/4/a

  return(Ymax)
}

#' calculate_max_yield_DM_grassclover
#'
#' @param df_obs dataframe of site-specific observations
#' @description
#' computes maximum Yield DM based on a quadratic function
#' @note Done by Jeroen
#' @return maximum yield DM
#' @unit kg DM ha-1 yr-1
#' @export
#'
#' @examples
calculate_max_yield_DM_grassclover = function(df_obs) {
  
  quadratic_func = summary(lm(df_obs$value[df_obs$variable=="Total_yield_dm"]~poly(df_obs$N_fert[df_obs$variable=="Total_yield_dm"],2,raw = T)))$coefficients
  c = quadratic_func[1]; b = quadratic_func[2]; a = quadratic_func[3]
  Ymax = c-b*b/4/a * 1000 # convert from tonnes to kg 
  
  return(Ymax)
}
