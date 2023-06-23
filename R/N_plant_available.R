# PLANT AVAILABLE N FUNCTIONS AND INPUTS ----

#' input_Nfertiliser
#'
#' @param N_fert
#' @param ef_ammonia
#'
#' @returns listh with effective N input and N-NH3 losses
#' @export
#'
#' @examples
input_Nfertiliser = function(N_fert,
                             ef_ammonia) {

  #' @param N_fert N fertiliser input (kg N ha-1 yr-1)
  #' @param ef_ammonia % N-NH3 losses (0-1) (??)
  #' @description
    #' computes effecive N from fertilisers
    #' excludes N-NH3 emissions
    #' @unit kg N ha-1 yr-1

  N_eff_fert = N_fert*(1-ef_ammonia)
  NH3 = N_fert*ef_ammonia

  return(list(
    N_eff_fert = N_eff_fert,
    NH3_fert = NH3
  ))
}


#' input_mineralization_same_year
#'
#' @param f_min_t fraction of residues mineralized in the same growing period (0-1) (??g)
#' @param N_residues_t N residues input at year t (kg N ha-1 yr-1)
#' @description
#' computes N mineralization from residues during the same growing period
#' @unit
#' kg N ha-1 yr-1
#' @note
#' initial min from previous residues set to 2 like jeroen
#' @return
#' @export
#' NOT USED
input_mineralization_same_year = function(f_min_t,
                                          N_residues_t){#,
                                        #  t=1) {
   min =  N_residues_t * f_min_t
 # min = ifelse(t==1, 2, N_residues_t * f_min_t)
  return(min)
}

#' input_mineralization_following_year
#'
#' @param ef_loss % leaching+denitrification losses (0-1) (??)
#' @param f_min_t  fraction of residues mineralized in the same growing period (0-1) (??g)
#' @param N_residues_t N residues input at year t (kg N ha-1 yr-1)
#' @param f_min_t1  fraction of residues mineralized in the next year (0-1) (??g)
#' @param t timestep; if timestep = 1, then there is no previousl Nmin
#' @description
#' computes N mineralization from residues that will occur next year (t+1 or t1)
#' Includes (i) N losses through denitrification/leaching, (ii) the fraction of residues that will be mineralized at t+1
#' (iii) N mineralization from residues at t
#' @unit kg N ha-1 yr-1
#' @returns list with min for the following year and N losses
#' @export
#' NOT USED
input_mineralization_following_year = function(ef_loss,
                                               f_min_t,
                                               f_min_t1,
                                               N_residues_t) {#,
                                              # t=1) {

  #N_min_t1 = ifelse(t==1, 0, (1-ef_loss)*(f_min_t1*(1-f_min_t)*N_residues_t))
  #N_loss = ifelse(t==1, 0, ef_loss*(f_min_t1*(1-f_min_t)*N_residues_t))
  N_min_t1 = (1-ef_loss)*(f_min_t1*(1-f_min_t)*N_residues_t)
  N_loss = ef_loss*(f_min_t1*(1-f_min_t)*N_residues_t)

  return(list(
    N_min_t1=N_min_t1,
    N_loss = N_loss
  ))
}

#' input_background_soil_N
#'
#' @param ef_loss % leaching+denitrification losses (0-1) (??)
#' @param N_residues_t N residues input at year t (kg N ha-1 yr-1)
#' @param N_min_t  mineralization from residues during the same growing period at year t (kg N ha-1 yr-1); see input_mineralization_same_year()
#' @param N_min_t1 N mineralization from residues that will occur next yea (kg N ha-1 yr-1); see input_mineralization_following_year()
#' @description
#' soil N supply from residues are the remaining N from residues after (i) mineralization at t, (ii) mineralization that will occur at t+1 and (iii) N losses
#' @unit kg N ha-1 yr-1
#' @returns list with soil N humus and N losses
#' @export
#' @note
#' Not used
input_background_soil_N = function(ef_loss,
                                   N_residues_t,
                                   N_min_t,
                                   N_min_t1) {

  Nback = (1-ef_loss)*(N_residues_t-N_min_t-N_min_t1)
  N_loss = ef_loss*(N_residues_t-N_min_t-N_min_t1)
  return(list(
    Nback=Nback,
    N_loss = N_loss
  ))
}

#' background_soil_N_modifier
#'
#' @param N_back_before N background before (other year) or 1.4*N_yield
#' @param N_back calculated N back based on residues and mineralization
#' @param t timestep
#' @description
#' modifies N background to accept input N back (N_back_before)
#' if N_back_before only has 1 length, it automatically assumes 0 N loss and converts it to list (as input_background_soil_N)
#' otherwise it accepts N_back_before input (length=2)
#' if timestep != 1 just receives calculated N back
#' @return list with soil N humus and N losses
#' @export
#'
#' @examples
#' @note
#' NOT USED
background_soil_N_modifier = function(N_back_before,
                                      N_back,
                                      t) {

  if (length(N_back_before)!= 2) { N_back_before = list(Nback = N_back_before,N_loss=0) }

  if (t==1) { N_back = N_back_before } else { N_back = N_back }
  return(N_back)
}


#' plant_N_avaible
#'
#' @param N_eff_fert  N effective fertiliser after NH3 volatilization (kg N ha-1 yr-1); see input_Nfertiliser
#' @param N_min_t N mineralisation from residues at t (kg N ha-1 yr-1); see input_mineralization_same_year
#' @param N_min_t1 N mineralisation from residues from t-1 (kg N ha-1 yr-1); see input_mineralization_following_year
#' @param N_back N available from semi-stable SON (kg N ha-1 yr-1); see input_background_soil_N
#' @description
#' calculates plant available N
#' @unit kg N ha-1 yr-1
#' @return
#' @export
plant_N_avaible_NOTUSED = function(N_eff_fert,
                           N_min_t,
                           N_min_t1,
                           N_back) {

  N_available = N_eff_fert  + N_min_t + N_min_t1 + N_back
  return(N_available)
}


#' plant_N_available
#'
#' @param N_eff_fert N effective fertiliser after NH3 volatilization (kg N ha-1 yr-1); see input_Nfertiliser
#' @param N_res_avail_t N available from crop residues at timestep t
#' @param N_res_avail_t1 N available from crop residues from previous year
#' @param N_back N available from semi-stable SON (kg N ha-1 yr-1); see input_background_soil_N
#' @description
#' Computes plant N available
#' @unit kg N ha-1 yr-1
#' @return
#' @export
#'
#' @examples
plant_N_available = function(N_eff_fert,
                             N_res_avail_t,
                             N_res_avail_t1,
                             N_back) {

  N_avail = N_eff_fert + N_res_avail_t + (1 - set_parameters$ef_loss) * (N_res_avail_t1 + N_back)
  N_loss = set_parameters$ef_loss * (N_res_avail_t1 + N_back)

  return(list(
    plant_N_avail = N_avail,
    N_loss = N_loss
  ))
}


#' excess_soil_N
#'
#' @param N_availability grass N availability; see plant_N_avaible() (kg N ha-1 yr-1)
#' @param grass_Nuptake total grass N uptake (kg N ha-1 yr-1)
#' @param clover_N_min clover N mineralization (kg N ha-1 yr-1)
#' @description
#' mass balance to compute excess N in soil
#' @unit kg N ha-1 yr-1
#' @return
#' @export
excess_soil_N = function(N_availability,
                         grass_Nuptake,
                         clover_N_min) {


  N_exc = N_availability - grass_Nuptake - clover_N_min
  return(N_exc)
}
