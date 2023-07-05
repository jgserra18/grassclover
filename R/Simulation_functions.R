
#' grass_only_simulation
#'
#' @param yield_dm_max max yield dm; max threshold
#' @param N_fert N fertilisation (the higher, the more the model reaches equilibrium)
#' @param f_min_t fraction of crop residues mineralized at timestep t
#' @param f_min_t1 fraction of crop residues mineralized at timestep t+1 (next year)
#' @param N_back_before N background; set this up only in the first year, otherwise it is from previous year
#' @description
#' loop until model reaches equilibrium where plant available N is similar to total crop N uptake
#' @return
# grass_only_simulation_OLD = function(yield_dm_max = 12000,
#                                   N_fert = 100,
#                                   f_min_t = 0.6,
#                                   f_min_t1 = 0.25,
#                                   N_back_before=NULL,
#                                   N_min_t1=NULL) {
#
#   # initial parameters
#   yield_dm = 1 # star small
#   yield_change = 1
#
#   timestep = 1
#   N_upt = 0;  N_plant = 150 # give very high values
#   N_min_t = 2 # initialize N min values per Jeroen
#   if (length(N_min_t1)==0) {  N_min_t1 = list(0,0) }
#
#   while (N_plant-N_upt>0.01 & yield_dm<=yield_dm_max) {
#     #print(timestep)
#
#     # Plant available N ----
#     N_eff_fert = input_Nfertiliser(N_fert = N_fert, ef_ammonia = set_parameters$ef_ammonia)
#   #  N_back = input_background_soil_N(ef_loss = set_parameters$ef_loss, N_residues_t = N_res, N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]])
#    # N_back = background_soil_N_modifier(N_back_before = N_back_before, N_back = N_back, t = timestep) # updates N back to receive inputs
#     #if (N_back[[1]]<0) stop('N background lower than 0')
#     N_plant = plant_N_avaible(N_eff_fert = N_eff_fert[[1]], N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]], N_back = N_back_before)
#
#     ## N from residues above ground ----
#     f_n = N_conc_harvested(yield_dm = yield_dm, yield_dm_max = yield_dm_max, f_n_min = set_parameters$f_n_min, f_n_max = set_parameters$f_n_max)
#     n_yield = N_yield_combined(yield_dm = yield_dm, yield_dm_max = yield_dm_max, f_n_min = f_min_t, f_n_max = set_parameters$f_n_max)
#     N_in_aboveground = N_yield_soil_aboveground(N_yield = n_yield)
#
#     ## N from residues below ground ----
#     f_shoots = shoot_fraction_fitted(f_n = f_n)
#     f_n_roots = N_concentration_roots_fitted(f_n = f_n*100) # %N
#     N_in_belowground = N_input_roots(yield_dm = yield_dm, f_shoot = f_shoots, f_n_roots = f_n_roots/100)
#
#     ## Residues soil input ----
#     N_res = N_residues_in(N_in_aboveground = N_in_aboveground, N_in_roots = N_in_belowground)
#
#     ## Total N uptake ----
#     ### check this ----
#     #N_upt = total_plant_N_uptake(N_residues_t = N_res, N_yield = n_yield)
#     N_upt = N_res + n_yield
#
#     ## Update mineralization ----
#     N_min_t = input_mineralization_same_year(f_min_t = f_min_t, N_residues_t = N_res)#, t = timestep)
#     N_min_t1 = input_mineralization_following_year(ef_loss = set_parameters$ef_loss, f_min_t = f_min_t, N_residues_t = N_res, f_min_t1 = f_min_t1)#, t = timestep)
#
#
#     # update counters
#     yield_dm = yield_dm + yield_change
#     timestep = timestep + 1
#   }
#
#   return(list(
#     yield_dm = yield_dm,
#     timestep = timestep,
#     N_upt = N_upt,
#     N_plant = N_plant,
#     N_min_t1 = N_min_t1[[1]],
#     N_back = N_back_before,
#     N_yield = n_yield
#   ))
# }


grass_only_simulation = function(yield_dm_max = 12000,
                                 N_fert = 100,
                                 f_min_t = 0.6,
                                 param_file,
                                 N_back_before=NULL,
                                 N_res_avail_t1=NULL) {

  # initial parameters
  yield_dm = 200 # star small
  yield_change = 1

  timestep = 1
  N_upt = 0; N_plant = 150 # give very high values
  N_res_avail_t = 0.01 # initialize N res min values per Jeroen

  while (N_plant-N_upt>0.01 & yield_dm<=yield_dm_max) {
    #print(timestep)
    # Plant available N ----
    N_eff_fert = input_Nfertiliser(N_fert = N_fert, param_file = param_file)
    N_plant_calc = plant_N_available(N_eff_fert = N_eff_fert[[1]], N_res_avail_t = N_res_avail_t, N_res_avail_t1 = N_res_avail_t1, N_back = N_back_before, param_file = param_file)
    N_plant = N_plant_calc[[1]]

    ## N from residues above ground ----
    f_n = N_conc_harvested_fitted(yield_dm = yield_dm, yield_dm_max = yield_dm_max)
   # n_yield = alpha * yield_dm * f_n
    n_yield = N_yield(yield_dm = yield_dm, f_n = f_n)
    N_in_aboveground = N_yield_soil_aboveground(N_yield = n_yield, f_harvested = param_file$f_harvested)

    ## N from residues below ground ----
    f_shoots = shoot_fraction_fitted(f_n = f_n)
  #  f_shoots = alpha
    f_n_roots = N_concentration_roots_fitted(f_n = f_n*100) # %N
    N_in_belowground = N_input_roots(yield_dm = yield_dm, f_shoot = f_shoots, f_n_roots = f_n_roots/100)
    yield_belowground = belowground_yield_DM(yield_dm = yield_dm, f_shoot = f_shoots)


    ## Residues soil input ----
    N_res = N_residues_in(N_in_aboveground = N_in_aboveground, N_in_roots = N_in_belowground)

    ## Total N uptake ----
    N_upt = N_res + n_yield

    # N humus ----
    N_humus_currYear = N_humus_current_year(f_min_t = f_min_t, yield_dm = yield_dm, yield_dm_belowground = yield_belowground)
    N_humus_nxYear = N_humus_next_year(f_min_t = f_min_t, yield_dm = yield_dm, yield_dm_belowground = yield_belowground)

    # N residues mineralization ----
    N_res_avail_t = N_residues_availability_current_year(f_min_t = f_min_t, N_res_total = N_res, N_humus_t = N_humus_currYear)
    N_res_avail_t1 = N_residues_availability_next_year(f_min_t = f_min_t, N_res_total = N_res, N_humus_t = N_humus_nxYear)

    # update counters
    yield_dm = yield_dm + yield_change
    timestep = timestep + 1
    #print(paste0(round(N_res_avail_t1,1),';;', N_back_before))
  }

  return(list(
    N_res = N_res,
    yield_dm = yield_dm,
    timestep = timestep,
    N_upt = N_upt,
    N_plant = N_plant,
    N_res_avail_t1 = N_res_avail_t1,
    N_back = N_back_before,
    N_yield = n_yield
  ))
}






grassclover_simulation = function(N_fert = 0,
                                  grass_f_min_t = 0.25,
                                  clover_f_min_t = 0.25,
                                  f_min_t1 = 0.5,
                                  N_back_before=NULL,
                                  grass_N_min_t1=NULL,
                                  clover_N_min_t1 = NULL,
                                  grass_yield_dm_max=12000) {


  # initial parameters
  grass_yield_dm = 1 # # iteractive process
  yield_change = 3

  timestep = 1
  N_upt = 0;  N_plant = 150 # give very high values
  N_min_t = 2 # initialize N min values per Jeroen
  if (length(grass_N_min_t1)==0 & length(clover_N_min_t1)==0) {  grass_N_min_t1 = clover_N_min_t1 = list(0,0) }

  while (N_plant-grass_Nuptake>0.01 & grass_yield_dm<=grass_yield_dm_max) {

    # plant N available ----
    N_eff_fert = input_Nfertiliser(N_fert = N_fert, ef_ammonia = set_parameters$ef_ammonia)

    if (timestep==1) {
      N_min_t1 = grass_N_min_t1[[1]] + clover_N_min_t1 # sum grass and clover N min from previous years; if year==1, these are 0, else they are an input and must be summed
      N_plant = plant_N_avaible(N_eff_fert = N_eff_fert[[1]], N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]], N_back = N_back_before)
    }

    ## grass yield ----
    f_n_grass = N_conc_harvested(yield_dm = grass_yield_dm, yield_dm_max = grass_yield_dm_max, f_n_min = set_parameters$f_n_min, f_n_max = set_parameters$f_n_max)
    grass_n_yield = N_yield_combined(yield_dm = grass_yield_dm, yield_dm_max = grass_yield_dm_max, f_n_min = set_parameters$f_n_min, f_n_max = set_parameters$f_n_max)

    ## clover yield ----
    f_clover = fraction_clover(grass_yield_dm = grass_yield_dm, grass_yield_dm_max = grass_yield_dm_max, over_yielding_factor = set_parameters$over_yielding_factor)
    clover_yield_dm = clover_DM_yield(yield_clover_dm_max = set_parameters$yield_clover_max, f_clover = f_clover)
    clover_N_yield = clover_N_yield_base(yield_clover_dm_max = set_parameters$yield_clover_max, f_clover = f_clover, f_n_clover = set_parameters$f_n_clover)

    ## total yield ----
    tot_DM_yield = clover_yield_dm + (grass_n_yield/f_n_grass)
    tot_N_yield = clover_N_yield + grass_n_yield

    ## grass N residues ----
    grass_N_in_aboveground = N_yield_soil_aboveground(N_yield = grass_n_yield)
    f_shoots = shoot_fraction_fitted(f_n = f_n_grass)
    f_n_roots = N_concentration_roots_fitted(f_n = f_n_grass*100) # %N
    grass_N_in_belowground = N_input_roots(yield_dm = grass_yield_dm, f_shoot = f_shoots, f_n_roots = f_n_roots/100)
    grass_N_res = N_residues_in(N_in_aboveground = grass_N_in_aboveground, N_in_roots = grass_N_in_belowground)

    ## grass N uptake
    #grass_Nuptake = grass_N_uptake(f_shoot = f_shoots, N_yield = grass_n_yield)
    grass_Nuptake = grass_N_res + grass_n_yield

    ## clover N residues ----
    clover_N_res = clover_N_residues(N_yield = clover_N_yield, f_Navail = set_parameters$f_Navail)

    # clover N ----
    N_clover = clover_N_total(N_yield = clover_N_yield, f_ab_clover = set_parameters$f_Navail)
    Nmin_clover = clover_N_mineralization(N_availability= N_plant, clover_N_total=N_clover, grass_N_uptake=grass_Nuptake) # what is this??

    ## clover N fixation ----
    total_clover_Nfixation = N_clover - Nmin_clover
    harvested_clover_Nfixation = clover_harvested_N_fixation(clover_N_yield=clover_N_yield, N_min_clover=Nmin_clover, f_Navail = set_parameters$f_Navail)
    Ndfa = clover_Ndfa(clover_N = N_clover, total_N_fixation = total_clover_Nfixation)

    ## soil excess N? ----
    excess_soilN = excess_soil_N(N_availability = N_plant ,grass_Nuptake = grass_Nuptake, clover_N_min = Nmin_clover)

    ## Total residues ----
    Nrs_total = clover_N_res + grass_N_res

    ## Mineralization current year ----
    grass_N_min_t = input_mineralization_same_year(f_min_t = grass_f_min_t, N_residues_t = grass_N_res)
    glover_N_min_t = input_mineralization_same_year(f_min_t = clover_f_min_t, N_residues_t = clover_N_res)
    N_min_t = grass_N_min_t + glover_N_min_t + excess_soilN

    ## Mineralization next year ----
    grass_N_min_t1 = input_mineralization_following_year(ef_loss = set_parameters$ef_loss, f_min_t = grass_f_min_t, f_min_t1 = f_min_t1, N_residues_t = grass_N_res)
    clover_N_min_t1 = input_mineralization_following_year(ef_loss = set_parameters$ef_loss, f_min_t = clover_f_min_t, f_min_t1 = f_min_t1, N_residues_t = clover_N_res)
    N_min_t1 = grass_N_min_t1[[1]] + clover_N_min_t1[[1]]

    if (timestep!=1) {
      N_plant = plant_N_avaible(N_eff_fert = N_eff_fert[[1]], N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]], N_back = N_back_before)
    }

    # update counters
    grass_yield_dm = grass_yield_dm + yield_change
    timestep = timestep + 1
  }

  return(list(
    grass_yield_dm = grass_yield_dm, # simulated grass yield
    timestep = timestep,
    grass_Nuptake = grass_Nuptake, # grass total N uptake
    N_plant = N_plant, # plant N availability
    grass_N_min_t1 = grass_N_min_t1[[1]], # grass mineralization from t-1
    clover_N_min_t1 = clover_N_min_t1[[1]], # clover mineralization from t-1
    N_back = N_back_before, # soil N background
    grass_n_yield = grass_n_yield, # Grass N yield
    clover_n_yield = clover_N_yield, # Clover N yield
    tot_DM_yield = tot_DM_yield, # total DM yield
    tot_N_yield = tot_N_yield, # total N yield
    clover_yield_dm = clover_yield_dm # clover DM yield
  ))
}






