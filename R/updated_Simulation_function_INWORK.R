

yield_dm = 1 # star small
yield_change = 1

timestep = 1
N_upt = 0;  N_plant = 150 # give very high values
N_min_t = 2 # initialize N min values per Jeroen
if (length(N_min_t1)==0) {  N_min_t1 = list(0,0) }

while (N_plant-N_upt>0.01 & yield_dm<=yield_dm_max) {
  #print(timestep)

  # Plant available N ----
  N_eff_fert = input_Nfertiliser(N_fert = N_fert, ef_ammonia = set_parameters$ef_ammonia)
  #  N_back = input_background_soil_N(ef_loss = set_parameters$ef_loss, N_residues_t = N_res, N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]])
  # N_back = background_soil_N_modifier(N_back_before = N_back_before, N_back = N_back, t = timestep) # updates N back to receive inputs
  #if (N_back[[1]]<0) stop('N background lower than 0')
  N_plant = plant_N_avaible(N_eff_fert = N_eff_fert[[1]], N_min_t = N_min_t, N_min_t1 = N_min_t1[[1]], N_back = N_back_before)

  ## N from residues above ground ----
  f_n = N_conc_harvested(yield_dm = yield_dm, yield_dm_max = yield_dm_max, f_n_min = set_parameters$f_n_min, f_n_max = set_parameters$f_n_max)
  n_yield = N_yield_combined(yield_dm = yield_dm, yield_dm_max = yield_dm_max, f_n_min = f_min_t, f_n_max = set_parameters$f_n_max)
  N_in_aboveground = N_yield_soil_aboveground(N_yield = n_yield)

  ## N from residues below ground ----
  f_shoots = shoot_fraction_fitted(f_n = f_n)
  f_n_roots = N_concentration_roots_fitted(f_n = f_n*100) # %N
  N_in_belowground = N_input_roots(yield_dm = yield_dm, f_shoot = f_shoots, f_n_roots = f_n_roots/100)

  ## Residues soil input ----
  N_res = N_residues_in(N_in_aboveground = N_in_aboveground, N_in_roots = N_in_belowground)

  ## Total N uptake ----
  ### check this ----
  #N_upt = total_plant_N_uptake(N_residues_t = N_res, N_yield = n_yield)
  N_upt = N_res + n_yield

  ## Update mineralization ----
  N_min_t = input_mineralization_same_year(f_min_t = f_min_t, N_residues_t = N_res)#, t = timestep)
  N_min_t1 = input_mineralization_following_year(ef_loss = set_parameters$ef_loss, f_min_t = f_min_t, N_residues_t = N_res, f_min_t1 = f_min_t1)#, t = timestep)


  # update counters
  yield_dm = yield_dm + yield_change
  timestep = timestep + 1
}

return(list(
  yield_dm = yield_dm,
  timestep = timestep,
  N_upt = N_upt,
  N_plant = N_plant,
  N_min_t1 = N_min_t1[[1]],
  N_back = N_back_before,
  N_yield = n_yield
))


alpha_g_estimative_t1 = function(yield_dm,
                                 yield_dm_max,
                                 N_back) {



  N_conc_above = N_conc_harvested_fitted(yield_dm = yield_dm, yield_dm_max = yield_dm_max)/100
  N_yield = N_conc_shoot * yield_dm
  N_in_aboveground = N_yield_soil_aboveground(N_yield = N_yield, f_harvested = 0.8)

  f_shoot = shoot_fraction_fitted(f_n = N_conc_above) # alpha_gDM
  yield_dm_roots = belowground_yield_DM(yield_dm = yield_dm, f_harvested = 0.8, f_shoot = f_shoot) #Ygr
  N_conc_roots = N_concentration_roots_fitted(f_n = N_conc_above*100)
  N_in_belowground = N_input_roots(yield_dm = yield_dm, f_harvested = 0.8, f_shoot = f_shoot, f_n_roots = N_conc_roots)

  N_res = N_residues_in(N_in_aboveground = N_in_aboveground, N_in_roots = N_in_belowground)
  Total_dm_yield = total_DM_yield(yield_dm = yield_dm, f_harvested = 0.8, yield_roots = yield_dm_roots)
  N_upt = N_res + N_yield # N grass

  return( N_conc_above*yield_dm/N_upt)
}


N_conc_above = N_conc_harvested_fitted(yield_dm = yield_dm, yield_dm_max = yield_dm_max)/100
N_conc_above = 3
N_yield = N_conc_shoot * yield_dm
N_in_aboveground = N_yield_soil_aboveground(N_yield = N_yield, f_harvested = 0.8)

f_shoot = shoot_fraction_fitted(f_n = N_conc_above) # alpha_gDM
yield_dm_roots = belowground_yield_DM(yield_dm = yield_dm, f_harvested = 0.8, f_shoot = f_shoot) #Ygr
N_conc_roots = N_concentration_roots_fitted(f_n = N_conc_above*100)
N_in_belowground = N_input_roots(yield_dm = yield_dm, f_harvested = 0.8, f_shoot = f_shoot, f_n_roots = N_conc_roots)

N_res = N_residues_in(N_in_aboveground = N_in_aboveground, N_in_roots = N_in_belowground)
Total_dm_yield = total_DM_yield(yield_dm = yield_dm, f_harvested = 0.8, yield_roots = yield_dm_roots)
N_upt = N_res + N_yield # N grass

alpha_g = N_conc_above*yield_dm/N_upt


N_back = N_yield / alpha_g


f_min_t1 = 0.84
+

# t = 1; first calculte alpha g (which is default); use the carry over N model to know N yield at t=t-1
  # N_back from t=1 is the intercept of the carry over model;;+ if it is grass - if it isn't grass
