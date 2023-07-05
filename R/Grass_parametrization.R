
#' grass_simulation_metrics
#'
#' @param N_fert N fertilisation level (eg, 100)
#' @param year int (0,n)
#' @param f_min_t fraction of mineralized at timestep t, sigma
#' @param f_min_t1 fraction of mineralized at timestep t+1, beta g
#' @param df_obs S23 obs for a given site and fertilisation level
#' @param df_obs_Nback S23 obs for a given site
#' @param yield_dm_max maximum yield DM (kg ha-1 yr-1)
#' @param param_file file with parameters (set_paramets)
#' @param N_min_t1 N mineralized in the year before
#' @description
#' function that runs simulation of grass only for a given f_min_t and f_min_t1 and returns sum of nRMSE
#' of simulated and observed data
#' @return list with different output; see function output
#' @export
#'
#' @examples
grass_simulation_metrics = function(site_name,
                                    N_fert = 100,
                                    year=1,
                                    f_min_t,
                                    df_obs,
                                    df_obs_partition,
                                    yield_dm_max,
                                    param_file,
                                    N_back=NULL,
                                    N_res_avail_t1 = NULL) {

  NY0 = 2.5*df_obs_partition$value[df_obs_partition$Yield=="Nyield"&df_obs_partition$variable==0]
 # NY0 = ifelse(year==1, 2.5*df_obs$value[df_obs$Yield=="Nyield"], 2.5*df_obs_partition$value[df_obs_partition$Yield=="Nyield"&df_obs_partition$variable==0])
  f_partitions = find_NY0_partitioning(site_name = site_name, year = year)

  if (year == 1) {
    N_res_avail_t1 = NY0 * f_partitions$f_Nres_t1
  }
  if (missing(N_back)==T) { N_back = NY0 * f_partitions$f_Nback } else { N_back = N_back }
  #N_back = ifelse(year==1, 1.4*df_obs$value[df_obs$Yield=="Nyield"], df_obs_Nback$value[df_obs_Nback$Yield=="Nyield"&df_obs_Nback$variable==0]) # update N back based on whether it is the first year or not
  grass_loop = grass_only_simulation(N_fert = N_fert, yield_dm_max = yield_dm_max, f_min_t = f_min_t, param_file = param_file, N_back_before = N_back, N_res_avail_t1 = N_res_avail_t1)

  sim_Nyield = grass_loop[['N_yield']]
  sim_dm_yield = grass_loop[['yield_dm']]

  eval_Nyield = Metrics::rmse(actual = df_obs$value[df_obs$Yield=="Nyield"],sim_Nyield)/mean(df_obs$value[df_obs$Yield=="Nyield"])
  eval_dm_yield = Metrics::rmse(actual = df_obs$value[df_obs$Yield=="Total"],sim_dm_yield)/
                                  mean(df_obs$value[df_obs$Yield=="Total"])
  return(list(
    N_res = grass_loop$N_res,
    N_plant = grass_loop$N_plant,
    N_upt = grass_loop$N_upt,
    yield_dm_max = yield_dm_max,
    sim_Nyield = sim_Nyield,
    obs_Nyield = df_obs$value[df_obs$Yield=="Nyield"],
    sim_dm_yield = sim_dm_yield,
    obs_dm_yield = df_obs$value[df_obs$Yield=="Total"],
    N_back = grass_loop[['N_back']],
    N_res_avail_t1 = grass_loop$N_res_avail_t1,
    eval_Nyield = eval_Nyield,
    eval_dm_yield = eval_dm_yield,
    #alpha = alpha,
    eval_sum = eval_Nyield+eval_dm_yield))
}

#' grass_simulation_per_site
#'
#' @param site_name site name with experimental data
#' @param yrs experimental data years
#' @param param_file parameter file
#' @param N_fert fertilisation levels (seq(0,600,100))
#' @description
#' for a given sitename, loops for the different N fert levels and finds the optimal parameters (based on the minimum nRMSE)
#' @return returns a dataframe with the best simulations for the different years and fertilisation levels
#' @export
#' @note This is not terribly efficient as it is; just to test whether it works
#' @examples looping_per_site('Leeds',c(1978,1980,1981))
grass_simulation_per_site = function(site_name = 'Leeds',
                                     yrs = 1978:1980,
                                     param_file,
                                     N_fert = seq(0,600,100)) {
  data('S23')

  store_out_df = list()
  for (fert in 1:length(N_fert)) {
    store_N_res_avail_t1 = c()
    store_out = vector('list', length(yrs))

    for (i in 1:length(yrs)) {
     print(i)
      new_store_N_res_avail_t1 = ifelse(i>1, store_N_res_avail_t1[i-1], NA)# N min from the previous year after year 1

      obs = subset(S23,Site==site_name&Year==yrs[i] & variable==N_fert[fert])
      obs_Nback = subset(S23,Site==site_name&Year==yrs[1]& variable==N_fert[1])
      obs_dm_max =subset(S23,Site==site_name&Year==yrs[i])
      yield_dm_max = calculate_max_yield_DM(df_obs =obs_dm_max)

      N_fert_lim = apply_max_yield_threshold(df_obs = df_obs, threshold = 0.7)

      if (N_fert_lim<N_fert[fert]) {
        next
      }
      else {
        # store simulations in list----
        ## optimization ----
        wrapper = function(params, ...) {
          f_min_t = params[1]
          return(grass_simulation_metrics(f_min_t = f_min_t, ...)[['eval_Nyield']])
        }

        store_sim = DEoptim::DEoptim(fn = wrapper, lower = c(0), upper = c(1),
                         N_fert=N_fert[fert],
                         site_name=x,
                         year=i,
                         df_obs=obs,
                         df_obs_partition = obs_Nback,
                         yield_dm_max = yield_dm_max,
                         N_res_avail_t1 = new_store_N_res_avail_t1,
                         param_file = param_file,
                         control = DEoptim.control(itermax = 100, parallelType = 1, reltol = 0.01))

        ## calculates Normalised Root Mean Square Error for N yield and yield DM
        # store_sim = lapply(1:nrow(param_dist), function(x) {
        #   grass_simulation_metrics(N_fert = N_fert[fert], site_name = site_name, year = i, f_min_t = param_dist[x,1], df_obs = obs, df_obs_partition = obs_Nback, yield_dm_max = yield_dm_max, N_res_avail_t1 = new_store_N_res_avail_t1, param_file = param_file, alpha = alpha[x])
        # })

        # find simulation with the lowest RMSE (N yield + Yield) ----
        ## Get stored info for that simulation
        ## Populate param list
        #id = which.min(sapply(store_sim, function(x) x[['eval_Nyield']]))
        #optm_sim = store_sim[[id]]

        optim_params = store_sim$optim$bestmem
        f_min_t =  optim_params[1]
      }

      min_df = cbind(new_param_dist[id,], data.frame(optm_sim))
      min_df$Nfert = N_fert[fert]; min_df$year = yrs[i]; min_df$site = site_name
      store_N_res_avail_t1[i] = min_df[1,'N_res_avail_t1'] # update N min t1

      store_out[[i]] = min_df
    }
    store_out_df[[fert]] = data.table::rbindlist(store_out)
  }
  return(data.table::rbindlist(store_out_df))
}





#' grassclover_simulation_metrics
#'
#' @param species
#' @param N_fert
#' @param year
#' @param grass_f_min_t
#' @param clover_f_min_t
#' @param f_min_t1
#' @param df_obs
#' @param df_obs_Nback
#' @param grass_yield_dm_max
#' @param N_min_t1
#'
#' @return
#' @export
#'
#' @examples
grassclover_simulation_metrics = function(species=c('blanca','s100'),
                                          N_fert = 100,
                                          year=1,
                                          grass_f_min_t,
                                          clover_f_min_t,
                                          f_min_t1,
                                          df_obs,
                                          df_obs_Nback,
                                          grass_yield_dm_max = 12000,
                                          N_min_t1=NULL) {

  N_back = ifelse(year==1, 1.4*df_obs$value[df_obs$variable=="Total_N_yield"], df_obs_Nback$value[df_obs_Nback$variable=="Total_N_yield"&df_obs_Nback$N_fert==0]) # update N back based on whether it is the first year or not

  grassclover_loop = grassclover_simulation(N_fert = N_fert, grass_f_min_t = grass_f_min_t, clover_f_min_t = clover_f_min_t, f_min_t1 = f_min_t1, N_back_before = N_back, grass_N_min_t1 = grass_f_min_t, clover_N_min_t1 = clover_f_min_t, grass_yield_dm_max = grass_yield_dm_max)
  sim_grass_N_yield = grassclover_loop[['grass_n_yield']]
  sim_grass_dm_yield = grassclover_loop[['grass_yield_dm']]
  sim_clover_N_yield = grassclover_loop[['clover_N_yield']]
  sim_clover_dm_yield = grassclover_loop[['clover_yield_dm']]
  sim_tot_N_yield = grassclover_loop[['tot_N_yield']]
  sim_tot_dm_yield = grassclover_loop[['tot_DM_yield']]

  eval_grass_dm =
    eval_grass_Ny =
    eval_clover_dm =
    eval_clover_Ny =
    eval_tot_dm =
    eval_tot_Ny =
    eval_Nyield = Metrics::rmse(actual = df_obs$value[df_obs$Yield=="Nyield"],sim_Nyield)/mean(df_obs$value[df_obs$Yield=="Nyield"])
  eval_dm_yield = Metrics::rmse(actual = df_obs$value[df_obs$Yield=="Total"],sim_dm_yield)/mean(df_obs$value[df_obs$Yield=="Total"])
  return(list(
    yield_dm_max = yield_dm_max,
    sim_Nyield = sim_Nyield,
    obs_Nyield = df_obs$value[df_obs$Yield=="Nyield"],
    sim_dm_yield = sim_dm_yield,
    obs_dm_yield = df_obs$value[df_obs$Yield=="Total"],
    N_back = grassclover_loop[['N_back']],
    grass_N_min_t1 = grassclover_loop[['grass_N_min_t1']],
    clover_N_min_t1 = grassclover_loop[['clover_N_min_t1']],
    eval_Nyield = eval_Nyield,
    eval_dm_yield = eval_dm_yield,
    eval_sum = eval_Nyield+eval_dm_yield))
}


