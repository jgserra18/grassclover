
#' find_NY0_partitioning
#'
#' @param site_name site name
#' @param year 1,n
#' @description
#' for a given site name partitions NYield at Nfert=0 according to the year and previous crop rotation
#' @return
#' @export
#'
#' @examples
find_NY0_partitioning = function(site_name,
                                 year) {
  
  data('site_rotation')
  
  id_site = which(site_rotation$Site==site_name)
  
  if (length(id_site)==0 | year != 1) {
    f_Nback = 1/3; f_Nres_t1 = 2/3
  }
  else {
    site_params = site_rotation[id_site,]
    f_Nback = site_params$f_Nback; f_Nres_t1 = site_params$f_Nres_t1
  }
  return(list(
    f_Nback = f_Nback,
    f_Nres_t1 = f_Nres_t1
  ))
}




