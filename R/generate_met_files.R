#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dir
#' @param local_tzone
#' @param start_datetime_local
#' @param end_datetime_local
#' @param forecast_start_datetime_local
#' @param use_forecasted_met
#' @param use_ler
#'
#' @importFrom zoo na.approx
#' @return
#' @export
#'
#' @examples
generate_met_files <- function(obs_met_file = NULL,
                               out_dir,
                               forecast_dir = NULL,
                               config){
  if(config$use_ler) {
    met_out <- generate_ler_met_files(obs_met_file = obs_met_file,
                                      out_dir,
                                      forecast_dir = forecast_dir,
                                      config)
  } else {
    met_out <- generate_glm_met_files(obs_met_file = obs_met_file,
                                      out_dir,
                                      forecast_dir = forecast_dir,
                                      config)
  }
  return(met_out)
}
