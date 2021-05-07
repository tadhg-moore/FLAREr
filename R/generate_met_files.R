#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dir
#' @param start_datetime
#' @param end_datetime
#' @param forecast_start_datetime
#' @param local_tzone
#'
#' @importFrom zoo na.approx
#' @return
#' @export
#'
#' @examples
generate_met_files <- function(obs_met_file = NULL,
                               out_dir,
                               forecast_dir = NULL,
                               local_tzone,
                               start_datetime_local,
                               end_datetime_local,
                               forecast_start_datetime_local,
                               use_forecasted_met,
                               use_ler = FALSE){
  if(use_ler) {
    met_out <- flare::generate_ler_met_files(obs_met_file = observed_met_file,
                                             out_dir = config$run_config$execute_location,
                                             forecast_dir = forecast_path,
                                             local_tzone = config$local_tzone,
                                             start_datetime_local = start_datetime_local,
                                             end_datetime_local = end_datetime_local,
                                             forecast_start_datetime = forecast_start_datetime_local,
                                             use_forecasted_met = TRUE)
  } else {
    met_out <- flare::generate_glm_met_files(obs_met_file = observed_met_file,
                                             out_dir = config$run_config$execute_location,
                                             forecast_dir = forecast_path,
                                             local_tzone = config$local_tzone,
                                             start_datetime_local = start_datetime_local,
                                             end_datetime_local = end_datetime_local,
                                             forecast_start_datetime = forecast_start_datetime_local,
                                             use_forecasted_met = TRUE)
  }
  return(met_out)
}
