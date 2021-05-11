#' Title
#'
#' @param inflow_file_dir
#' @param inflow_obs
#' @param working_directory
#' @param start_datetime_local
#' @param end_datetime_local
#' @param forecast_start_datetime_local
#' @param use_future_inflow
#' @param state_names
#' @param tz
#'
#' @return
#' @export
#'
#' @examples
create_inflow_outflow_files <- function(inflow_file_dir,
                                        inflow_obs,
                                        working_directory,
                                        start_datetime_local,
                                        end_datetime_local,
                                        forecast_start_datetime_local,
                                        use_future_inflow,
                                        state_names,
                                        tz,
                                        use_ler = FALSE) {
  if(use_ler) {
    inflow_outflow_files <- create_ler_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                           inflow_obs = cleaned_inflow_file,
                                                           working_directory = config$run_config$execute_location,
                                                           start_datetime_local = start_datetime_local,
                                                           end_datetime_local = end_datetime_local,
                                                           forecast_start_datetime_local = forecast_start_datetime_local,
                                                           use_future_inflow = TRUE,
                                                           state_names = NULL,
                                                           tz = config$local_tzone)
  } else {
    inflow_outflow_files <- create_glm_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                           inflow_obs = cleaned_inflow_file,
                                                           working_directory = config$run_config$execute_location,
                                                           start_datetime_local = start_datetime_local,
                                                           end_datetime_local = end_datetime_local,
                                                           forecast_start_datetime_local = forecast_start_datetime_local,
                                                           use_future_inflow = TRUE,
                                                           state_names = NULL)
  }
  return(inflow_outflow_files)
}
