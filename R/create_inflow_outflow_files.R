#' Title
#'
#' @param inflow_file_dir
#' @param inflow_obs
#' @param working_directory
#' @param config
#' @param state_names
#'
#' @return
#' @export
#'
#' @examples
create_inflow_outflow_files <- function(inflow_file_dir,
                                        inflow_obs,
                                        working_directory,
                                        config,
                                        state_names) {
  if(config$model_settings$use_ler) {
    inflow_outflow_files <- create_ler_inflow_outflow_files(inflow_file_dir,
                                                            inflow_obs,
                                                            working_directory,
                                                            config,
                                                            state_names)
  } else {
    inflow_outflow_files <- create_glm_inflow_outflow_files(inflow_file_dir,
                                                            inflow_obs,
                                                            working_directory,
                                                            config,
                                                            state_names)
  }
  return(inflow_outflow_files)
}
