#' @title Generating inflow and output files in the model specified format
#' @details Processes historical inflow data from inflow_obs and from files in the inflow_file_dir into the model specified format
#' @param inflow_file_dir string; full directory path that contains forecasted inflow and outflow files
#' @param inflow_obs string; full path to cleaned inflow observation in the specified format
#' @param working_directory string; full directory where FLARE executes
#' @param state_names vector; vector of state names that will be included in the inflow files
#' @return list with two vectors. One vector is the matrix of inflow_file_names and the other is the matrix of outflow_file_names
#' @export
#'
#' @examples
create_inflow_outflow_files <- function(inflow_file_dir = NULL,
                                        inflow_obs,
                                        working_directory,
                                        config,
                                        state_names) {
  if(config$model_settings$use_ler) {
    inflow_outflow_files <- create_ler_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                            inflow_obs = inflow_obs,
                                                            working_directory = working_directory,
                                                            config = config,
                                                            state_names = state_names)
  } else {
    inflow_outflow_files <- create_glm_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                            inflow_obs = inflow_obs,
                                                            working_directory = working_directory,
                                                            config = config,
                                                            state_names = state_names)
  }
  return(inflow_outflow_files)
}
