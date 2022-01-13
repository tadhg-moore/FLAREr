#' @title Convert historical meteorology and NOAA forecasts to model specified format
#' @details Function combines historical meteorology and NOAA forecasts to create meteorology input files in the designated model format.  A file is generated for each ensemble member.
#' @param obs_met_file string; full path to netcdf that is observed historical meteorology
#' @param out_dir string; full path to directory where the converted files will be saved
#' @param forecast_dir string; full path to directory with the NOAA forecast netcdf files
#' @return list; vector of full path for the converted files and boolean flag if issues with historical meteorology files
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
  if(config$model_settings$use_ler) {
    met_out <- generate_ler_met_files(obs_met_file = obs_met_file,
                                      out_dir = out_dir,
                                      forecast_dir = config$file_path$noaa_directory,
                                      config = config)
  } else {
    met_out <- generate_glm_met_files(obs_met_file = obs_met_file,
                                      out_dir = out_dir,
                                      forecast_dir = config$file_path$noaa_directory,
                                      config = config)
  }
  return(met_out)
}
