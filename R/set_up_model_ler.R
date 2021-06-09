

set_up_model_ler <- function(config,
                             ens_working_directory,
                             state_names,
                             inflow_file_names,
                             outflow_file_names){

  oldwd <- getwd()
  setwd(ens_working_directory)
  on.exit({
    setwd(oldwd)
  })


  file.copy(from = file.path(config$file_path$execute_directory, config$model_settings$ler_yaml),
            to = file.path(ens_working_directory, "LakeEnsemblR.yaml"), overwrite = TRUE)
  yaml_file <- file.path(ens_working_directory, "LakeEnsemblR.yaml")
  ler_yaml <- yaml::read_yaml(yaml_file)
  # yml <- yaml::read_yaml(file.path(ens_working_directory, ler_yaml))
  ler_directory <- gsub(config$location$site_id, "", ens_working_directory)

  if(config$model_settings$model_name == "glm") {

    dir.create(file.path(ens_working_directory, "GLM"), showWarnings = FALSE)
    file.copy(from = file.path(config$file_path$configuration_directory, "forecast_model",
                               config$model_settings$model_name, config$model_settings$base_GLM_nml),
              to = file.path(ens_working_directory, "GLM", "glm3.nml"), overwrite = TRUE)

    non_temp_names <- state_names[which(!(state_names %in% "temp"))]
    if(length(non_temp_names) == 0) {
      non_temp_names <- NULL
    }

    inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)

    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = as.integer(0), key1 = "model_parameters", key2 = "GLM", key3 = "init_profiles/num_wq_vars")
    # ler_yaml <- gotmtools::set_yaml(ler_yaml, value = "''", key1 = "model_parameters", key2 = "GLM", key3 = "wq_names")
    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = ncol(inflow_file_names), key1 = "model_parameters", key2 = "GLM", key3 = "inflow/num_inflows")
    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = ncol(outflow_file_names), key1 = "model_parameters", key2 = "GLM", key3 = "outflow/num_outlet")
    # ler_yaml$model_parameters$GLM$the_depths <- config$modeled_depths
    # ler_yaml <- gotmtools::set_yaml(ler_yaml, value = length(config$modeled_depths), key1 = "model_parameters", key2 = "GLM", key3 = "init_profiles/num_depths")
    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = length(inflow_var_names), key1 = "model_parameters", key2 = "GLM", key3 = "inflow/inflow_varnum")
    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = inflow_var_names, key1 = "model_parameters", key2 = "GLM", key3 = "inflow/inflow_vars")
    ler_yaml <- gotmtools::set_yaml(ler_yaml, value = "'output'", key1 = "model_parameters", key2 = "GLM", key3 = "output/out_dir")

    gotmtools::write_yaml(ler_yaml, yaml_file)


    if(config$include_wq){

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_nml),
                to = paste0(ens_working_directory, "/", "aed2.nml"), overwrite = TRUE)

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_phyto_pars_nml),
                to = paste0(ens_working_directory, "/", "aed2_phyto_pars.nml"), overwrite = TRUE)

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_zoop_pars_nml),
                to = paste0(ens_working_directory, "/", "aed2_zoop_pars.nml"), overwrite = TRUE)

    }
  }

  switch(config$model_settings$model_name,
         glm = { model <- "GLM" },
         gotm = { model <- "GOTM" },
         simstrat = { model <- "Simstrat"})

    LakeEnsemblR::export_config(config_file = basename(yaml_file), model = model, dirs = TRUE,
                              time = FALSE, location = TRUE, output_settings = TRUE,
                              meteo = FALSE, init_cond = FALSE, extinction = TRUE,
                              inflow = FALSE, model_parameters = TRUE,
                              folder = ens_working_directory)


}
