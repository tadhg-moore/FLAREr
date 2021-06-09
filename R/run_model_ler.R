#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#' @importFrom LakeEnsemblR export_config
#' @importFrom gotmtools write_yaml
#' @importFrom yaml read_yaml
#'
#' @author Quinn Thomas
#'


run_model_ler <- function(model,
                          ler_yaml,
                      i,
                      m,
                      curr_start,
                      curr_stop,
                      par_names,
                      curr_pars,
                      working_directory,
                      par_file,
                      num_phytos,
                      model_depths_start,
                      lake_depth_start,
                      x_start,
                      full_time,
                      wq_start,
                      wq_end,
                      management = NULL,
                      hist_days,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_name,
                      outflow_file_name,
                      output_vars,
                      diagnostics_names,
                      npars,
                      num_wq_vars,
                      snow_ice_thickness_start,
                      salt_start,
                      nstates,
                      state_names,
                      include_wq,
                      restart,
                      restart_list){

  switch(config$model_settings$model_name,
         glm = { model <- "GLM" },
         gotm = { model <- "GOTM" },
         simstrat = { model <- "Simstrat"})

  if(is.null(management)){
    simulate_sss <- FALSE
  }else{
    simulate_sss <- management$simulate_sss
  }

  yml <- yaml::read_yaml(file.path(working_directory, ler_yaml))

  model_depths_end <- rep(NA,length(model_depths_start))

  model_depths_tmp <- model_depths_start[!is.na(model_depths_start)]
  model_depths_tmp_tmp <- c(model_depths_tmp, lake_depth_start)
  model_depths_mid <- model_depths_tmp_tmp[1:(length(model_depths_tmp_tmp)-1)] + diff(model_depths_tmp_tmp)/2

  the_sals <- approx(modeled_depths, salt_start, modeled_depths, rule = 2)$y

  # Initial temperature
  the_temps_enkf_tmp <- x_start[1:ndepths_modeled]

  the_temps <- approx(modeled_depths, the_temps_enkf_tmp, modeled_depths, rule = 2)$y


  init_prof <- data.frame(Depth_meter = round(modeled_depths, 4),
                          Water_Temperature_celsius = round(the_temps, 4))
  write.csv(init_prof, file.path(working_directory, "initial_profile.csv"),
            row.names = FALSE, quote = FALSE)


  # GLM ----
  if(model == "GLM") {
    mixing_vars_start = restart_list$mixing_vars[,i-1 , m]
    avg_surf_temp_start = restart_list$avg_surf_temp[i-1, m]

    yml$model_parameters$GLM$`init_profiles/restart_variables` <- mixing_vars_start

    # update_glm_nml_list <- list()
    update_aed_nml_list <- list()
    update_phyto_nml_list <- list()
    # update_glm_nml_names <- c()
    update_aed_nml_names <- c()
    update_phyto_nml_names <- c()
    # list_index <- 1
    list_index_aed <- 1
    list_index_phyto <- 1


    diagnostics <- array(NA, dim = c(length(diagnostics_names),ndepths_modeled))

    x_star_end <- rep(NA, nstates)

    if(npars > 0){

      unique_pars <- unique(par_names)

      for(par in 1:length(unique_pars)){

        curr_par_set <- which(par_names == unique_pars[par])
        curr_nml <- par_file[curr_par_set[1]]
        if(curr_nml == "glm3.nml"){
          yml$model_parameters$GLM[[unique_pars[par]]] <- round(curr_pars[curr_par_set], 4)
        }else if(curr_nml == "aed2.nml"){
          update_aed_nml_list[[list_index_aed]] <- round(curr_pars[curr_par_set], 4)
          update_aed_nml_names[list_index_aed] <- unique_pars[par]
          list_index_aed <- list_index_aed + 1
        }else if(curr_nml == "aed2_phyto_pars.nml"){
          update_phyto_nml_list[[list_index_phyto]] <- rep(round(curr_pars[curr_par_set],4), num_phytos)
          update_phyto_nml_names[list_index_phyto] <- unique_pars[par]
          list_index_phyto <- list_index_phyto + 1
        }
      }
    }

    if(include_wq){

      wq_init_vals <- c()

      for(wq in 1:num_wq_vars){
        wq_enkf_tmp <- x_start[wq_start[wq + 1]:wq_end[wq + 1]]
        wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
        wq_init_vals <- c(wq_init_vals,
                          approx(modeled_depths,wq_enkf_tmp, model_depths_mid, rule = 2)$y)
      }
      update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
      update_glm_nml_names[list_index] <- "wq_init_vals"
      list_index <- list_index + 1

      if(simulate_sss){
        if(is.na(management$specified_sss_inflow_file)){
          FLAREr:::create_sss_input_output(x = x_start,
                                          i,
                                          m,
                                          full_time,
                                          working_directory,
                                          wq_start,
                                          management$management_input,
                                          hist_days,
                                          management$forecast_sss_on,
                                          management$sss_depth,
                                          management$use_specified_sss,
                                          state_names,
                                          modeled_depths = modeled_depths,
                                          forecast_sss_flow = management$forecast_sss_flow,
                                          forecast_sss_oxy = management$forecast_sss_oxy,
                                          salt = salt_start)
        }else{
          file.copy(file.path(working_directory, management$specified_sss_inflow_file), paste0(working_directory,"/sss_inflow.csv"))
          if(!is.na(management$specified_sss_outflow_file)){
            file.copy(file.path(working_directory, management$specified_sss_outflow_file), paste0(working_directory,"/sss_outflow.csv"))
          }
        }
      }
    }


    # yml[["model_parameters"]][[model]][["the_temps"]] <- round(the_temps_glm, 4)
    # yml[["model_parameters"]][[model]][["the_sals"]] <- round(the_sals, 4)
    # yml[["model_parameters"]][[model]][["the_depths"]] <- round(model_depths_tmp, 4)
    # yml[["model_parameters"]][[model]][["num_depths"]] <- length(model_depths_tmp)
    yml[["model_parameters"]][[model]][["init_profiles/lake_depth"]] <- round(lake_depth_start, 4)
    yml[["model_parameters"]][[model]][["init_profiles/snow_thickness"]] <- 0
    yml[["model_parameters"]][[model]][["init_profiles/white_ice_thickness"]] <- round(snow_ice_thickness_start[2], 4)
    yml[["model_parameters"]][[model]][["init_profiles/blue_ice_thickness"]] <- round(snow_ice_thickness_start[3], 4)
    yml[["model_parameters"]][[model]][["init_profiles/avg_surf_temp"]] <- round(avg_surf_temp_start, 4)
  }

  # GOTM ----
  if( model == "GOTM") {

    diagnostics <- array(NA, dim = c(length(diagnostics_names), ndepths_modeled))

    x_star_end <- rep(NA, nstates)

    if(npars > 0){

      unique_pars <- unique(par_names)

      for(par in 1:length(unique_pars)){

        curr_par_set <- which(par_names == unique_pars[par])
        curr_nml <- par_file[curr_par_set[1]]
        yml[["model_parameters"]][[model]][[unique_pars[par]]] <- signif(curr_pars[curr_par_set], 4)

      }
    }

    if(restart) {
      yml$model_parameters$GOTM$`restart/load` <- TRUE
    } else {
      yml$model_parameters$GOTM$`restart/load` <- FALSE
    }
  }

  # Simstrat ----
  if( model == "Simstrat") {

    diagnostics <- array(NA, dim = c(length(diagnostics_names), ndepths_modeled))

    x_star_end <- rep(NA, nstates)

    if(npars > 0){

      unique_pars <- unique(par_names)

      for(par in 1:length(unique_pars)){

        curr_par_set <- which(par_names == unique_pars[par])
        curr_nml <- par_file[curr_par_set[1]]
        yml$model_parameters$Simstrat[[par_names[par]]] <- signif(curr_pars[curr_par_set], 4)

      }
    }
  }

  #ALLOWS THE LOOPING THROUGH NOAA ENSEMBLES
  yml$input$init_temp_profile$file <- "initial_profile.csv"
  yml$input$meteo$file <- paste0("../", basename(curr_met_file))
  yml$inflows$file <- paste0("../", basename(unlist(inflow_file_name)))

  yml$time$start <- curr_start
  yml$time$stop <- curr_stop

  gotmtools::write_yaml(yml, file.path(working_directory, ler_yaml))

  suppressMessages({
    LakeEnsemblR::export_config(config_file = ler_yaml, model = model, dirs = FALSE,
                                time = TRUE, location = TRUE, output_settings = TRUE,
                                meteo = T, init_cond = TRUE, extinction = FALSE,
                                inflow = T, # INFLOWS SWITCHED OFF!
                                model_parameters = TRUE,
                                folder = working_directory, print = FALSE)
  })

  if(model == "Simstrat") {
    # Input Simstrat restart values into initial condition file
    simstrat_init <- read.delim(file.path(working_directory, model, "init_cond.dat"))
    simstrat_init[, 2] <- signif(restart_list$U_restart[, i-1,m], 5)
    simstrat_init[, 3] <- signif(restart_list$V_restart[, i-1,m], 5)
    simstrat_init[, 5] <- signif(restart_list$k_restart[, i-1,m], 5)
    simstrat_init[, 6] <- signif(restart_list$eps_restart[, i-1,m], 5)
    colnames(simstrat_init) <- c("Depth [m]",	"U [m/s]",	"V [m/s]",	"T [deg C]",	"k [J/kg]",	"eps [W/kg]")
    vroom::vroom_write(simstrat_init, file.path(working_directory, "Simstrat", "init_cond.dat"), delim = "\t",
                       col_names = TRUE, quote = "none")
  }


  if(model == "GLM" & include_wq){
    FLAREr:::update_nml(update_aed_nml_list,
                       update_aed_nml_names,
                       working_directory,
                       "aed2.nml")
    FLAREr:::update_nml(update_phyto_nml_list,
                       update_phyto_nml_names,
                       working_directory,
                       "aed2_phyto_pars.nml")
  }


  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed.
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0
  setwd(working_directory)

  if(i == 2 & m == 1){
    file.copy(from = paste0(working_directory, "/", "LakeEnsemblR.yaml"), #GLM SPECIFIC
              to = paste0(working_directory, "/", "LakeEnsemblR_initial.yaml"),
              overwrite = TRUE) #GLM SPECIFIC
  }

  # From LakeEnsemblR::run_ensemble()
  time_step <- yml$output$time_step
  time_unit <- yml$output$time_unit
  if(time_unit == "second"){
    # Needed to create out_time vector
    time_unit <- "sec"
  }
  # Create output time vector
  out_time <- data.frame(datetime = seq.POSIXt(as.POSIXct(curr_start),
                                               as.POSIXct(curr_stop),
                                               by = paste(time_step, time_unit)))
  out_vars <- yml$output$variables

  # Create output folder
  dir.create(file.path(working_directory, model, "output"), showWarnings = FALSE)

  while(!pass) {

    #Delete previous output
    old_output <- list.files(file.path(working_directory, model, "output"))
    unlink(file.path(working_directory, model, "output", old_output), recursive = TRUE)

    model_states <- FLAREr:::run_models_ler(model = model,
                          folder = working_directory,
                          verbose = FALSE,
                          restart = restart,
                          member = m,
                          the_temps = the_temps,
                          model_depths = modeled_depths)

    fils <- list.files(file.path(working_directory, model, "output"))

    if(length(fils) != 0){ #&
       # !testit::has_error(nc <- ncdf4::nc_open(paste0(working_directory, "/output/output.nc")))){

      # if(length(ncvar_get(nc, "time")) >= 1){
      if(TRUE){
          # nc_close(nc)

        output_vars_multi_depth <- state_names
        output_vars_no_depth <- NA

        # LakeEnsemblR Output
        ler_temp_out <-  FLAREr:::get_ler_var_all(model = model,
                                                  working_dir = working_directory,
                                                  z_out = modeled_depths,
                                                  vars_depth = output_vars_multi_depth,
                                                  vars_no_depth = output_vars_no_depth,
                                                  diagnostic_vars = diagnostics_names,
                                                  ler_yaml = yml)


        num_model_depths <- length(ler_temp_out$depths_enkf)
        # temps <- rev(ler_temp_out$output[ ,1])
        temps <- (ler_temp_out$output[ ,1])
        model_depths_end[1:num_model_depths] <- ler_temp_out$depths_enkf

        model_depths_tmp <- c(ler_temp_out$depths_enkf, ler_temp_out$lake_depth)

        model_depths_mid <- model_depths_tmp[1:(length(model_depths_tmp)-1)] + diff(model_depths_tmp)/2


        x_star_end[1:ndepths_modeled] <- approx(model_depths_mid, temps,
                                                modeled_depths, rule = 2)$y
        # x_star_end[1:ndepths_modeled] <- approx(LER_temp_out$depths, LER_temp_out$temp,
        #                                         modeled_depths, rule = 2)$y

        salt_end <- approx(model_depths_mid, ler_temp_out$salt,
                           modeled_depths, rule = 2)$y #approx(modeled_depths, ler_temp_out$salt, modeled_depths, rule = 2)$y

        if(include_wq){
          for(wq in 1:num_wq_vars){
            glm_wq <-  rev(ler_temp_out$output[ ,1+wq])
            x_star_end[wq_start[1 + wq]:wq_end[1 + wq]] <- approx(model_depths_mid,glm_wq, modeled_depths, rule = 2)$y
          }
        }

        if(length(diagnostics_names) > 0){
          for(wq in 1:length(diagnostics_names)){
            glm_wq <-  rev(ler_temp_out$diagnostics_output[ , wq])
            diagnostics[wq , ] <- approx(model_depths_mid, glm_wq, modeled_depths, rule = 2)$y
          }
        } else {
          diagnostics <- rep(NA, length(modeled_depths))
        }

        if(length(which(is.na(x_star_end))) == 0){
          pass = TRUE
        }else{
          num_reruns <- num_reruns + 1
        }
      }else{
        num_reruns <- num_reruns + 1
      }
    }else{
      num_reruns <- num_reruns + 1
    }
    if(num_reruns > 1000){
      stop(paste0("Too many re-runs (> 1000) due to NaN values in output"))
    }

    return(list(x_star_end  = x_star_end,
                lake_depth_end  = ler_temp_out$lake_depth,
                snow_ice_thickness_end  = ler_temp_out$snow_wice_bice,
                restart_vars = ler_temp_out$restart_vars,
                avg_surf_temp_end  = ler_temp_out$avg_surf_temp,
                mixing_vars_end = ler_temp_out$mixing_vars,
                salt_end = salt_end,
                diagnostics_end  = diagnostics,
                model_internal_depths = model_depths_end))
  }
}
