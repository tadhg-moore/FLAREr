

#### NEED A TEST HERE TO CHECK THAT MET FILES ARE GENERATED AND CORRECT
test_that("LER met files are generated", {

  template_folder <- system.file("data", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))
  config$use_ler <- TRUE

  met_out <- FLAREr::generate_met_files(obs_met_file = observed_met_file,
                                        out_dir = config$run_config$execute_location,
                                        forecast_dir = file.path(config$data_location, config$forecast_met_model),
                                        config)
  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 21))
})


#Inflow Drivers (already done)
test_that("LER inflow & outflow files are generated", {

  # library(tidyverse)
  template_folder <- system.file("data", package = "FLAREr")

  source(file.path(template_folder, "test_inflow_prep.R"))
  config$use_ler <- TRUE

  inflow_forecast_path <- file.path(config$data_location, config$forecast_inflow_model)

  #### NEED A TEST HERE TO CHECK THAT INFLOW FILES ARE GENERATED AND CORRECT
  inflow_outflow_files <- FLAREr::create_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                              inflow_obs = cleaned_inflow_file,
                                                              working_directory = config$run_config$execute_location,
                                                              config,
                                                              state_names = NULL)

  inflow_file_names <- inflow_outflow_files$inflow_file_name
  outflow_file_names <- inflow_outflow_files$outflow_file_name

  testthat::expect_equal(file.exists(inflow_outflow_files[[1]]), expected = rep(TRUE, 21))
  testthat::expect_equal(file.exists(inflow_outflow_files[[2]]), expected = rep(TRUE, 21))
})



#Create observation matrix
#### NEED A TEST HERE TO CHECK THAT OBS MATRIX IS GENERATED AND CORRECT
test_that("observation matrix is generated and correct", {

  # library(tidyverse)

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))

  obs_tmp <- read.csv(cleaned_observations_file_long)
  # obs_tmp$hour[which(obs_tmp$hour == 7)] <- 19
  write.csv(obs_tmp, cleaned_observations_file_long, row.names = FALSE, quote = FALSE)


  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)
  testthat::expect_true(is.array(obs))

  testthat::expect_true(any(!is.na(obs[1, , ])))

})



test_that("generate states to obs mapping", {

  # library(tidyverse)

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
  testthat::expect_true(is.data.frame(states_config))
})



test_that("initial model error is generated", {

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))

  config_file_location <- config$run_config$forecast_location

  model_sd <- FLAREr::initiate_model_error(config, states_config, config_file_location)
  testthat::expect_true(is.array(model_sd))
  testthat::expect_true(any(!is.na(model_sd)))
})


#Set initial conditions
test_that("LER-GLM initial conditions are generated", {

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))
  config$model <- "GLM"

  obs_tmp <- read.csv(cleaned_observations_file_long)
  # obs_tmp$hour[which(obs_tmp$hour == 7)] <- 19
  write.csv(obs_tmp, cleaned_observations_file_long, row.names = FALSE, quote = FALSE)

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)
  testthat::expect_true(is.list(init))
  chk <- lapply(init, is.array)
  testthat::expect_true(any(unlist(chk)))
})

#Set initial conditions
test_that("LER-GOTM initial conditions are generated", {

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))
  config$model <- "GOTM"

  obs_tmp <- read.csv(cleaned_observations_file_long)
  # obs_tmp$hour[which(obs_tmp$hour == 7)] <- 19
  write.csv(obs_tmp, cleaned_observations_file_long, row.names = FALSE, quote = FALSE)

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)
  testthat::expect_true(is.list(init))
  chk <- lapply(init, is.array)
  testthat::expect_true(any(unlist(chk)))
})

#Set initial conditions
test_that("LER-Simstrat initial conditions are generated", {

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_met_prep.R"))
  config$model <- "Simstrat"

  obs_tmp <- read.csv(cleaned_observations_file_long)
  # obs_tmp$hour[which(obs_tmp$hour == 7)] <- 19
  write.csv(obs_tmp, cleaned_observations_file_long, row.names = FALSE, quote = FALSE)

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                                 obs_config,
                                                 pars_config,
                                                 obs,
                                                 config)
  testthat::expect_true(is.list(init))
  chk <- lapply(init, is.array)
  testthat::expect_true(any(unlist(chk)))
})

# LER-GLM-EnKF Tests ----
# library(testthat)
test_that("LER-GLM-EnKF can be run", {

  # library(tidyverse)

  template_folder <- system.file("data", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_enkf_prep_ler.R"))
  config$model <- "GLM"

  # # Set up timings
  # start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_datetime), tz = config$local_tzone)
  # if(is.na(config$run_config$forecast_start_day_local)){
  #   end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  #   forecast_start_datetime_local <- end_datetime_local
  # }else{
  #   forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  #   end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
  # }
  #
  # #Set observations in the "future" to NA
  # full_time_forecast <- seq(as.POSIXct(config$run_config$start_datetime, tz= "UTC"), as.POSIXct(config$run_config$end_datetime, tz = "UTC"), by = "1 day")
  # obs[ , which(full_time_forecast > forecast_start_datetime_local), ] <- NA

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)

  states_init = init$states
  pars_init = init$pars
  aux_states_init = init$aux_states_init
  obs = obs
  obs_sd = obs_config$obs_sd
  model_sd = model_sd
  working_directory = config$run_config$execute_location
  met_file_names = (met_file_names)
  inflow_file_names = (inflow_file_names)
  outflow_file_names = (outflow_file_names)
  config = config
  pars_config = pars_config
  states_config = states_config
  obs_config = obs_config
  management = NULL
  da_method = "enkf"
  par_fit_method = "inflate"

  #Run EnKF

  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                         pars_init = init$pars,
                                         aux_states_init = init$aux_states_init,
                                         obs = obs,
                                         obs_sd = obs_config$obs_sd,
                                         model_sd = model_sd,
                                         working_directory = config$run_config$execute_location,
                                         met_file_names = (met_file_names),
                                         inflow_file_names = (inflow_file_names),
                                         outflow_file_names = (outflow_file_names),
                                         config = config,
                                         pars_config = pars_config,
                                         states_config = states_config,
                                         obs_config = obs_config
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data", "sampenkf_output_GLM.RDS"))
  samp_enkf_output <- readRDS(file.path(test_location, "sampenkf_output_GLM.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_location, config = config)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_eml(file_name = saved_file,
                          enkf_output)
  file_chk <- list.files(forecast_location, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                          qaqc_location = qaqc_data_location)
  file_chk <- list.files(forecast_location, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)


})

# LER-GOTM-EnKF Tests ----
test_that("LER-GOTM-EnKF can be run", {

  # library(tidyverse)

  template_folder <- system.file("data", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_enkf_prep_ler.R"))
  config$model <- "GOTM"

  #Set observations in the "future" to NA
  full_time_forecast <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  obs[ , which(full_time_forecast > forecast_start_datetime_local), ] <- NA

  init <- FLAREr::generate_initial_conditions(states_config,
                                                 obs_config,
                                                 pars_config,
                                                 obs,
                                                 config,
                                                 config$model)
  aux_states_init <- list()
  aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
  aux_states_init$avg_surf_temp <- init$avg_surf_temp
  aux_states_init$the_sals_init <- config$the_sals_init
  aux_states_init$mixing_vars <- init$mixing_vars
  aux_states_init$model_internal_depths <- init$model_internal_depths
  aux_states_init$lake_depth <- init$lake_depth
  aux_states_init$salt <- init$salt

  #Run EnKF
  # library(LakeEnsemblR); library(gotmtools)

  config$diagnostics_names <- NULL

  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                        pars_init = init$pars,
                                        aux_states_init = aux_states_init,
                                        obs = obs,
                                        obs_sd = obs_config$obs_sd,
                                        model_sd = model_sd,
                                        working_directory = config$run_config$execute_location,
                                        met_file_names = basename(met_file_names),
                                        inflow_file_names = as.matrix(basename(inflow_file_names)),
                                        outflow_file_names = basename(outflow_file_names),
                                        start_datetime = start_datetime_local,
                                        end_datetime = end_datetime_local,
                                        forecast_start_datetime = forecast_start_datetime_local,
                                        config = config,
                                        pars_config = pars_config,
                                        states_config = states_config,
                                        obs_config = obs_config,
                                        management = NULL,
                                        da_method = "enkf",
                                        par_fit_method = "inflate",
                                        use_ler = config$use_ler
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data", "sampenkf_output_GOTM.RDS"))
  samp_enkf_output <- readRDS(file.path(test_location, "sampenkf_output_GOTM.RDS"))
  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_location, config = config)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_eml(file_name = saved_file,
                          enkf_output)
  file_chk <- list.files(forecast_location, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                          qaqc_location = qaqc_data_location)
  file_chk <- list.files(forecast_location, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)


})

# LER-Simstrat-EnKF Tests ----
test_that("LER-Simstrat-EnKF can be run", {

  # library(tidyverse)

  template_folder <- system.file("data", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_location <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_location <- file.path(temp_dir, "data")

  source(file.path(test_location, "test_enkf_prep_ler.R"))
  config$model <- "Simstrat"

  #Set observations in the "future" to NA
  full_time_forecast <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  obs[ , which(full_time_forecast > forecast_start_datetime_local), ] <- NA

  init <- FLAREr::generate_initial_conditions(states_config,
                                                 obs_config,
                                                 pars_config,
                                                 obs,
                                                 config,
                                                 model = config$model)
  aux_states_init <- list()
  aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
  aux_states_init$avg_surf_temp <- init$avg_surf_temp
  aux_states_init$the_sals_init <- config$the_sals_init
  aux_states_init$mixing_vars <- init$mixing_vars
  aux_states_init$model_internal_depths <- init$model_internal_depths
  aux_states_init$lake_depth <- init$lake_depth
  aux_states_init$salt <- init$salt
  aux_states_init$U <- init$U
  aux_states_init$V <- init$V
  aux_states_init$k <- init$k
  aux_states_init$eps <- init$eps

  #Run EnKF
  # library(LakeEnsemblR); library(gotmtools)

  config$diagnostics_names <- NULL

  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                        pars_init = init$pars,
                                        aux_states_init = aux_states_init,
                                        obs = obs,
                                        obs_sd = obs_config$obs_sd,
                                        model_sd = model_sd,
                                        working_directory = config$run_config$execute_location,
                                        met_file_names = basename(met_file_names),
                                        inflow_file_names = as.matrix(basename(inflow_file_names)),
                                        outflow_file_names = basename(outflow_file_names),
                                        start_datetime = start_datetime_local,
                                        end_datetime = end_datetime_local,
                                        forecast_start_datetime = forecast_start_datetime_local,
                                        config = config,
                                        pars_config = pars_config,
                                        states_config = states_config,
                                        obs_config = obs_config,
                                        management = NULL,
                                        da_method = "enkf",
                                        par_fit_method = "inflate",
                                        use_ler = config$use_ler
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data", "sampenkf_output_Simstrat.RDS"))
  samp_enkf_output <- readRDS(file.path(test_location, "sampenkf_output_Simstrat.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_location, config = config)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_eml(file_name = saved_file,
                          enkf_output)
  file_chk <- list.files(forecast_location, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                          qaqc_location = qaqc_data_location)
  file_chk <- list.files(forecast_location, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)


})

# end
