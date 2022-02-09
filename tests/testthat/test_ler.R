test_that("LER met files are generated", {

  template_folder <- system.file("example", package = "FLAREr")

  source(file.path(template_folder, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE
  config$model_settings$model <- "GLM"

  met_out <- FLAREr::generate_met_files(obs_met_file = obs_met_file,
                                       out_dir = config$file_path$execute_directory,
                                       forecast_dir = config$file_path$noaa_directory,
                                       config)

  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 21))
})


test_that("LER inflow & outflow files are generated", {

  template_folder <- system.file("example", package = "FLAREr")
  source(file.path(template_folder, "R/test_inflow_prep.R"))
  inflow_file_dir <- config$file_path$inflow_directory

  config$model_settings$use_ler <- TRUE
  config$model_settings$model <- "GLM"

  inflow_outflow_files <- FLAREr::create_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                              inflow_obs = cleaned_inflow_file,
                                                              working_directory = config$file_path$execute_directory,
                                                              config,
                                                              state_names = NULL)

  inflow_file_names <- inflow_outflow_files$inflow_file_name
  outflow_file_names <- inflow_outflow_files$outflow_file_name

  testthat::expect_equal(file.exists(inflow_outflow_files[[1]]), expected = rep(TRUE, 21))
  testthat::expect_equal(file.exists(inflow_outflow_files[[2]]), expected = rep(TRUE, 21))
})

#Set initial conditions
test_that("LER-GLM initial conditions are generated", {

  template_folder <- system.file("example", package = "FLAREr")
  source(file.path(template_folder, "R/test_met_prep_ler.R"))

  config$model_settings$model <- "GLM"

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

  template_folder <- system.file("example", package = "FLAREr")
  source(file.path(template_folder, "R/test_met_prep_ler.R"))

  config$model_settings$model <- "GOTM"

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
# test_that("LER-Simstrat initial conditions are generated", {
#
#   template_folder <- system.file("example", package = "FLAREr")
#   source(file.path(template_folder, "R/test_met_prep_ler.R"))
#
#   config$model_settings$model <- "Simstrat"
#
#   obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
#                                    obs_config,
#                                    config)
#
#   init <- FLAREr::generate_initial_conditions(states_config,
#                                               obs_config,
#                                               pars_config,
#                                               obs,
#                                               config)
#
#   testthat::expect_true(is.list(init))
#   chk <- lapply(init, is.array)
#   testthat::expect_true(any(unlist(chk)))
# })

# LER-GLM-EnKF Tests ----
test_that("LER-GLM-EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")

  source(file.path(template_folder, "R", "test_enkf_prep_ler.R"))
  config$model_settings$model <- "GLM"

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)


  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$file_path$execute_directory
  # met_file_names = (met_file_names)
  # inflow_file_names = (inflow_file_names)
  # outflow_file_names = (outflow_file_names)
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # management = NULL
  # da_method = "enkf"
  # par_fit_method = "inflate"
  # debug = FALSE

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast_all(states_init = init$states,
                                         pars_init = init$pars,
                                         aux_states_init = init$aux_states_init,
                                         obs = obs,
                                         obs_sd = obs_config$obs_sd,
                                         model_sd = model_sd,
                                         working_directory = config$file_path$execute_directory,
                                         met_file_names = met_file_names,
                                         inflow_file_names = inflow_file_names,
                                         outflow_file_names = outflow_file_names,
                                         config = config,
                                         pars_config = pars_config,
                                         states_config = states_config,
                                         obs_config = obs_config,
                                         management = NULL,
                                         da_method = "enkf",
                                         par_fit_method = "inflate",
                                         debug = FALSE
  )

  # saveRDS(object = enkf_output, file = "inst/example/benchmark_data/sampenkf_output_GLM.RDS")
  samp_enkf_output <- readRDS(file.path(template_folder, "benchmark_data", "sampenkf_output_GLM.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = enkf_output,
                                              forecast_output_directory = config$file_path$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                da_forecast_output = enkf_output)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_data_directory = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)
})

# LER-GOTM-EnKF Tests ----
test_that("LER-GOTM-EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")

  source(file.path(template_folder, "R", "test_enkf_prep_ler.R"))
  config$model_settings$model <- "GOTM"

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)


  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$file_path$execute_directory
  # met_file_names = (met_file_names)
  # inflow_file_names = (inflow_file_names)
  # outflow_file_names = (outflow_file_names)
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # management = NULL
  # da_method = "enkf"
  # par_fit_method = "inflate"
  # debug = FALSE

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast_all(states_init = init$states,
                                             pars_init = init$pars,
                                             aux_states_init = init$aux_states_init,
                                             obs = obs,
                                             obs_sd = obs_config$obs_sd,
                                             model_sd = model_sd,
                                             working_directory = config$file_path$execute_directory,
                                             met_file_names = met_file_names,
                                             inflow_file_names = inflow_file_names,
                                             outflow_file_names = outflow_file_names,
                                             config = config,
                                             pars_config = pars_config,
                                             states_config = states_config,
                                             obs_config = obs_config,
                                             management = NULL,
                                             da_method = "enkf",
                                             par_fit_method = "inflate",
                                             debug = FALSE
  )

  # saveRDS(object = enkf_output, file = "inst/example/benchmark_data/sampenkf_output_GOTM.RDS")
  samp_enkf_output <- readRDS(file.path(template_folder, "benchmark_data", "sampenkf_output_GOTM.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = enkf_output,
                                              forecast_output_directory = config$file_path$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                da_forecast_output = enkf_output)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_data_directory = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)
})

# LER-Simstrat-EnKF Tests ----
# test_that("LER-Simstrat-EnKF can be run", {
#
#   template_folder <- system.file("example", package = "FLAREr")
#
#   source(file.path(template_folder, "R", "test_enkf_prep_ler.R"))
#   config$model_settings$model <- "Simstrat"
#
#   obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
#                                    obs_config,
#                                    config)
#
#   init <- FLAREr::generate_initial_conditions(states_config,
#                                               obs_config,
#                                               pars_config,
#                                               obs,
#                                               config)
#
#
#   # states_init = init$states
#   # pars_init = init$pars
#   # aux_states_init = init$aux_states_init
#   # obs = obs
#   # obs_sd = obs_config$obs_sd
#   # model_sd = model_sd
#   # working_directory = config$file_path$execute_directory
#   # met_file_names = (met_file_names)
#   # inflow_file_names = (inflow_file_names)
#   # outflow_file_names = (outflow_file_names)
#   # config = config
#   # pars_config = pars_config
#   # states_config = states_config
#   # obs_config = obs_config
#   # management = NULL
#   # da_method = "enkf"
#   # par_fit_method = "inflate"
#   # debug = FALSE
#
#   #Run EnKF
#   enkf_output <- FLAREr::run_da_forecast_all(states_init = init$states,
#                                              pars_init = init$pars,
#                                              aux_states_init = init$aux_states_init,
#                                              obs = obs,
#                                              obs_sd = obs_config$obs_sd,
#                                              model_sd = model_sd,
#                                              working_directory = config$file_path$execute_directory,
#                                              met_file_names = met_file_names,
#                                              inflow_file_names = inflow_file_names,
#                                              outflow_file_names = outflow_file_names,
#                                              config = config,
#                                              pars_config = pars_config,
#                                              states_config = states_config,
#                                              obs_config = obs_config,
#                                              management = NULL,
#                                              da_method = "enkf",
#                                              par_fit_method = "inflate",
#                                              debug = FALSE
#   )
#
#   # saveRDS(object = enkf_output, file = "inst/example/benchmark_data/sampenkf_output_Simstrat.RDS")
#   samp_enkf_output <- readRDS(file.path(template_folder, "benchmark_data", "sampenkf_output_Simstrat.RDS"))
#
#   testthat::expect_true(is.list(enkf_output))
#   chk <- lapply(1:length(enkf_output), function(x) {
#     class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
#   })
#
#   testthat::expect_true(all(unlist(chk)))
#
#   # Save forecast
#   saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = enkf_output,
#                                               forecast_output_directory = config$file_path$forecast_output_directory)
#   testthat::expect_true(file.exists(saved_file))
#
#   #Create EML Metadata
#   FLAREr::create_flare_metadata(file_name = saved_file,
#                                 da_forecast_output = enkf_output)
#   file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
#   testthat::expect_true(length(file_chk) > 0)
#
#   FLAREr::plotting_general(file_name = saved_file,
#                            qaqc_data_directory = config$file_path$qaqc_data_directory)
#   file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
#   testthat::expect_true(length(file_chk) > 0)
# })

# end
