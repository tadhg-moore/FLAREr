# Met files ----
test_that("met files are generated", {

  template_folder <- system.file("example", package = "FLAREr")

  source(file.path(template_folder, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE

  met_out <- FLAREr::generate_met_files(obs_met_file = observed_met_file,
                                            out_dir = config$file_path$execute_directory,
                                            forecast_dir = config$file_path$noaa_directory,
                                            config)
  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 21))
})


#Inflow files ----
test_that("LER inflow & outflow files are generated", {

  template_folder <- system.file("example", package = "FLAREr")

  source(file.path(template_folder, "R/test_inflow_prep.R"))
  config$model_settings$use_ler <- TRUE

  inflow_outflow_files <- FLAREr::create_inflow_outflow_files(inflow_file_dir = config$file_path$inflow_directory,
                                                              inflow_obs = cleaned_inflow_file,
                                                              working_directory = config$file_path$execute_directory,
                                                              config,
                                                              state_names = NULL)

  inflow_file_names <- inflow_outflow_files$inflow_file_name
  outflow_file_names <- inflow_outflow_files$outflow_file_name

  testthat::expect_equal(file.exists(inflow_outflow_files[[1]]), expected = rep(TRUE, 21))
  testthat::expect_equal(file.exists(inflow_outflow_files[[2]]), expected = rep(TRUE, 21))
})



# Create observation matrix ----
test_that("observation matrix is generated and correct", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\example"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)
  testthat::expect_true(is.array(obs))

  testthat::expect_true(any(!is.na(obs[1, , ])))

})

test_that("generate states to obs mapping", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
  testthat::expect_true(is.data.frame(states_config))
})



test_that("initial model error is generated", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\example"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE

  model_sd <- FLAREr::initiate_model_error(config, states_config)
  testthat::expect_true(is.array(model_sd))
  testthat::expect_true(any(!is.na(model_sd)))
})


#Set GLM initial conditions ----
test_that("LER-GLM initial conditions are generated", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\example"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE
  config$model_settings$model_name <- "glm"

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)
  testthat::expect_true(is.list(init))
  testthat::expect_true(is.array(init$states))
  testthat::expect_true(is.array(init$pars))
  testthat::expect_true(is.list(init$aux_states_init))
})

# LER-GLM-EnKF Tests ----
test_that("LER-GLM-EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  test_directory <- file.path(temp_dir, "example")

  mod <- "glm"
  source(file.path(test_directory, "R/test_enkf_prep_ler.R"))
  config$model_settings$ncore <- 2

  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$file_path$execute_directory
  # met_file_names = met_file_names
  # inflow_file_names = inflow_file_names
  # outflow_file_names = outflow_file_names
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # management = NULL
  # da_method = "enkf"
  # par_fit_method = "inflate"

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
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
                                         obs_config = obs_config
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("inst/example/benchmark_data/", "enkf_output_glm.RDS"))
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output_ler.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                              forecast_location = config$file_path$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                enkf_output)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)
})

# Set GOTM initial conditions ----
test_that("LER-GLM initial conditions are generated", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\example"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE
  config$model_settings$model_name <- "gotm"

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)
  testthat::expect_true(is.list(init))
  testthat::expect_true(is.array(init$states))
  testthat::expect_true(is.array(init$pars))
  testthat::expect_true(is.list(init$aux_states_init))
})

# LER-GOTM-EnKF Tests ----
test_that("LER-GOTM-EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  test_directory <- file.path(temp_dir, "example")

  mod <- "gotm"
  source(file.path(test_directory, "R/test_enkf_prep_ler.R"))
  config$model_settings$ncore <- 2
  config$output_settings$diagnostics_names <- NULL

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
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
                                         obs_config = obs_config
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("inst/example/benchmark_data/", "enkf_output_gotm.RDS"))
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output_gotm.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                              forecast_location = config$file_path$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                enkf_output)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)
})

# Set Simstrat initial conditions ----
test_that("LER-GLM initial conditions are generated", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\example"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))
  config$model_settings$use_ler <- TRUE
  config$model_settings$model_name <- "simstrat"

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)
  testthat::expect_true(is.list(init))
  testthat::expect_true(is.array(init$states))
  testthat::expect_true(is.array(init$pars))
  testthat::expect_true(is.list(init$aux_states_init))
})

# LER-Simstrat-EnKF Tests ----
test_that("LER-Simstrat-EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  test_directory <- file.path(temp_dir, "example")

  mod <- "simstrat"
  source(file.path(test_directory, "R/test_enkf_prep_ler.R"))
  config$model_settings$ncore <- 2
  config$output_settings$diagnostics_names <- NULL

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
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
                                         obs_config = obs_config
  )

  #Load in pre-prepared output
  # saveRDS(enkf_output, file.path("inst/example/benchmark_data/", "enkf_output_glm.RDS"))
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output_ler.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])
  })

  testthat::expect_true(all(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                              forecast_location = config$file_path$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                enkf_output)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$file_path$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)
})

# end
