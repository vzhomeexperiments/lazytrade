#' Function to simulate multiple input structures
#'
#' @description Function is designed to evaluate several different inputs.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @details  Function is using several other functions to perform sets of operations
#' designed to test several inputs.
#'
#' @author (C) 2021 Vladimir Zhbanko
#'
#' @param timeframe           Integer, Data timeframe e.g. 60 min. This will be equal to 1 bar
#' @param path_sim_input      String, Path to the folder where csv files will be placed, typically AI_RSIADXAUDCAD60.csv
#' @param path_sim_result     String, Path to the folder where all results from simulations shall be written
#' @param par_simulate        Integer, Parameter that can be used in simulation
#' @param demo_mode           Boolean, Simplify function test. When TRUE no simulation will be made
#'
#' @return Function is writing file into Decision Support System folders
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' library(dplyr)
#' library(magrittr)
#' library(readr)
#' library(h2o)
#' library(lazytrade)
#' library(lubridate)
#' library(stats)
#'
#' path_input <- normalizePath(tempdir(),winslash = "/")
#' path_sim_input <- file.path(path_input, "path_sim_input")
#' dir.create(path_sim_input)
#' path_sim_result <- file.path(path_input, "path_sim_result")
#' dir.create(path_sim_result)
#'
#' file.copy(from = system.file("extdata", "AI_RSIADXCADCHF60.csv", package = "lazytrade"),
#'           to = file.path(path_sim_input, "AI_RSIADXCADCHF60.csv"), overwrite = TRUE)
#' file.copy(from = system.file("extdata", "AI_RSIADXEURNZD60.csv", package = "lazytrade"),
#'           to = file.path(path_sim_input, "AI_RSIADXEURNZD60.csv"), overwrite = TRUE)
#'
#' # start h2o engine
#' h2o.init(nthreads = 2)
#'
#' # simulation with random structures
#' aml_simulation(timeframe = 60, path_sim_input = path_sim_input,
#'                path_sim_result = path_sim_result,
#'                par_simulate = 3,
#'                demo_mode = TRUE)
#'
#' Sys.sleep(5)
#' # stop h2o engine
#' h2o.shutdown(prompt = FALSE)
#'
#' #set delay to insure h2o unit closes properly before the next test
#' Sys.sleep(5)
#'
#' }
#'
#'
#'
aml_simulation <- function(timeframe = 60, path_sim_input, path_sim_result,
                           par_simulate = 16, demo_mode = FALSE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  # Function should perform simulation for one set of Neural Network Inputs
  ## Generate subfolders needed for data storage and processing for one set
  #path with the data where rds files will be recorded
  path_sim_data <- file.path(path_sim_input, "_DATA")
  if(!dir.exists(path_sim_data)){dir.create(path_sim_data)}
  #path with models
  path_sim_models <- file.path(path_sim_input, "_MODELS")
  if(!dir.exists(path_sim_models)){dir.create(path_sim_models)}
  # 2 dummy paths to drop str test results
  path_sim_sbxm <- file.path(path_sim_input, "_SBXM")
  if(!dir.exists(path_sim_sbxm)){dir.create(path_sim_sbxm)}
  path_sim_sbxs <- file.path(path_sim_input, "_SBXS")
  if(!dir.exists(path_sim_sbxs)){dir.create(path_sim_sbxs)}
  # =================================
  # collect data
  # =================================
  #### Read inputs ==========================================
  # read files for which symbols are actually placed to the folder path_sim_input
  myFiles <- list.files(path_sim_input,pattern = "AI_RSIADX", all.files = TRUE)
  mySymbols <- stringr::str_remove(myFiles, pattern = "AI_RSIADX")
  mySymbols <- stringr::str_remove(mySymbols, pattern = as.character(timeframe))
  mySymbols <- stringr::str_remove(mySymbols, pattern = ".csv")

  #time frames used
  timeframeHP <- timeframe



  # Writing indicator and price change to the file
  for (PAIR in mySymbols) {
    # PAIR <- mySymbols[1]
    # performing data collection
    indHP = file.path(path_sim_input, paste0("AI_RSIADX",PAIR,timeframeHP,".csv")) %>%
      readr::read_csv(col_names = FALSE)

    indHP$X1 <- lubridate::ymd_hms(indHP$X1)

    # data transformation using the custom function for one symbol
    lazytrade::aml_collect_data(indicator_dataset = indHP,
                                symbol = PAIR,
                                timeframe = timeframeHP,
                                path_data = path_sim_data,
                                max_nrows = 15000)

    #full_path <- file.path(path_data, 'AI_RSIADXEURUSD60.rds')

    #x1 <- read_rds(full_path)

  }

  if(!demo_mode){
  # =================================
  # force model update
  # =================================
  #path to store logs data (e.g. duration of machine learning steps)
  #h2o.init()

  # Writing indicator and price change to the file
  for (PAIR in mySymbols) {
    ## PAIR <- mySymbols[1]
    # performing Deep Learning Regression using the custom function
    lazytrade::aml_make_model(symbol = PAIR,
                              timeframe = timeframeHP,
                              path_model = path_sim_models,
                              path_data = path_sim_data,
                              force_update = TRUE,
                              num_nn_options = 12,
                              num_bars_test = 600,
                              num_bars_ahead = 34,
                              num_cols_used = par_simulate)

  }
  # =================================
  # test build test...
  # =================================

  #copy file with tick size info
  tick = system.file("extdata", "TickSize_AI_RSIADX.csv",
                     package = "lazytrade") %>% read_csv(col_names = FALSE)
  write_csv(tick, file.path(path_sim_data, "TickSize_AI_RSIADX.csv"), col_names = FALSE)



  # Performing Testing => Building -> Testing...
  for (PAIR in mySymbols) {
    ## PAIR <- mySymbols[1]

    # repeat testing and training several times

    lazytrade::aml_test_model(symbol = PAIR,
                              num_bars = 600,
                              timeframe = timeframeHP,
                              path_model = path_sim_models,
                              path_data = path_sim_data,
                              path_sbxm = path_sim_sbxm,
                              path_sbxs = path_sim_sbxs)

  }

  perf <- lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                                  used_symbols = mySymbols,
                                  path_model = path_sim_models,
                                  path_sbxm = path_sim_sbxm,
                                  path_sbxs = path_sim_sbxs,
                                  min_quality = 0.8,
                                  get_quantile = TRUE)
  # function to write log to the _LOG folder
  lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                                     used_symbols = mySymbols,
                                     path_model = path_sim_models,
                                     path_sbxm = path_sim_sbxm,
                                     path_sbxs = path_sim_sbxs,
                                     min_quality = 0.75,
                                     get_quantile = FALSE,
                                     log_results = TRUE,
                                     path_logs = path_sim_result)



  for (PAIR in mySymbols) {
    lazytrade::aml_make_model(symbol = PAIR,
                   timeframe = timeframeHP,
                   path_model = path_sim_models,
                   path_data = path_sim_data,
                   force_update=FALSE,
                   min_perf = perf,
                   num_nn_options = 12,
                   num_bars_test = 600,
                   num_bars_ahead = 34,
                   num_cols_used = par_simulate)

    lazytrade::aml_test_model(symbol = PAIR,
                   num_bars = 600,
                   timeframe = timeframeHP,
                   path_model = path_sim_models,
                   path_data = path_sim_data,
                   path_sbxm = path_sim_sbxm,
                   path_sbxs = path_sim_sbxs)

  }

  perf <- lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                                  used_symbols = mySymbols,
                                  path_model = path_sim_models,
                                  path_sbxm = path_sim_sbxm,
                                  path_sbxs = path_sim_sbxs,
                                  min_quality = 0.5,
                                  get_quantile = TRUE)
  # function to write log to the _LOG folder
  lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                                     used_symbols = mySymbols,
                                     path_model = path_sim_models,
                                     path_sbxm = path_sim_sbxm,
                                     path_sbxs = path_sim_sbxs,
                                     min_quality = 0.75,
                                     get_quantile = FALSE,
                                     log_results = TRUE,
                                     path_logs = path_sim_result)



  for (PAIR in mySymbols) {
    lazytrade::aml_make_model(symbol = PAIR,
                   timeframe = timeframeHP,
                   path_model = path_sim_models,
                   path_data = path_sim_data,
                   force_update=FALSE,
                   num_nn_options = 12,
                   num_bars_test = 600,
                   num_bars_ahead = 34,
                   num_cols_used = par_simulate,
                   min_perf = perf)

    lazytrade::aml_test_model(symbol = PAIR,
                   num_bars = 600,
                   timeframe = timeframeHP,
                   path_model = path_sim_models,
                   path_data = path_sim_data,
                   path_sbxm = path_sim_sbxm,
                   path_sbxs = path_sim_sbxs)

  }

  # stop h2o engine
  #h2o.shutdown(prompt = F)



  AverPerf <- lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                                             used_symbols = mySymbols,
                                             path_model = path_sim_models,
                                             path_sbxm = path_sim_sbxm,
                                             path_sbxs = path_sim_sbxs,
                                             min_quality = 0.5,
                                             get_quantile = TRUE)

  # function to write log to the _LOG folder
  Qntil = lazytrade::aml_consolidate_results(timeframe = timeframeHP,
                          used_symbols = mySymbols,
                          path_model = path_sim_models,
                          path_sbxm = path_sim_sbxm,
                          path_sbxs = path_sim_sbxs,
                          min_quality = 0.75,
                          get_quantile = FALSE,
                          log_results = TRUE,
                          path_logs = path_sim_result)

  #setup a log dataframe to consolidate results of particular sets
  logs <- data.frame(TimeTest = Sys.time(),
                     Folder = path_sim_input,
                     MeanPerf = AverPerf, HighPerf = Qntil$Quantil)

  #read existing log (if exists) and add there a new log data
  if(!file.exists(file.path(path_sim_result, 'all_results.rds'))){
    write_rds(logs, file.path(path_sim_result, 'all_results.rds'))
  } else {
    read_rds(file.path(path_sim_result, 'all_results.rds')) %>%
      bind_rows(logs) %>%
      write_rds(file.path(path_sim_result, 'all_results.rds'))
  }

  } #end of test bypass with demo_mode


}


