#' Function to score new data and predict change for each single currency pair
#'
#' @description  Function is using the latest data from the financial assets indicator pattern and deep learning model.
#' Prediction is a price change in the future for that asset will be used by the trading system
#'
#' `r lifecycle::badge('stable')`
#'
#' @details Performs fresh data reading from the rds file
#'
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @param symbol              Character symbol of the asset for which the model shall predict
#' @param timeframe           Data timeframe e.g. 60 min
#' @param path_model          Path where the models are be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#' @param path_sbxm           Path to the sandbox where file with predicted price should be written (master terminal)
#' @param path_sbxs           Path to the sandbox where file with predicted price should be written (slave terminal)
#'
#' @return Function is writing file into Decision Support System folder, mainly file with price change prediction in pips
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # test of function aml_make_model is duplicated here
#' library(dplyr)
#' library(readr)
#' library(lubridate)
#' library(h2o)
#' library(magrittr)
#' library(lazytrade)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' ind = system.file("extdata", "AI_RSIADXUSDJPY60.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' ind$X1 <- ymd_hms(ind$X1)
#'
#' write_csv(ind, file.path(path_data, "AI_RSIADXUSDJPY60.csv"), col_names = FALSE)
#'
#'
#' # data transformation using the custom function for one symbol
#' aml_collect_data(indicator_dataset = ind,
#'                  symbol = 'USDJPY',
#'                  timeframe = 60,
#'                  path_data = path_data)
#' # start h2o engine (using all CPU's by default)
#'
#' h2o.init(nthreads = 2)
#'
#'
#' # performing Deep Learning Regression using the custom function
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=FALSE,
#'                num_nn_options = 2)
#'
#'
#' path_sbxm <- normalizePath(tempdir(),winslash = "/")
#' path_sbxs <- normalizePath(tempdir(),winslash = "/")
#'
#'
#' # score the latest data to generate predictions for one currency pair
#' aml_score_data(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                path_sbxm = path_sbxm,
#'                path_sbxs = path_sbxs)
#'
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
aml_score_data <- function(symbol,
                           timeframe,
                           path_model,
                           path_data,
                           path_sbxm,
                           path_sbxs){


  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  #construct the path to the data object see function aml_collect_data.R
  # generate a file name for latest dataset
  f_name <- paste0("AI_RSIADX", symbol,timeframe, ".csv")
  full_path <- file.path(path_data,  f_name)

  x <- readr::read_csv(full_path, col_names = F)
  #get latest data to predict
  x1 <- head(x, 1)[, -c(1:3)]

  # generate a file name for model
  m_name <- paste0("DL_Regression", "-", symbol,"-", timeframe)
  m_path <- file.path(path_model, m_name)
  #load model
  ModelR <- h2o::h2o.loadModel(path = m_path)

  # uploading data to h2o
  recent_ML  <- h2o::as.h2o(x = x1, destination_frame = "recent_ML")
  # PREDICT the next period...
  result_R <- h2o::h2o.predict(ModelR, recent_ML) %>% as.data.frame()

    ### Applying prediction by writing files
  # Rename the row and column names
  rownames(result_R) <- symbol
  names(result_R) <- symbol

  # write files with predictions
  file_string <- paste0("AI_M", timeframe, "_Change", symbol, ".csv")
  readr::write_csv(result_R, file.path(path_sbxm, file_string))
  readr::write_csv(result_R, file.path(path_sbxs, file_string))


  #h2o.shutdown(prompt = FALSE)




}

