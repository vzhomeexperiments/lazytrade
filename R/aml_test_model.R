#' Function to test the model and conditionally decide to update existing model for a single currency pair
#'
#' @description Function is designed to test of the trading decision based on the deep learning regression model.
#' The outcome of this function will be used to perform update of existing model with a fresh data.
#'
#' @details  Function is reading shifted price data and corresponding indicator.
#' Starting from the trained model function will test the trading strategy using simplified trading approach.
#' Trading approach will entail using the last available indicator data, predict the price change for every row,
#' verify obtained results on the available data after defined period of time (75 bars).
#' Obtained virtual win/loss is consolidated to calculate profit factor.
#' Whenever profit factor value is less than 1.1 function is writing dedicated decision using simple *.csv file
#' Such file will be used in the production script MakeModelMxx.R see repository R_selflearning
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @param symbol              Character symbol of the asset for which to train the model
#' @param num_bars            Number of bars used to detect pattern
#' @param timeframe           Data timeframe e.g. 1 min
#' @param path_model          Path where the models are be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#'
#' @return Function is writing file into Decision Support System folder
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' # start h2o engine (using all CPU's by default)
#' library(tidyverse)
#' library(h2o)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' #path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_MODELS"
#' path_data <- normalizePath(tempdir(),winslash = "/")
#' #path_data <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_DATA"
#' data(EURUSDM15X75)
#' write_rds(EURUSDM15X75, file.path(path_data, 'EURUSDM15X75.rds'))
#'
#' h2o.init()
#'
#' # performing Deep Learning Regression using the custom function
#' aml_test_model(symbol = 'EURUSD',
#'                num_bars = 75,
#'                timeframe = 15,
#'                path_model = path_model,
#'                path_data = path_data)
#'
#' # stop h2o engine
#' h2o.shutdown(prompt = F)
#'
#' }
#'
#'
#'
aml_test_model <- function(symbol, num_bars, timeframe, path_model, path_data){

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  #construct the path to the data object see function aml_collect_data.R
  # generate a file name to be able to read the right dataset
  f_name <- paste0(symbol, "M",timeframe,"X",num_bars, ".rds")
  full_path <- file.path(path_data,  f_name)

  x <- read_rds(full_path)

  # generate a file name for model
  m_name <- paste0("DL_Regression", "-", symbol,"-", num_bars, "-", timeframe)
  m_path <- file.path(path_model, m_name)
  #load model
  ModelR <- h2o.loadModel(path = m_path)

  # split data to train and test blocks
  # note: model will be tested on the PAST data and trained on the NEWEST data
  test_ind  <- 1:round(0.3*(nrow(x))) #train indices 1:xxx
  dat21 <- x[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- x[-test_ind, ]   #dataset to train the model


  # uploading data to h2o
  recent_ML  <- as.h2o(x = x, destination_frame = "recent_ML")
  # PREDICT the next period...
  result_R <- h2o.predict(ModelR, recent_ML) %>% as.data.frame()

  ## Checking the trading strategy assuming we open and hold position for 75 bars!
  dat31 <- x %>%
    # select only original value of the price change
    select(LABEL) %>%
    # add column with predicted price change
    bind_cols(result_R) %>%
    # lag column 'predict' to 75 periods, column P_lag will match corresponding real price in the column 'LABEL'
    mutate(predict = lag(predict, 75)) %>%
    # omit na's
    na.omit() %>%
    # create a risk column, use 10 pips as a trigger
    mutate(Risk = if_else(predict > 10, 1, if_else(predict < -10, -1, 0))) %>%
    # calculate expected outcome of risking the 'Risk': trade according to prediction
    mutate(ExpectedGain = predict*Risk) %>%
    # calculate 'real' gain or loss. LABEL is how the price moved (ground truth) so the column will be real outcome
    mutate(AchievedGain = LABEL*Risk) %>%
    # to account on spread
    mutate(Spread = if_else(AchievedGain > 0, - 5, if_else(AchievedGain < 0, -5, 0))) %>%
    # calculate 'net' gain
    mutate(NetGain = AchievedGain + Spread) %>%
    # remove zero values to calculate presumed number of trades
    filter(AchievedGain != 0) %>%
    # get the sum of both columns
    # Column Expected PNL would be the result in case all trades would be successful
    # Column Achieved PNL is the results achieved in reality
    summarise(ExpectedPnL = sum(ExpectedGain),
              AchievedPnL = sum(NetGain),
              TotalTrades = n(),
              TPSL_Level = 10) %>%
    # interpret the results
    mutate(FinalOutcome = if_else(AchievedPnL > 0, "VeryGood", "VeryBad"),
           FinalQuality = AchievedPnL/(0.0001+ExpectedPnL))


   ## write condition to the csv file
  dec_file_name <- paste0("StrTest-", symbol, "M",timeframe,"X",num_bars, ".csv")
  dec_file_path <- file.path(path_model,  dec_file_name)
  write_csv(dat31, dec_file_path)

  #h2o.shutdown(prompt = FALSE)




}


