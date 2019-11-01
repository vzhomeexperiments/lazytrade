#' Function to train Deep Learning regression model for a single currency pair
#'
#' @description  Function is training h2o deep learning model to match future prices of the asset to the indicator pattern.
#' Main idea is to be able to predict future prices by solely relying on the most recent indicator pattern.
#' This is to mimic traditional algorithmic systems based on the indicator rule attempting to automate optimization process with AI.
#'
#' @details Performs data manipulation and training of the model. Function is using the dataset prepared by the function aml_collect_data.R.
#' At first collected data will be read by the function. After that function will split the data into 70:30 manner.
#' Secondly, function will attempt to build deep learning model and test it on the remaining 30% of the data.
#'
#' Function will start to train the model as soon as there are more than 100 rows in the dataset
#'
#' Whenever previous model is built function will try to update that model trying to reach the better results.
#' In case new model is better, the better model will be used.
#'
#'
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @param symbol              Character symbol of the asset for which to train the model
#' @param num_bars            Number of bars used to detect pattern
#' @param timeframe           Data timeframe e.g. 1 min
#' @param path_model          Path where the models are be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#'
#'
#' @return Function is writing file object with the model
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'
#' library(tidyverse)
#' library(h2o)
#' library(lazytrade)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' #path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_MODELS"
#' path_data <- normalizePath(tempdir(),winslash = "/")
#' #path_data <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_DATA"
#' data(EURUSDM15X75)
#' write_rds(EURUSDM15X75, file.path(path_data, 'EURUSDM15X75.rds'))
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init()
#'
#'
#' # performing Deep Learning Regression using the custom function
#' aml_make_model(symbol = 'EURUSD',
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
aml_make_model <- function(symbol, num_bars, timeframe, path_model, path_data){

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  ### check if it's actually required to make a model: e.g. if model is tested and results are good...
  ## recover the file name and path
  dec_file_name <- paste0("StrTest-", symbol, "M",timeframe,"X",num_bars, ".csv")
  dec_file_path <- file.path(path_model,  dec_file_name)
  ## read the file and the status of the model
  if(file.exists(dec_file_path)){
    # read the file
    model_status <- read_csv(dec_file_path) %>% select(FinalQuality)
  }

  #construct the path to the data object see function aml_collect_data.R
  # generate a file name
  f_name <- paste0(symbol, "M",timeframe,"X",num_bars, ".rds")
  full_path <- file.path(path_data,  f_name)

  x <- try(read_rds(full_path), silent = T)

  # generate a file name for model
  m_name <- paste0("DL_Regression", "-", symbol,"-", num_bars, "-", timeframe)
  m_path <- file.path(path_model, m_name)

  # proceed with further steps only if model status is < 0 and there are enough data in x
  if(model_status < 0 || (!file.exists(m_path) && nrow(x) > 100)) {

  # split data to train and test blocks
  # note: model will be tested on the PAST data and trained on the NEWEST data
  test_ind  <- 1:round(0.3*(nrow(x))) #train indices 1:xxx
  dat21 <- x[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- x[-test_ind, ]   #dataset to train the model

  ## ---------- Data Modelling  ---------------
  #h2o.init()

  # load data into h2o environment
  #macd_ML  <- as.h2o(x = dat22, destination_frame = "macd_ML")
  macd_ML  <- as.h2o(x = x, destination_frame = "macd_ML")

  # fit models from simplest to more complex
  ModelC <- h2o.deeplearning(
    model_id = paste0("DL_Regression", "-", symbol, "-", num_bars, "-", timeframe),
    x = names(macd_ML[,2:num_bars+1]),
    y = "LABEL",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = c(50,30,15,5),
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    #balance_classes = F,
    epochs = 100)

  #ModelC
  #summary(ModelC)
  #h2o.performance(ModelC)
  # save model object
  h2o.saveModel(ModelC, path = path_model, force = T)
}
  #h2o.shutdown(prompt = FALSE)


}

