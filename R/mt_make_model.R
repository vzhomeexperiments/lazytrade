#' Function to train Deep Learning Classification model for Market Type recognition
#'
#' @description  Function is training h2o deep learning model to match manually classified patterns of the financial
#' indicator. Main idea is to be able to detect Market Type by solely relying on the current indicator pattern.
#' This is in the attempt to evaluate current market type and to use proper trading strategy.
#'
#' Selected Market Periods according to the theory from Van K. Tharp:
#' 1. Bull normal, BUN
#' 2. Bull volatile, BUV
#' 3. Bear normal, BEN
#' 4. Bear volatile, BEV
#' 5. Sideways quiet, RAN
#' 6. Sideways volatile, RAV
#'
#'
#' @details Function is using manually prepared dataset
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @param num_bars            Number of bars used to detect pattern
#' @param path_model          Path where the models are be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#' @param f_name_data         Name of the file with the data
#'
#' @return Function is writing file object with the model
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' library(dplyr)
#' library(readr)
#' library(h2o)
#' library(lazytrade)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(macd_ML2_small)
#' write_rds(macd_ML2_small, file.path(path_data, 'macd_ML2_small.rds'))
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init()
#'
#'
#' # performing Deep Learning Regression using the custom function
#' mt_make_model(num_bars = 128,
#'               path_model = path_model,
#'               path_data = path_data,
#'               f_name_data = "macd_ML2_small.rds")
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
mt_make_model <- function(num_bars, path_model, path_data, f_name_data){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  macd_ML2 <- read_rds(file.path(path_data, f_name_data)) %>% mutate_at("M_T", as.factor)

  macd_ML  <- as.h2o(x = macd_ML2, destination_frame = "macd_ML")

  # fit models from simplest to more complex
  ModelC <- h2o.deeplearning(
    model_id = "DL_Classification",
    x = names(macd_ML[,1:num_bars]),
    y = "M_T",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = c(100,100),
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "AUTO",
    #balance_classes = T,
    epochs = 200)

  #ModelC
  #summary(ModelC)
  #h2o.performance(ModelC)

h2o.saveModel(ModelC, file.path(path_model, "classification.bin"), force = TRUE)

  #h2o.shutdown(prompt = FALSE)


}

