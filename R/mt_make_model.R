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
#' @details Function is using manually prepared dataset and tries several different random neural network structures.
#' Once the best neural network is found then the better model is trained and stored.
#'
#' @author (C) 2020 Vladimir Zhbanko
#' @backref Market Type research of Van Tharp Institute: <https://www.vantharp.com/>
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
#' data(macd_ML2)
#' write_rds(macd_ML2, file.path(path_data, 'macd_ML2.rds'))
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init()
#'
#'
#' # performing Deep Learning Regression using the custom function
#' mt_make_model(num_bars = 64,
#'               path_model = path_model,
#'               path_data = path_data,
#'               f_name_data = "macd_ML2.rds")
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

  # try different models and choose the best one...
  ### random network structure
  nn_sets <- sample.int(n = 100, 24) %>% matrix(ncol = 3)

  for (i in 1:dim(nn_sets)[1]) {

    # i <- 1


  ModelC <- h2o.deeplearning(
    model_id = "DL_Classification",
    x = names(macd_ML[,1:num_bars]),
    y = "M_T",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = nn_sets[i, ],
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
  RMSE <- h2o::h2o.performance(ModelC)@metrics$RMSE %>% as.data.frame()
  names(RMSE) <- 'RMSE'

  # record results of modelling
  if(!exists("df_res")){
    df_res <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(RMSE)
  } else {
    df_row <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(RMSE)
    df_res <- df_res %>% dplyr::bind_rows(df_row)
  }




  } # end of for loop

  # find which row in the df_res has the smallest RMSE value slice(which.min(Employees))
  lowest_RMSE <- df_res %>% dplyr::slice(which.min(RMSE)) %>% select(-RMSE) %>% unlist() %>% unname()

  ModelC <- h2o.deeplearning(
    model_id = "DL_Classification",
    x = names(macd_ML[,1:num_bars]),
    y = "M_T",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = lowest_RMSE,
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "AUTO",
    #balance_classes = T,
    epochs = 200)

h2o.saveModel(ModelC, file.path(path_model, "classification.bin"), force = TRUE)

  #h2o.shutdown(prompt = FALSE)


}

