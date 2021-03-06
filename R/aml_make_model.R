#' Function to train Deep Learning regression model for a single asset
#'
#' @description  Function is training h2o deep learning model to match future prices of the asset to the indicator pattern.
#' Main idea is to be able to predict future prices by solely relying on the recently retrieved indicator pattern.
#' This is to mimic traditional algorithmic systems based on the indicator rule attempting to automate optimization process with AI.
#'
#' Deep learning model structure is obtained from the 8 random combinations of neurons within 3 hidden layers of the network,
#' the most accurate model configuration will be automatically selected
#'
#' In addition, the function will check if there is a need to update the model. To do that function will check
#' results of the function aml_test_model.R.
#'
#' @details Function is using the dataset prepared by the function aml_collect_data.R.
#' Function will start to train the model as soon as there are more than 1000 rows in the dataset
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @param symbol              Character symbol of the asset for which to train the model
#' @param timeframe           Data timeframe e.g. 1 min
#' @param path_model          Path where the models shall be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#' @param force_update        Boolean, by setting this to TRUE function will generate new model
#'                            (useful after h2o engine update)
#' @param num_nn_options      Integer, value from 1 to 20 or more. Used to change number of variants
#'                            of the random neural network structures
#' @param min_perf            Double, value greater than 0. Used to set minimum value of model performance.
#'                            Higher value will increase computation time
#'
#' @return Function is writing a file object with the best Deep Learning Regression model
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'
#' library(dplyr)
#' library(readr)
#' library(h2o)
#' library(lazytrade)
#' library(lubridate)
#' library(magrittr)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' ind = system.file("extdata", "AI_RSIADXUSDJPY60.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' ind$X1 <- ymd_hms(ind$X1)
#'
#'
#' # data transformation using the custom function for one symbol
#' aml_collect_data(indicator_dataset = ind,
#'                  symbol = 'USDJPY',
#'                  timeframe = 60,
#'                  path_data = path_data)
#'
#' # dataset will be written to the temp directory
#'
#' # start h2o engine
#' h2o.init(nthreads = 2)
#'
#'
#' # performing Deep Learning Regression using the custom function
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=FALSE,
#'                num_nn_options = 3,
#'                min_perf = 0)
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
aml_make_model <- function(symbol, timeframe, path_model, path_data,
                           force_update=FALSE,
                           num_nn_options = 24,
                           min_perf = 0.3){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  ### check if it's actually required to make a model: e.g. if model is tested and results are good...
  ## recover the file name and path
  dec_file_name <- paste0("StrTest-", symbol, "M",timeframe, ".csv")
  dec_file_path <- file.path(path_model,  dec_file_name)

  # generate a file name for model
  m_name <- paste0("DL_Regression", "-", symbol,"-", timeframe)
  m_path <- file.path(path_model, m_name)

  ## read the file and the status of the model
  if(file.exists(dec_file_path) && force_update == FALSE){
    # read the file
    model_status <- readr::read_csv(dec_file_path) %>% select(MaxPerf) %$% MaxPerf
  } else if(force_update == TRUE) {
    # delete the model and previous test results
    #try(remove(dec_file_path), silent = TRUE)
    #try(remove(m_path),silent = TRUE)
    model_status <- -1
  } else { model_status <- 0 }

  #construct the path to the data object see function aml_collect_data.R
  # generate a file name
  f_name <- paste0("AI_RSIADX", symbol,timeframe, ".rds")
  full_path <- file.path(path_data,  f_name)

  x <- try(readr::read_rds(full_path), silent = T)

  # proceed with further steps only if model status is < 0 and there are enough data in x
  if((model_status <= min_perf && nrow(x) > 1000) || (!file.exists(m_path) && nrow(x) > 1000)) {

    dat12 <- x %>%
      # lagging the dataset:    %>% mutate_all(~lag(., n = 28))
      dplyr::mutate(dplyr::across(LABEL, ~lag(., n = 34))) %>%
      # remove empty rows
      na.omit() %>% filter_all(any_vars(. != 0))  %>%
      select(-X1, -X2, -X3)


  # split data to train and test blocks
  # note: model will be trained on the OLDEST data
  test_ind  <- 1:round(0.3*(nrow(dat12))) #train indices 1:xxx
  dat21 <- dat12[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- dat12[-test_ind, ]   #dataset to train the model

  ## ---------- Data Modelling  ---------------
  #h2o.init()

  ### random network structurenum_nn_options <- 24
  nn_sets <- sample.int(n = 100, num_nn_options) %>% matrix(ncol = 3)

  ###

  # load data into h2o environment
  #macd_ML  <- as.h2o(x = dat22, destination_frame = "macd_ML")
  macd_ML  <- h2o::as.h2o(x = dat22, destination_frame = "macd_ML")
  recent_ML  <- h2o::as.h2o(x = dat21, destination_frame = "recent_ML")
  # for loop to select the best neural network structure

  for (i in 1:dim(nn_sets)[1]) {

    # i <- 1
    # fit models from simplest to more complex
  ModelC <- h2o::h2o.deeplearning(
    model_id = paste0("DL_Regression", "-", symbol, "-", timeframe),
    x = names(macd_ML[,1:16]),
    y = "LABEL",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = nn_sets[i, ],
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    #balance_classes = F,
    epochs = 100)

  #ModelC
  #summary(ModelC)

  RMSE <- h2o::h2o.performance(ModelC,newdata = recent_ML)@metrics$RMSE %>%
    as.data.frame()
  #RMSE <- h2o::h2o.performance(ModelC)@metrics$RMSE %>% as.data.frame()
  names(RMSE) <- 'RMSE'

  # record results of modelling
  if(!exists("df_res")){
    df_res <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(RMSE)
  } else {
    df_row <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(RMSE)
    df_res <- df_res %>% dplyr::bind_rows(df_row)
  }


  } # end of for loop

  ## retrain and save the best model
  #what is the most accurate model?
  # find which row in the df_res has the smallest RMSE value slice(which.min(Employees))
  lowest_RMSE <- df_res %>% dplyr::slice(which.min(RMSE)) %>% select(-RMSE) %>% unlist() %>% unname()

  # train the model again:
  ModelC <- h2o::h2o.deeplearning(
    model_id = paste0("DL_Regression", "-", symbol, "-", timeframe),
    x = names(macd_ML[,1:16]),
    y = "LABEL",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = lowest_RMSE,
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    #balance_classes = F,
    epochs = 100)


  # save model object
  h2o::h2o.saveModel(ModelC, path = path_model, force = T)
}
  #h2o.shutdown(prompt = FALSE)


}

