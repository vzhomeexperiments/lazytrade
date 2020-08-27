#' Function to train Deep Learning regression model
#'
#' @description  Function is training h2o deep learning model to match future prices of the asset to the indicator pattern.
#' Main idea is to be able to predict future prices by relying on several indicators and their patterns
#'
#' @details
#' Several indicators data points on different timeframes are provided together with corresponding asset price.
#' Function is handling shift of the price and indicator datasets.
#' Function will also check how the model predict by using trading objective.
#' NOTE: This is a special variant of the function aml_make_model.R to handle data from files
#'
#' Because of the function is intended to periodically re-train the model it would always check how the previous model was working.
#' In case new model is better, the better model will be used.
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @param asset_dataset       Dataset containing price and corresponding indicator of the asset
#' @param asset_name          String, name of the asset we work with
#' @param num_bars            Number of bars used to train the model
#' @param timeframe           Data timeframe e.g. 60 min
#' @param path_data           Path where the data files are stored
#' @param path_model          Path where the models are be stored
#' @param setup_mode          When TRUE function will attempt to write model to the disk without checking it
#'
#' @return Function is output is a Deep Learning model object
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' library(dplyr)
#' library(readr)
#' library(magrittr)
#' library(h2o)
#' library(lazytrade)
#' # start h2o engine (using all CPU's by default)
#' h2o.init()
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' # using asset 1
#' asset_dataset = system.file("extdata", "AI_RSIADXUSDJPY.csv",
#'                             package = "lazytrade") %>% read_csv(col_names = F)
#'
#' aml2_make_model(asset_dataset = asset_dataset,
#'                 asset_name = "USDJPY",
#'                 num_bars = 1500,
#'                 timeframe = 60,
#'                 path_data = path_data,
#'                 path_model = path_model,
#'                 setup_mode = FALSE)
#'
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
aml2_make_model <- function(asset_dataset,
                            asset_name,
                            num_bars = 1500,
                            timeframe = 60,
                            path_data = path_data,
                            path_model = path_model,
                            setup_mode = FALSE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  # transform data and get the labels shift rows down
  # Note: the Newest data in the first row!!
  # X2 - latest price value
  # X3 - price value 21 bars ago
  dat11 <- asset_dataset %>%
  # find the price difference between now and 21 bars ago consider JPY pairs...
    dplyr::mutate(LABEL = ifelse(X2 < 30, 1000*(X3-X2), 100 * (X3-X2))) %>%
  # lagging the dataset:    %>% mutate_all(~lag(., n = 28))
    dplyr::mutate(dplyr::across(LABEL, ~lag(., n = 34))) %>%
  # remove empty rows
    na.omit() %>% filter_all(any_vars(. != 0)) %>%
  # select right columns by removing not used ones
    select(-X1, -X2, -X3)


    # split data to train and test blocks
  #test_ind  <- 1:round(0.3*(nrow(dat11))) #train indices 1:xxx
  test_ind  <- 1:num_bars #train indices 1:xxx
  dat21 <- dat11[test_ind, ]    #dataset to train the model
  dat22 <- dat11[-test_ind, ]   #dataset to test the model

  ## ---------- Data Modelling  ---------------
  #h2o.init()

  # load data into h2o environment
  macd_ML  <- as.h2o(x = dat21, destination_frame = "macd_ML")

  # fit models from simplest to more complex
  ModelC <- h2o.deeplearning(
    model_id = paste0("DL_Regression", asset_name, "-", timeframe),
    x = names(macd_ML[,1:16]),
    y = "LABEL",
    training_frame = macd_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE,
    autoencoder = FALSE,
    hidden = c(50,20,13),
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    #balance_classes = F,
    epochs = 200)

  #ModelC
  #summary(ModelC)
  #h2o.performance(ModelC)
  ## save model object if it is not exists yet or when setup_mode == TRUE this is useful when updating h2o!
  if (setup_mode == TRUE || !file.exists(file.path(path_model, paste0("DL_Regression", asset_name, "-", timeframe)))) {
    h2o.saveModel(ModelC, path = path_model, force = T)
  }


  ## Checking how the new model predict using the test dataset
  # upload recent dataset to predict
  recent_ML  <- as.h2o(x = dat22[,-17], destination_frame = "recent_ML")
  # use model to predict
  result <- h2o.predict(ModelC, recent_ML) %>% as.data.frame() %>% select(predict) %>% round()
  ## evaluate hypothetical results of trading using the model, do for several take profit and stop loss levels. Bring the best results:
  dat31 <- test_model(dat22, result, test_type = "regression")


### Test existing model with new data to compare both results and keep the better model for production
  # check existence of the model trained previously and if exist, load it and test strategy using it
  ModelC_prev <- try(h2o.loadModel(paste0(path_model, "/DL_Regression",
                                          asset_name, "-", timeframe)),silent = T)
  if(!class(ModelC_prev)=='try-error'){
    # result prev
    result_prev <- h2o.predict(ModelC_prev, recent_ML) %>% as.data.frame() %>% select(predict) %>% round()

    ## evaluate hypothetical results of trading using the model
    dat31_prev <- test_model(dat22, result_prev, test_type = "regression")


  }

  # save the model in case it's good and Achieved is not much less than Expected!
  if(dat31$FinalOutcome == "VeryGood" &&
     #condition OR will also overwrite the model in case previously made model is performing worse than the new one
     # NOTE: this condition dat31$FinalQuality > 0.8 can be removed after finding the first model
     (dat31$FinalQuality > 0.6 || dat31$FinalQuality > dat31_prev$FinalQuality)){
  h2o.saveModel(ModelC, path = path_model, force = TRUE)
  }




  #h2o.shutdown(prompt = FALSE)




}

