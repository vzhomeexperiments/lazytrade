#' Function to train Deep Learning regression model
#'
#' @description  Function is training h2o deep learning model to match future prices of the asset to the indicator pattern.
#' Main idea is to be able to predict future prices by solely relying on the most recent indicator pattern.
#'
#' @details Performs data manipulation and training of the model. Function is handling shift of the price and indicator datasets.
#' Function will also check how the model predict by using trading objective.
#' NOTE: Always run parameter research_mode = TRUE for the first time
#'
#' Because of the function is intended to periodically re-train the model it would always check how the previous model was working.
#' In case new model is better, the better model will be used.
#'
#' Function can also write a log files with a results of the strategy test
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @param price_dataset       Dataset containing assets prices. It will be used as a label
#' @param indicator_dataset   Dataset containing assets indicator which pattern will be used as predictor
#' @param num_bars            Number of bars used to detect pattern
#' @param timeframe           Data timeframe e.g. 1 min
#' @param research_mode       When TRUE model will be saved and model result will be stored as well. To be used at the first run.
#' @param path_model          Path where the models are be stored
#' @param write_log           Writes results of the newly trained model and previously used model to the file
#' @param setup_mode          When TRUE function will attempt to write model to the disk without checking it
#'
#' @return Function is writing files into Decision Support System folder
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
#' # start h2o engine
#' h2o.init(nthreads = 2)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(indicator_dataset_big)
#' data(price_dataset_big)
#'
#' prices <- price_dataset_big
#' macd <- indicator_dataset_big
#'
#' # performing Deep Learning Regression using the custom function
#' self_learn_ai_R(price_dataset = prices,
#'                 indicator_dataset = macd,
#'                 num_bars = 75,
#'                 timeframe = 60,
#'                 path_model = path_model,
#'                 setup_mode = FALSE,
#'                 research_mode = FALSE,
#'                 write_log = FALSE)
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
self_learn_ai_R <- function(price_dataset, indicator_dataset, num_bars, timeframe, path_model,
                            setup_mode = FALSE,
                            research_mode = FALSE,
                            write_log = TRUE){
  .Deprecated("self_learn_ai_R",msg = "This function is replaced by function aml_make_model")
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  # transform data and get the labels shift rows down Note: the oldest data in the first row!! mutate_all(~.-lag(.))
  # working but depricated: %>% mutate_all(funs(lag), n=28)
  # new alternative:        %>% mutate_all(~lag(., n = 28))
  dat14 <- create_labelled_data(price_dataset, num_bars, type = "regression") %>% mutate_all(~lag(., n = 28))
  # transform data for indicator. Note: the oldest data in the first row!!
  dat15 <- create_transposed_data(indicator_dataset, num_bars)
  # dataframe for the DL modelling it contains all the available data.
  # Note: Zero values in rows will mean that there was no data in the MT4 database.
  #       These rows will be removed before modelling however it's advisable not to have those as it might give data artefacts!
  dat16 <- dat14 %>% select(LABEL) %>% bind_cols(dat15) %>% na.omit() %>% filter_all(any_vars(. != 0)) %>% filter(LABEL < 600, LABEL > -600)
  # checking the data: summary(dat16) # too high values in the LABEL Column are non-sense! hist(dat16$LABEL)

  # split data to train and test blocks
  # note: model will be tested on the PAST data and trained on the NEWEST data
  test_ind  <- 1:round(0.3*(nrow(dat16))) #train indices 1:xxx
  dat21 <- dat16[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- dat16[-test_ind, ]   #dataset to train the model

  ## ---------- Data Modelling  ---------------
  #h2o.init()

  # load data into h2o environment
  macd_ML  <- as.h2o(x = dat22, destination_frame = "macd_ML")

  # fit models from simplest to more complex
  ModelC <- h2o.deeplearning(
    model_id = paste0("DL_Regression", num_bars, "-", timeframe),
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
  ## save model object if it is not exists yet or when setup_mode == TRUE this is useful when updating h2o!
  if (setup_mode == TRUE || !file.exists(file.path(path_model, paste0("DL_Regression", num_bars, "-", timeframe)))) {
    h2o.saveModel(ModelC, path = path_model, force = T)
  }


  ## Checking how the new model predict using the latest dataset
  # upload recent dataset to predict
  recent_ML  <- as.h2o(x = dat21[,-1], destination_frame = "recent_ML")
  # use model to predict
  result <- h2o.predict(ModelC, recent_ML) %>% as.data.frame() %>% select(predict) %>% round()
  ## evaluate hypothetical results of trading using the model, do for several take profit and stop loss levels. Bring the best results:
  dat31 <- test_model(dat21, result, test_type = "regression")


### Test existing model with new data to compare both results and keep the better model for production
  # check existence of the model trained previously and if exist, load it and test strategy using it
  ModelC_prev <- try(h2o.loadModel(paste0(path_model, "/DL_Regression",
                                      num_bars, "-", timeframe)),silent = T)
  if(!class(ModelC_prev)=='try-error'){
    # result prev
    result_prev <- h2o.predict(ModelC_prev, recent_ML) %>% as.data.frame() %>% select(predict) %>% round()

    ## evaluate hypothetical results of trading using the model
    dat31_prev <- test_model(dat21, result_prev, test_type = "regression")


  }

  # write the final object dat31 to the file for debugging or research
  if(research_mode == TRUE){
    # In research mode we will write results to the new folder
    write_rds(dat31, file.path(getwd(), paste0("_SETUP/", Sys.Date(), "-Result-", num_bars, "-", timeframe, ".rds")))
    h2o.saveModel(ModelC, path = path_model, force = T)
    }


  # save the model in case it's good and Achieved is not much less than Expected!
  if(research_mode == FALSE && dat31$FinalOutcome == "VeryGood" &&
     #condition OR will also overwrite the model in case previously made model is performing worse than the new one
     # NOTE: this condition dat31$FinalQuality > 0.8 can be removed after finding the first model
     (dat31$FinalQuality > 0.8 || dat31$FinalQuality > dat31_prev$FinalQuality)){
  h2o.saveModel(ModelC, path = path_model, force = TRUE)
  }


  # write logs if enabled
  if(write_log == TRUE){
    # create folder where to save if not exists
    path_LOG <- paste0(path_model, "/LOG/")
    if(!dir.exists(path_LOG)){dir.create(path_LOG)}
    # combine data and join them to one object
    dat61 <- dat31 %>% mutate(new_or_old = "NEW", num_bars = num_bars, timeframe = timeframe, model_type = "R")
    dat62 <- dat31_prev %>% mutate(new_or_old = "PREV", num_bars = num_bars, timeframe = timeframe, model_type = "R")
    bind_rows(dat61, dat62) %>%
    # write combined data to the file named with current date
    write_csv(path = paste0(path_LOG, Sys.Date(), "-", num_bars, "-",timeframe, "R", ".csv"))
    # write the best current TP/SL level
    bind_rows(dat61, dat62) %>%
      # take the best quality level
      slice(which.max(FinalQuality)) %>%
      # select the TPSL levels
      select(TPSL_Level) %>%
      # write best possible trigger to the file
      write_csv(path = paste0(path_LOG, "AI_T-", timeframe, ".csv"))
  }

  #h2o.shutdown(prompt = FALSE)




}

