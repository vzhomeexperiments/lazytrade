#' Function to train Deep Learning regression model for a single asset
#'
#' @description  Function is training h2o deep learning model to match future
#' prices of the asset to the indicator pattern. Main idea is to be able
#' to predict future prices by solely relying on the recently retrieved
#'  indicator pattern. This is to mimic traditional algorithmic systems
#'  based on the indicator rule and to attempt automated optimization with AI.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @details Deep learning model structure is obtained from several random
#'  combinations of neurons within 3 hidden layers of the network.
#'  The most accurate model configuration will be automatically selected
#'  based either RMSE or Objective Test. In addition, the function will
#'  check if there is a need to update the model. To do that function will check
#'  results of the function aml_test_model.R.
#'
#'  Function is using the dataset prepared by the function aml_collect_data.R.
#'  Note that function will start to train the model as soon as there are more
#'   than 1000 rows in the dataset
#'
#' @concept see https://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
#'
#' @author (C) 2020, 2021 Vladimir Zhbanko
#'
#' @param symbol              Character symbol of the asset for which to train the model
#' @param timeframe           Integer, value in minutes, e.g. 60 min
#' @param path_model          Path where the models shall be stored
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#' @param force_update        Boolean, by setting this to TRUE function will generate new model
#'                            (useful after h2o engine update)
#' @param objective_test      Boolean, option to use trading objective test as a parameter to validate best model,
#'                            defaults to FALSE
#' @param num_nn_options      Integer, value from 0 to 24 or more. Used to change number of variants
#'                            of the random neural network structures, when value is 0 uses fixed structure
#'                            Higher number may lead to long code execution. Select value multiple of 3 otherwise
#'                            function will generate warning. E.g. 12, 24, 48, etc
#' @param fixed_nn_struct     Integer vector with numeric elements, see par hidden in ?h2o.deeplearning,
#'                            default value is c(100,100). Note this will only work if num_nn_options is 0
#' @param num_epoch           Integer, see parameter epochs in ?h2o.deeplearning, default value is 100
#'                            Higher number may lead to long code execution
#' @param num_bars_test       Integer, value of bars used for model testing
#' @param num_bars_ahead      Integer, value to specify how far should the function predict. Default 34 bars.
#' @param min_perf            Double, value greater than 0. Used to set minimum value of model performance.
#'                            Higher value will increase computation time
#' @param num_cols_used       Integer, number of columns to use for training the model, defaults to 16
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
#' tick = system.file("extdata", "TickSize_AI_RSIADX.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' write_csv(tick, file.path(path_data, "TickSize_AI_RSIADX.csv"), col_names = FALSE)
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
#' # performing Deep Learning Regression using 2 random neural network structures and objective test
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=FALSE,
#'                objective_test = TRUE,
#'                num_nn_options = 6,
#'                num_epoch = 10,
#'                min_perf = 0,
#'                num_bars_test = 600,
#'                num_bars_ahead = 34,
#'                num_cols_used = 16)
#'
#' # performing DL Regression using 2 random neural network structures
#' # with objective test, all columns
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=FALSE,
#'                objective_test = TRUE,
#'                num_nn_options = 6,
#'                num_epoch = 10,
#'                min_perf = 0,
#'                num_bars_test = 600,
#'                num_bars_ahead = 34,
#'                num_cols_used = 0)
#'
#'
#' # performing Deep Learning Regression using the custom function
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=FALSE,
#'                objective_test = FALSE,
#'                num_nn_options = 6,
#'                num_epoch = 10,
#'                min_perf = 0,
#'                num_bars_test = 600,
#'                num_bars_ahead = 34,
#'                num_cols_used = 16)
#'
#' # performing Deep Learning Regression, fixed mode
#' aml_make_model(symbol = 'USDJPY',
#'                timeframe = 60,
#'                path_model = path_model,
#'                path_data = path_data,
#'                force_update=TRUE,
#'                num_nn_options = 0,
#'                fixed_nn_struct = c(100, 100),
#'                num_epoch = 10,
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
aml_make_model <- function(symbol, timeframe = 60,
                           path_model, path_data,
                           force_update=FALSE,
                           objective_test = FALSE,
                           num_nn_options = 12,
                           fixed_nn_struct = c(100, 100),
                           num_epoch = 100,
                           num_bars_test = 600,
                           num_bars_ahead = 34,
                           num_cols_used = 16,
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

  # file name with the tick data
  path_tick <- file.path(path_data, "TickSize_AI_RSIADX.csv")
  #dataset with tick data
  z <- readr::read_csv(path_tick, col_names = FALSE) %>%
    #filter line with a symbol we need
    dplyr::filter(X1 == symbol) %$%
    #value z will contain tick value for this symbol
    X2

  # proceed with further steps only if model status is < 0 and there are enough data in x
  if((model_status <= min_perf && nrow(x) > 1000) || (!file.exists(m_path) && nrow(x) > 1000)) {
    ## Generate dataset required for neural network training
    dat12 <- x %>%
      # lagging the dataset:    %>% mutate_all(~lag(., n = 28))
      dplyr::mutate(dplyr::across(LABEL, ~lag(., n = num_bars_ahead))) %>%
      # remove empty rows
      na.omit() %>% filter_all(any_vars(. != 0))  %>%
      select(-X1, -X2, -X3)


  # split data to train and test blocks
  # note: model will be trained on the OLDEST data
  test_ind  <- 1:num_bars_test #round(0.3*(nrow(dat12))) #train indices 1:xxx
  dat21 <- dat12[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- dat12[-test_ind, ]   #dataset to train the model


  # find number of columns
  dat22_ncol <- ncol(dat22)-1

  # fail safe: check number of columns with parameter num_cols_used
  if(num_cols_used > dat22_ncol){stop("Parameter num_cols_used is greater than what is in the available data",
                                      call. = FALSE)}

  # use full set of columns
  if(num_cols_used == 0){
    num_cols_used = dat22_ncol
  }

  ## ---------- Data Modelling  ---------------
  #h2o.init()

# load data into h2o environment
  #macd_ML  <- as.h2o(x = dat22, destination_frame = "macd_ML")
  macd_ML  <- h2o::as.h2o(x = dat22, destination_frame = "macd_ML")
  recent_ML  <- h2o::as.h2o(x = dat21, destination_frame = "recent_ML")

  # for loop to select the best neural network structure
  ### fix or random network structure num_nn_options <- 24
  ###
  n_layers <- length(fixed_nn_struct)

  if(num_nn_options == 0){
    nn_sets <- fixed_nn_struct %>% matrix(ncol = n_layers)
  } else {
    nn_sets <- sample.int(n = 100, num_nn_options) %>% matrix(ncol = 3)
  }


  ###



  for (i in 1:dim(nn_sets)[1]) {

    # i <- 2
    # fit models from simplest to more complex
  ModelC <- h2o::h2o.deeplearning(
    model_id = paste0("DL_Regression", "-", symbol, "-", timeframe),
    x = names(macd_ML[,1:num_cols_used]),
    y = "LABEL",
    training_frame = macd_ML,
    activation = "Tanh",
    autoencoder = FALSE,
    hidden = nn_sets[i, ],
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    #balance_classes = F,
    epochs = num_epoch)

  #ModelC
  #summary(ModelC)

    ## Option to do model functional test
    if(objective_test){

      ### define best model using objective test

      #dataset without X1 column (for predictions)
      te <- x  %>%
        dplyr::select(-X1, -X2, -X3, -LABEL) %>%
        # only keep last month for simulation
        utils::head(num_bars_test)

      # uploading data to h2o
      recent_ML  <- h2o::as.h2o(x = te, destination_frame = "recent_ML")

      # PREDICT the next period...
      result_R1 <- h2o::h2o.predict(ModelC, recent_ML) %>% as.data.frame()

      dat31 <- x %>%
        # using last 600 observations
        utils::head(num_bars_test) %>%
        ## select columns:
        # X1 time index
        # X2 price at the time index
        # X3 price 34 bars ago
        # LABEL is a price difference X3-X2
        dplyr::select(X1, X2) %>%
        # add column with predicted price change
        dplyr::bind_cols(result_R1) %>%
        ## create columns:
        # dP_34 - price difference now vs 34 bars (only for check)
        # dplyr::mutate(dP_34 = X3-X2) %>%
        ## setup condition to enter the trade
        # create a risk column, use 20 pips as a trigger
        dplyr::mutate(Risk = if_else(predict > 20, 1/z, if_else(predict < -20, -1/z, 0))) %>%
        ## create a columns with shifted X2 price down:
        # value BR will indicate number of bars we will hold this position
        dplyr::mutate(X2_NB = lag(X2, num_bars_ahead)) %>%
        # clean up this dataset
        na.omit() %>%
        # now calculate the scenario
        dplyr::mutate(Hold_NB = Risk*(X2_NB - X2)) %>%
        # remove zero values to calculate presumed number of trades
        dplyr::filter(Risk != 0) %>%
        # get the sum of columns
        # Column Expected PNL would be the result in case all trades would be successful
        # Column Achieved PNL is the results achieved in reality
        dplyr::summarise(PnL_NB = sum(Hold_NB),
                         TotalTrades = n(),
                         TR_Level = 20,
                         NB_hold = num_bars_ahead,
                         Symbol = symbol) %>%
        #extract only column with PnL
        select(PnL_NB)


      # record results of modelling
      if(!exists("df_res_t")){
        df_res_t <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(dat31)
      } else {
        df_row <- nn_sets[i,] %>% t() %>% as.data.frame() %>% dplyr::bind_cols(dat31)
        df_res_t <- df_res_t %>% dplyr::bind_rows(df_row)
      }

      #save intermediate models!
      # save model object
      temp_model_path <- file.path(path_model, i)
      if(!dir.exists(temp_model_path)){dir.create(temp_model_path)}
      h2o::h2o.saveModel(ModelC, path = temp_model_path, force = T)

      #what is the most accurate model structure from objective test?
      # find which row number in the df_res_t has the highest value slice(which.min(Employees))
      lowest_RMSE <- df_res_t %>% dplyr::slice(which.max(PnL_NB)) %>% select(-PnL_NB) %>% unlist() %>% unname()
      # find which row in the df_res_t produces max PnL_NB?
      best_row <- which.max(df_res_t$PnL_NB)


    } else {

      ### define best model using RMSE
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

      #save intermediate models!
      # save model object
      temp_model_path <- file.path(path_model, i)
      if(!dir.exists(temp_model_path)){dir.create(temp_model_path)}
      h2o::h2o.saveModel(ModelC, path = temp_model_path, force = T)


      #what is the most accurate model structure from RMSE test??
      # find which row in the df_res has the smallest RMSE value slice(which.min(Employees))
      lowest_RMSE <- df_res %>% dplyr::slice(which.min(RMSE)) %>% select(-RMSE) %>% unlist() %>% unname()
      best_row <- which.min(df_res$RMSE)

    }


  } # end of for loop






  ## retrieve and copy/paste the best model
  best_model_location <- file.path(path_model, best_row, m_name)
  best_model_destination <- file.path(path_model, m_name)
  # copy best model object
  file.copy(best_model_location, path_model, overwrite = TRUE)


}
  #h2o.shutdown(prompt = FALSE)


}

