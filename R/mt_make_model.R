#' Function to train Deep Learning Classification model for Market Type recognition
#'
#' @description  Function is training h2o deep learning model to match
#' classified patterns of the financial indicator. 
#' Main idea is to be able to detect Market Type by solely relying on the 
#' current indicator pattern.
#' This is in the attempt to evaluate current market type for trading purposes.
#'
#' Selected Market Periods could be manually classified 
#' according to the theory from Van K. Tharp:
#' 1. Bull normal, BUN
#' 2. Bull volatile, BUV
#' 3. Bear normal, BEN
#' 4. Bear volatile, BEV
#' 5. Sideways quiet, RAN
#' 6. Sideways volatile, RAV
#'
#' For automatic classification, could only be used: BUN, BEN, RAN market types  
#'
#' `r lifecycle::badge('experimental')`
#'
#' @details Function is using labeled dataset and tries several different random 
#' neural network structures. Once the best neural network is found then the 
#' better model is selected and stored. Dataset can be either manually labelled
#' or generated using function mt_stat_transf.R. In the latter case parameter
#' is_cluster shall be set to TRUE.
#'
#' @author (C) 2020, 2021 Vladimir Zhbanko
#' @backref Market Type research of Van Tharp Institute: <https://www.vantharp.com/>
#'
#' @param indicator_dataset   Data frame, Data set containing indicator patterns to train the model
#' @param num_bars            Integer, Number of bars used to detect pattern
#' @param timeframe           Integer, Data time frame in minutes.
#' @param path_model          String, Path where the models are be stored
#' @param path_data           String, Path where the aggregated historical data is stored,
#'                            if exists, in rds format
#' @param activate_balance    Boolean, option to choose to balance market type classes or not,
#'                            default TRUE
#' @param num_nn_options      Integer, value from 0 to 24 or more as multiple of 3.
#'                            Used to change number of variants for 3 hidden layer structure.
#'                            Random neural network structures will be generated.
#'                            When value 0 is set then a fixed structure will be used as 
#'                            defined by parameter fixed_nn_struct. 
#'                            To avoid warnings make sure to set this value as
#'                            multiple of 3. Higher values will increase computation time.
#' @param fixed_nn_struct     Integer vector with numeric elements, see par hidden in ?h2o.deeplearning,
#'                            default value is c(100,100). 
#'                            Note this will only work if num_nn_options is 0
#' @param num_epoch           Integer, see parameter epochs in ?h2o.deeplearning, default value is 100
#'                            Higher number may lead to long code execution
#' @param is_cluster          Boolean, set TRUE to use automatically clustered data
#' 
#' @return Function is writing file object with the model
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
#' library(stats)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(macd_ML60M)
#'
#' Sys.sleep(5)
#'
#' # start h2o engine
#' h2o.init(nthreads = 2)
#'
#'
#' # performing Deep Learning Classification using manually labelled data
#' mt_make_model(indicator_dataset = macd_ML60M,
#'               num_bars = 64,
#'               timeframe = 60,
#'               path_model = path_model,
#'               path_data = path_data,
#'               activate_balance = TRUE,
#'               num_nn_options = 3,
#'               num_epoch = 10)
#'
#' data(price_dataset_big)
#' data <- head(price_dataset_big, 5000) #reduce computational time
#'
#' ai_class <- mt_stat_transf(indicator_dataset = data,
#'                       num_bars = 64,
#'                       timeframe = 60,
#'                       path_data = path_data,
#'                       mt_classes = c('BUN', 'BEN', 'RAN'))
#'
#' # performing Deep Learning Classification using the custom function auto clustered data
#' mt_make_model(indicator_dataset = ai_class,
#'               num_bars = 64,
#'               timeframe = 60,
#'               path_model = path_model,
#'               path_data = path_data,
#'               activate_balance = TRUE,
#'               num_nn_options = 6,
#'               num_epoch = 10,
#'               is_cluster = TRUE)
#'
#' # performing Deep Learning Classification using the custom function auto clustered data
#' # and fixed nn structure
#' mt_make_model(indicator_dataset = ai_class,
#'               num_bars = 64,
#'               timeframe = 60,
#'               path_model = path_model,
#'               path_data = path_data,
#'               activate_balance = TRUE,
#'               num_nn_options = 0,
#'               fixed_nn_struct = c(10, 10),
#'               num_epoch = 10,
#'               is_cluster = TRUE)
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
mt_make_model <- function(indicator_dataset,
                          num_bars = 64,
                          timeframe = 60,
                          path_model, 
                          path_data,
                          activate_balance = TRUE,
                          num_nn_options = 24,
                          fixed_nn_struct = c(100, 100),
                          num_epoch = 100,
                          is_cluster = FALSE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("h2o", quietly = TRUE)

  # generate a file name for model
  m_name <- paste0("DL_Classification", "_", timeframe, "M")
  m_path <- file.path(path_model, m_name)
  
  if(is_cluster == TRUE){
    num_bars <- ncol(indicator_dataset)-1
  }

  macd_ML2 <- indicator_dataset %>% 
    #make sure column with label is a factor
    dplyr::mutate(across("M_T", as.factor))

  # check if we don't have too much data
  x1_nrows <- macd_ML2 %>% nrow()
  # what to do if too much rows?
  if(x1_nrows > 50000){
    # read all the data
    macd_ML2 <- macd_ML2 %>%
      # use only last 40000 rows, 40000 is to avoid this code to run so often...
      utils::head(40000)
  }

  # split data into 2 groups
  # split data to train and test blocks
  # note: model will be trained on the OLDEST data
  test_ind  <- 1:round(0.3*x1_nrows) #test indices 1:xxx
  dat21 <- macd_ML2[test_ind, ]    #dataset to test the model using 30% of data
  dat22 <- macd_ML2[-test_ind, ]   #dataset to train the model
  
  
  # get this data into h2o:
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
  
  # try different models and choose the best one...
  for (i in 1:dim(nn_sets)[1]) {

    # i <- 1


  ModelM <- h2o.deeplearning(
    model_id = paste0("DL_Classification", "_", timeframe, "M"),
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
    balance_classes = activate_balance,
    epochs = num_epoch)

  #ModelM
  #summary(ModelM)
  #h2o.performance(ModelM)
  
  ### define best model using RMSE
  RMSE <- h2o::h2o.performance(ModelM,newdata = recent_ML)@metrics$RMSE %>%
    as.data.frame()
  #RMSE <- h2o::h2o.performance(ModelM)@metrics$RMSE %>% as.data.frame()
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
  h2o::h2o.saveModel(ModelM, path = temp_model_path, force = T)

  } # end of for loop

  # find which row in the df_res has the smallest RMSE value slice(which.min(Employees))
  lowest_RMSE <- df_res %>% dplyr::slice(which.min(RMSE)) %>% select(-RMSE) %>% unlist() %>% unname()
  best_row <- which.min(df_res$RMSE)
  
  ## retrieve and copy/paste the best model
  best_model_location <- file.path(path_model, best_row, m_name)
  best_model_destination <- file.path(path_model, m_name)
  # copy best model object
  file.copy(best_model_location, path_model, overwrite = TRUE)
  

  #h2o.shutdown(prompt = FALSE)


}

