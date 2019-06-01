# 
#' Function to predict price change
#' 
#' Function use sentiment dataset. Goal of the function is to predict price change based on the sentiment score
#' For training and learning purposes only! Use at your own risk!!!
#' 
#' @param last_dataset 
#' @param path_model 
#' 
#' 
#' 
predict_price_change <- function(last_dataset, path_model){
  require(h2o)
  require(tidyverse)
  ### use commented code below to test this function  
  # # load dataset - change path when reproducing the code
  # topic_scores_last <- read_rds("C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Logs/topic_scores_last.rds")
    # path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Model/DL_Regression"
  ## --- or use *.rds files provided in the repository as an example
  # topic_scores_last <- read_rds("TWITTER_FINSEN/Logs/topic_scores_last.rds")
  
  
  #h2o.init(nthreads = 1)
  # loading the model
  ModelC <- h2o.loadModel(path = path_model)
  # uploading data to h2o
  recent_ML  <- as.h2o(x = topic_scores_last, destination_frame = "recent_ML")
  # PREDICT the next period...
  result <- h2o.predict(ModelC, recent_ML) %>% as.data.frame()
  
  return(result)
  # shutdown h2o
  #h2o.shutdown(prompt = F)
  
  
}






#### End