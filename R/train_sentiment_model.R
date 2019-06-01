# 
#' Function to train deep learning model
#' 
#' Function uses joined price and sentiment datasets. Goal of the function is to create deep learning
#' model trained to predict future price change of the label.
#' 
#' 
#' @param joined_dataset 
#' @param path_model 
#' 
#' 
#' 
train_sentiment_model <- function(joined_dataset, path_model){
  require(h2o)
  require(tidyverse)
  ### use commented code below to test this function  
  # # load dataset - change path when reproducing the code
  # joined_dataset <- read_rds("C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Logs/Sent_price.rds")
  # path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Model"
  ## --- or use *.rds files provided in the repository as an example
  # joined_dataset <- read_rds("TWITTER_FINSEN/Logs/Sent_price.rds")
  
  # arrange data so the latest records are on top
  dat11 <- joined_dataset %>% arrange(desc(DateTimeR))
  
  # calculate the price difference that will be used to predict
  dat12 <- dat11 %>% mutate_at(vars("X2"), funs(lag(.)- .))
  
  # shift data of the label to 'align' label if needed
  dat13 <- dat12 %>% mutate_at(vars("X2"), funs(lead), n = 1)
    
  # # shift data of the label to 'align' label more to the future
  # dat14 <- dat13 %>% mutate_at(vars("X2"), funs(lead), n = 1)
  
  # select columns for modeling
  dat15 <- dat12 %>% select(-c(11:12)) 
  
  # remove na's
  dat16 <- dat15 %>% na.omit()
  
  
  # # split data to train and test blocks 0.8 - training and 0.2 for test randomly
  # smp_size <- floor(0.8 * nrow(dat16))
  # train_ind  <- sample(seq_len(nrow(dat16)), size = smp_size)
  
  # split data to train and test blocks 0.8 - training and 0.2 for test non randomly
  train_ind <- 1:round(0.8*(nrow(dat16))) #train indices 1:xxx
  
  dat21 <- dat16[-train_ind, ] #dataset to test the model using 20% of data
  dat22 <- dat16[train_ind,]   #dataset to train the model
  
  
  #library(plotly)
  ## Visualize new matrix in 3D
  #plot_ly(z = as.matrix(dat16[,1:10]), type = "surface")
  
  ## ---------- Data Modelling  ---------------
  #h2o.init()
  
  # load data into h2o environment
  data_ML  <- as.h2o(x = dat22, destination_frame = "data_ML")
  
  # fit models from simplest to more complex
  ModelC <- h2o.deeplearning(
    model_id = "DL_Regression",
    x = names(data_ML[,1:10]), 
    y = "X2",
    training_frame = data_ML,
    activation = "Tanh",
    overwrite_with_best_model = TRUE, 
    autoencoder = FALSE, 
    hidden = c(8,5,3), 
    loss = "Automatic",
    sparse = TRUE,
    l1 = 1e-4,
    distribution = "AUTO",
    stopping_metric = "MSE",
    epochs = 100)
  
  #ModelC
  #summary(ModelC)
  #h2o.performance(ModelC)

  ## Checking how the new model predict using the latest dataset
  # upload recent dataset to predict
  recent_ML  <- as.h2o(x = dat21[,-11], destination_frame = "recent_ML")
  # use model to predict
  result <- h2o.predict(ModelC, recent_ML) %>% as.data.frame() %>% select(predict) #%>% round()
  
  ## save obtained results to files - they will be used by the different function to test the strategy
  # write data set for test with real data
  write_rds(dat21, "C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Model/truth_res.rds")
  
  # write predicted results 
  write_rds(result, "C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Model/predicted_res.rds")
  
  # save model
  h2o.saveModel(ModelC, path = path_model, force = T)
  #h2o.shutdown(prompt = FALSE)
  
  
  
}





#### End