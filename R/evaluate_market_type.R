### PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
#            to find out specific market type of the financial asset
#            it will also discard bad result outputting -1 if it is the case

# x -< dataframe with one column containing asset indicator in descending order
# model_path <- path to the model
# num_cols - number of columns (features) in the final vector input to the model
# note: it is mandatory to switch on the virtual h2o machine with h2o.init()
#       also to shut it down with h2o.shutdown(prompt = F)

evaluate_market_type <- function(x, model_path, num_cols){
  # x is a 1 column dataframe containing 32 or 64 observations
  # x <- read_rds("macd_df.rds")
  # x <- df
  # num_cols <- 64
  # model_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/regression.bin/DL_Regression"
  # model_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/classification.bin/DL_Classification"
  
  # load models
  m1 <- h2o.loadModel(model_path) 
  
  
  if(class(m1)[1] == "H2ORegressionModel") {
  # Convert to matrix
  X_m <- to_m(x, num_cols) %>% as.data.frame()
  colnames(X_m) <- c(paste("X",1:num_cols,sep=""))
  # load the dataset to h2o 
  test  <- as.h2o(x = X_m, destination_frame = "test")
  
  # retrieve the predicted market type value
  e1 <- h2o.predict(m1, test) 
  
  # round the number to achieve class
  result <- round(e1) %>% as.vector()
  
  # manage negatives and/or bizzare numbers
  if(result <= 0 || result > 6) {element <- -1} else {element <- result}
  
  # output result of prediction from the function
  return(element) 
  
  } 
  
  if(class(m1)[1] == "H2OMultinomialModel") {
    
    # Convert to matrix
    X_m <- to_m(x, num_cols) %>% as.data.frame() 
    colnames(X_m) <- c(paste("X",1:num_cols,sep=""))
    X_m <- X_m %>% transform(M_T = "RAV")
    
    # load the dataset to h2o 
    test  <- as.h2o(x = X_m, destination_frame = "test")
    
    # retrieve the predicted value of the market type
    e1 <- h2o.predict(m1, test) %>% as.data.frame()
    
    # output result of prediction from the function
    return(e1)
    
  }
  
}
