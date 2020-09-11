#' Function to score data and predict current market type using pre-trained classification model
#'
#' @description PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
#'            to find out specific market type of the financial asset
#'            it will also discard bad result outputting -1 if it is the case
#'
#'
#' @details it is mandatory to switch on the virtual h2o machine with h2o.init()
#' also to shut it down with h2o.shutdown(prompt = F)
#'
#' @param x - dataframe with one column containing asset indicator in the time descending order,
#' typically 64 or more values
#'
#' @param path_model - path to the model
#' @param num_cols - number of columns (features) in the final vector input to the model
#'
#' @return dataframe with predicted value of the market type
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' library(h2o)
#' library(magrittr)
#' library(dplyr)
#' library(readr)
#' library(lazytrade)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(macd_ML2)
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init()
#'
#' # performing Deep Learning Regression using the custom function
#' # this function stores model to the temp location
#' mt_make_model(indicator_dataset = macd_ML2,
#'               num_bars = 64,
#'               path_model = path_model,
#'               path_data = path_data)
#'
#'
#' # Use sample data
#' data(macd_100)
#'
#' # use one column for testing
#' x <- macd_100[ ,2]
#'
#'
#' evaluate_market_type(x = x,
#'                      path_model = path_model,
#'                      num_cols = 64)
#'
#' h2o.shutdown(prompt = FALSE)
#'
#' #set delay to insure h2o unit closes properly before the next test
#' Sys.sleep(5)
#'
#' }
#'
evaluate_market_type <- function(x, path_model, num_cols){


  requireNamespace("h2o", quietly = TRUE)

  # generate a file name for model
  m_name <- "DL_Classification"
  m_path <- file.path(path_model, m_name)

  # load models
  m1 <- h2o::h2o.loadModel(m_path)


  if(class(m1)[1] == "H2ORegressionModel") {
  # Convert to matrix
  X_m <- lazytrade::to_m(x, num_cols) %>% as.data.frame()
  colnames(X_m) <- c(paste("X",1:num_cols,sep=""))
  # load the dataset to h2o
  test  <- h2o::as.h2o(x = X_m, destination_frame = "test")

  # retrieve the predicted market type value
  e1 <- h2o::h2o.predict(m1, test)

  # round the number to achieve class
  result <- round(e1) %>% as.vector()

  # manage negatives and/or bizzare numbers
  if(result <= 0 || result > 6) {element <- -1} else {element <- result}

  # output result of prediction from the function
  return(element)

  }

  if(class(m1)[1] == "H2OMultinomialModel") {

    # Convert to matrix
    X_m <- lazytrade::to_m(x, num_cols) %>% as.data.frame()
    colnames(X_m) <- c(paste("X",1:num_cols,sep=""))
    X_m <- X_m %>% transform(M_T = "RAV")

    # load the dataset to h2o
    test  <- h2o::as.h2o(x = X_m, destination_frame = "test")

    # retrieve the predicted value of the market type
    e1 <- h2o::h2o.predict(m1, test) %>% as.data.frame()

    # obtained values will be analysed in the scoring script

    # output result of prediction from the function
    return(e1)

  }

}
