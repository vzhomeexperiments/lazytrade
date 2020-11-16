#' Function to score data and predict current market type using pre-trained classification model
#'
#' @description PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
#'                       to find out specific market type of the financial asset
#'                       it will also discard bad result outputting -1 if it is the case
#'
#'
#' @details it is mandatory to switch on the virtual h2o machine with h2o.init()
#' also to shut it down with h2o.shutdown(prompt = F)
#'
#' @param x - dataframe with one column containing asset indicator in the time descending order,
#' typically 64 or more values
#'
#' @param path_model       String, path to the model
#' @param num_cols         Integer, number of columns (features) in the final vector input to the model
#' @param timeframe        Integer, timeframe in Minutes.
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
#' data(macd_ML60M)
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init(nthreads = 2)
#'
#' # performing Deep Learning Regression using the custom function
#' # this function stores model to the temp location
#' mt_make_model(indicator_dataset = macd_ML60M,
#'               num_bars = 64,
#'               timeframe = 60,
#'               path_model = path_model,
#'               path_data = path_data,
#'               activate_balance = TRUE,
#'               num_nn_options = 3)
#'
#'
#' # Use sample data
#' data(macd_100)
#'
#' # use one column for testing
#' x <- macd_100[ ,2]
#'
#'
#' mt_evaluate(x = x,
#'             path_model = path_model,
#'             num_cols = 64,
#'             timeframe = 60)
#'
#' h2o.shutdown(prompt = FALSE)
#'
#' #set delay to insure h2o unit closes properly before the next test
#' Sys.sleep(5)
#'
#' }
#'
mt_evaluate <- function(x, path_model, num_cols, timeframe){


  requireNamespace("h2o", quietly = TRUE)

  # generate a file name for model
  m_name <- paste0("DL_Classification", "_", timeframe, "M")
  m_path <- file.path(path_model, m_name)

  # load models
  m1 <- h2o::h2o.loadModel(m_path)

  #normalize macd value if we are dealing with JPY pairs
  if(stringr::str_detect(names(x), 'JPY')){
    x <- x/100
  }

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
