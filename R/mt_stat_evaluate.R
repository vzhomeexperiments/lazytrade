#' Function to prepare and score data, finally predict current market type using pre-trained classification model
#'
#' @description PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
#'                       to find out specific market type of the financial asset
#'                       it will also discard bad result outputting -1 if it is the case
#'
#'
#' @details it is mandatory to switch on the virtual h2o machine with h2o.init()
#' also to shut it down with h2o.shutdown(prompt = F)
#'
#' @author (C) 2021 Vladimir Zhbanko
#'
#' @param x - dataframe with one column containing asset indicator in the time descending order,
#' typically 64 or more values
#'
#' @param path_model       String, path to the model
#' @param num_bars         Integer, Number of bars used to perform transformation
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
#' library(stats)
#' library(moments)
#'
#' path_model <- normalizePath(tempdir(),winslash = "/")
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#'
#' # start h2o engine (using all CPU's by default)
#' h2o.init(nthreads = 2)
#'
#' data(price_dataset_big)
#' data <- head(price_dataset_big, 500) #reduce computational time
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
#'               num_nn_options = 3,
#'               is_cluster = TRUE)
#'
#'
#' # Use sample data
#' data(price_dataset)
#'
#' # use one column for testing
#' x <- price_dataset[ ,2]
#'
#'
#' mt_stat_evaluate(x = x,
#'                  path_model = path_model,
#'                  num_bars = 64,
#'                  timeframe = 60)
#'
#' h2o.shutdown(prompt = FALSE)
#'
#' #set delay to insure h2o unit closes properly before the next test
#' Sys.sleep(5)
#'
#' }
#'
mt_stat_evaluate <- function(x, path_model, num_bars, timeframe){


  requireNamespace("h2o", quietly = TRUE)

  # generate a file name for model
  m_name <- paste0("DL_Classification", "_", timeframe, "M")
  m_path <- file.path(path_model, m_name)

  # load models
  m1 <- h2o::h2o.loadModel(m_path)

    # Calculate statistics based on the method in the function mt_stat_transf

  dat11 <- x %>% head(num_bars)

      dfr12 <- dat11 %>% as.data.frame()


      lg12 <- apply(dfr12, 2, lazytrade::dlog)

      #Q1 vector with first quantile
      q1 <- apply(lg12, 2,stats::quantile, 0.25)

      #Q2 vector with second quantile
      q2 <- apply(lg12, 2,stats::quantile, 0.5)

      #Q3 vector with third quantile
      q3 <- apply(lg12, 2,stats::quantile, 0.75)

      # vector with kurtosis
      k1 <- apply(lg12, 2,moments::kurtosis)

      # vector with skewness
      s1 <- apply(lg12, 2, moments::skewness)

      ## combine these vectors
      dfC <- data.frame(Q1 = q1,
                        Q2 = q2,
                        Q3 = q3,
                        K1 = k1#,
                        #S1 = s1
                        )




    dfC <- dfC %>% transform(M_T = "BUN")

    # load the dataset to h2o
    test  <- h2o::as.h2o(x = dfC, destination_frame = "test")

    # retrieve the predicted value of the market type
    e1 <- h2o::h2o.predict(m1, test) %>% as.data.frame()

    # obtained values will be analysed in the scoring script

    # output result of prediction from the function
    return(e1)



}
