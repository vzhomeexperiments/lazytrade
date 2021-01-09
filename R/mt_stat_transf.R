#' Perform Statistical transformation and clustering of Market Types on the price data
#'
#' @description  Function to statistically transform and cluster price data.
#' Multiple statistical properties are calculated for a defined time interval.
#' Once combined unsupervised learning is performed to assign 6 classes
#'
#'
#' @details Function will randomly assign Market Period labels based on Unsupervised Learning.
#'
#' @author (C) 2021 Vladimir Zhbanko
#' @backref Market Type research of Van Tharp Institute: <https://www.vantharp.com/>
#'
#' @param indicator_dataset   Dataframe, multiple column dataset containing price data in each column.
#'                            Each row is a time index, multiple columns are required but not strictly needed
#' @param num_bars            Integer, Number of bars used to perform transformation
#' @param timeframe           Integer, Data timeframe in Minutes, only used for naming convention
#' @param path_data           String, User path where the dataset could be stored for the future use by other function
#' @param mt_classes          Character Vector, with 2 or more Market Type classes
#'
#' @return Dataframe with statistically transformed and classified dataset for classification modeling
#' @export
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(stats)
#' library(magrittr)
#' library(readr)
#' library(lazytrade)
#' library(moments)
#'
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(price_dataset_big)
#'
#' mt_stat_transf(indicator_dataset = price_dataset_big,
#'                num_bars = 64,
#'                timeframe = 60,
#'                path_data = path_data,
#'                mt_classes = c('BUN', 'BEN', 'RAN'))
#'
#'
#'
mt_stat_transf <- function(indicator_dataset,
                           num_bars=64,
                           timeframe = 60,
                           path_data,
                           mt_classes){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  #steps:
  #grab as many price data as possible;
  #calculate properties for 28 columns
  #caracterize statistically (Q1, Q2, Q3, Kurtosis, etc)
  #scale data
  #calculate 6 clusters with knn
  #assign labels BUN, BEV, 1-6 etc
  #return' clustered dataset
  #  make a classification model to explain data and label

  # within a separate function
  # use last 64 bars, calculate Q1, Q2, ... run the model and predict class...


  # transform data using code from another function create transposed data

  nr <- nrow(indicator_dataset)
  namesdfr12 <- paste0("X", 1:num_bars) #generated names for dataset useful later in the code
  n <- num_bars
  dat11 <- indicator_dataset %>% dplyr::select(-1) %>% split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  dat11[length(dat11)] <- NULL

  # operations within the list
  for (i in 1:length(dat11)) {
    #i <- 1

    if(!exists("dfC")){
      dfr12 <- dat11[i] %>% as.data.frame()
      ##apply functions on each column:

      #create table with log return dlog <- fun{diff(log(a))}
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
      #s1 <- apply(lg12, 2, moments::skewness)

      ## combine these vectors
      dfC <- data.frame(Q1 = q1,
                        Q2 = q2,
                        Q3 = q3,
                        K1 = k1#,
                        #S1 = s1,
                        )



    } else {
      dfr12 <- dat11[i] %>% as.data.frame()
      ##apply functions on each column:

      #table with log return diff(log(a))
      lg12 <- apply(dfr12, 2, lazytrade::dlog)

      #Q1 vector with first quantile
      q1 <- apply(lg12, 2,stats::quantile, 0.25)

      #Q2 vector with second quantile
      q2 <- apply(lg12, 2,stats::quantile, 0.5)

      #Q3 vector with third quantile
      q3 <- apply(lg12, 2,stats::quantile, 0.75)

      # vector with kurtosis
      k1 <- apply(lg12, 2,moments::kurtosis)

      # # vector with skewness
      # s1 <- apply(lg12, 2, moments::skewness)

      ## combine these vectors
      dfC1 <- data.frame(Q1 = q1,
                        Q2 = q2,
                        Q3 = q3,
                        K1 = k1#,
                        #S1 = s1
                        )

      dfC <- dfC %>% dplyr::bind_rows(dfC1)
    }

  } # end of the for loop

  ## performing clustering algorithm to classify this dataset into N classes
  # scale data
  dfCsc <- scale(dfC)
  # kmeans with N clusters
  N <- length(mt_classes)
  km_6 <- stats::kmeans(dfCsc, centers = N, nstart = 20)

  #plot(dfC, col = km_6$cluster)

  ## TDL -> properly assign classes to labels

  #join classes to the dataset
  dfC$M_T <- km_6$cluster

  #rename clusters to be like desired (but classes will be unsupervised)
  dfC$M_T <- factor(dfC$M_T, labels=mt_classes)
  #plot(dfC, col = dfC$M_T)

  full_path <- file.path(path_data, paste0('auto_M_T', timeframe, '.rds'))

  readr::write_rds(dfC, full_path)

  return(dfC)





}

