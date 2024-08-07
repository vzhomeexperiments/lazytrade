#' Perform Statistical transformation and clustering of Market Types on the price data
#'
#' @description  Function features methods of statistical data transformation and clustering of the price data.
#' Multiple statistical properties are calculated for a defined time interval. Once combined,
#' unsupervised learning (clustering) is performed to assign several classes, see function `mt_make_model`.
#' Function allows to fully automatize financial periods classification.
#' It is possible to choose two clustering methods either kmeans or hierarchical clustering.
#'
#' `r lifecycle::badge('stable')`
#'
#' @details User can define several market type classes names however function will randomly assign
#' Market Period labels based on Unsupervised Learning. This is inconvenient however that should be compensated by
#' automated way of doing such data classification
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
#' @param clust_method        Character, option to select which clustering method to choose. Could be either
#'                            'kmeans' or 'hclust'. Default value is 'kmeans'
#' @param clust_opt           Character, option to select how to perform h clustering
#'                            "average", "single", "complete", "ward". Default value is 'complete'
#' @param rule_opt            Boolean, option to perform rule-based Market Type Assignment, defaults to TRUE
#'
#' @return Dataframe with statistically transformed and classified dataset for classification modeling
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(stats)
#' library(magrittr)
#' library(readr)
#' library(lazytrade)
#'
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' data(price_dataset_big)
#'
#' #option
#' #mt_classes = c('BUN', 'BEN', 'RAN','BUV', 'BEV', 'RAV')
#' #mt_classes = c('BUN', 'BEN', 'RAN')
#' #clust_method = 'hclust'
#' #clust_opt = 'ward'
#' 
#' #build dataset for Market Type detection without rule based check
#' ai_class_rand <- mt_stat_transf(indicator_dataset = price_dataset_big,
#'                                 num_bars = 64,
#'                                 timeframe = 60,
#'                                 path_data = path_data,
#'                                 mt_classes = c('BUN', 'BEN'),
#'                                 clust_method = 'kmeans',
#'                                 clust_opt = 'complete',
#'                                 rule_opt = FALSE)
#'
#' #use rule base check
#' ai_class_rule <- mt_stat_transf(indicator_dataset = price_dataset_big,
#'                                 num_bars = 64,
#'                                 timeframe = 60,
#'                                 path_data = path_data,
#'                                 mt_classes = c('BUN', 'BEN'),
#'                                 clust_method = 'kmeans',
#'                                 clust_opt = 'complete',
#'                                 rule_opt = TRUE)
#'
#'
mt_stat_transf <- function(indicator_dataset,
                           num_bars=64,
                           timeframe = 60,
                           path_data,
                           mt_classes,
                           clust_method = 'kmeans',
                           clust_opt = 'complete',
                           rule_opt = TRUE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  #steps:
  #grab as many price data as possible;
  #calculate properties for 28 columns
  #caracterize statistically (Q1, Q2, Q3, Kurtosis, etc)
  #scale data
  #find 6 clusters with unsupervised learning
  #re-assign labels BUN, BEV, etc
  #return clustered dataset
  #further in a separate function to make a classification model to explain data and label

  # within a separate function `mt_stat_evalute` use use last 64 bars,
  # calculate statistical inputs, use the model and predict current class...


  # split data into several lists
  nr <- nrow(indicator_dataset)
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
      
      #n1 vector with first element of selected row
      n1 <- apply(dfr12, 2,head,1)
      #n2 vector with last element of selected row
      n2 <- apply(dfr12, 2, tail,1)
      # vector with kurtosis
      #k1 <- apply(lg12, 2,moments::kurtosis)

      # vector with skewness
      #s1 <- apply(lg12, 2, moments::skewness)

      ## combine these vectors
      dfC <- data.frame(Q1 = q1,
                        Q2 = q2,
                        Q3 = q3,
                        N1 = n1,
                        N2 = n2)
                        #K1 = k1,
                        #S1 = s1
                        #)



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
      #n1 vector with first element of selected row
      n1 <- apply(dfr12, 2,head,1)
      #n2 vector with last element of selected row
      n2 <- apply(dfr12, 2, tail,1)
      # vector with kurtosis
      #k1 <- apply(lg12, 2,moments::kurtosis)

      # # vector with skewness
      # s1 <- apply(lg12, 2, moments::skewness)

      ## combine these vectors
      dfC1 <- data.frame(Q1 = q1,
                        Q2 = q2,
                        Q3 = q3,
                        N1 = n1,
                        N2 = n2)
      #K1 = k1,
      #S1 = s1
      #)



      dfC <- dfC %>% dplyr::bind_rows(dfC1)
    }

  } # end of the for loop
  
  # Create copies of the dataset for verification purposes
  dfCa <- dfC
  dfC <- dfC[, 1:3]
  
  ## performing clustering algorithm to classify this dataset into N classes
  # scale data
  dfCsc <- scale(dfC)

  # derive number of classes
  N <- length(mt_classes)
  # kmeans with N clusters
  if(clust_method == 'kmeans'){

    km_6 <- stats::kmeans(dfCsc, centers = N, nstart = 20)
    #plot(dfC, col = km_6$cluster)
    #join classes to the dataset
    dfC$M_T <- km_6$cluster
  }

  # option hclust with N clusters
  if(clust_method == 'hclust'){
    d <- stats::dist(dfCsc, method = 'euclidean')
    hc_6 <- cluster::agnes(d, method = clust_opt)
    #pltree(hc_6, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
    hc_cl <- stats::cutree(hc_6, k = N)

    dfC$M_T <- hc_cl
  }


  ## rule based assignment of classes to labels
  if(rule_opt && N==3){  
    ## derive which class is which!!!
    # join columns with price values to the dataset
    dfCa$M_T <- dfC$M_T

    # calculate summary statistics for each class
    dfC2 <- dfCa %>% 
      dplyr::mutate(dN = 1000*(N2-N1)) %>% 
      dplyr::group_by(M_T) %>% 
      dplyr::summarise(Q1mean = mean(Q1),
                       Q2mean = mean(Q2),
                       Q3mean = mean(Q3),
                       dNmean = mean(dN),
                       Nobs = n()) 
      # result of dN is probably biased 

    # max dN && dN > 0 -> BUN
        dfBUN <- dfC2 %>% 
      dplyr::slice(which.max(dNmean)) %>% 
      dplyr::select(M_T) %>% 
      dplyr::mutate(MT_A = "BUN")
        
        # min dN && dN < 0 -> BEN
    dfBEN <- dfC2 %>% 
      dplyr::slice(which.min(dNmean)) %>% 
      dplyr::select(M_T) %>% 
      dplyr::mutate(MT_A = "BEN")
    # keep them together
    dfBUNBEN <- dplyr::bind_rows(dfBUN, dfBEN)
    df_all <- dfBUNBEN %>% 
      dplyr::full_join(dfC2) %>% 
      dplyr::arrange(M_T)
    
    # remaining is RAN...
    df_all$MT_A[is.na(df_all$MT_A)] <- "RAN"
    
    # relabel the column M_T
    dfC$M_T <- factor(dfC$M_T, labels=df_all$MT_A)
    # relabel for manual checking
    dfCa$M_T <- factor(dfCa$M_T, labels = df_all$MT_A)
    
    }else{
  #rename clusters to be like desired (but classes will be unsupervised)
  dfC$M_T <- factor(dfC$M_T, labels=mt_classes)
  #plot(dfC, col = dfC$M_T)
  }
  full_path <- file.path(path_data, paste0('auto_M_T', timeframe, '.rds'))

  readr::write_rds(dfC, full_path)

  return(dfC)





}

