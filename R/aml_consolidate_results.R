#' Function to consolidate model test results for several models
#'
#' @description Function is designed to evaluate test results of multiple models.
#' This is done to select only group of best models
#'
#' @details  Provide a modular facility to aggregate and update files
#'
#' @author (C) 2021 Vladimir Zhbanko
#'
#' @param timeframe           Integer, Data timeframe e.g. 60 min. This will be equal to 1 bar
#' @param path_model          String, User path where the test results were stored
#' @param path_sbxm           String, User path to the sandbox where file with strategy test results should be written (master terminal)
#' @param path_sbxs           String, User path to the sandbox where file with strategy test results should be written (slave terminal)
#' @param used_symbols        Vector, containing several financial instruments that were previously used
#'                            to test the model
#' @param min_quality         Double, value typically from 0.25 to 0.9 to select the min threshold value
#' @param get_quantile        Bool, whether or not function should return an overal value of model performances
#'                            this will be used to conditionally update only weak models
#'
#' @return Function is writing files into Decision Support System folders
#' @export
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(magrittr)
#' library(readr)
#' library(lazytrade)
#'
#' testpath <- normalizePath(tempdir(),winslash = "/")
#' path_model <- file.path(testpath, "_Model")
#' path_sbxm <- file.path(testpath, "_T1")
#' path_sbxs <- file.path(testpath, "_T3")
#' dir.create(path_model)
#' dir.create(path_sbxm)
#' dir.create(path_sbxs)
#'
#' file.copy(from = system.file("extdata", "StrTest-EURGBPM15.csv", package = "lazytrade"),
#'           to = file.path(path_model, "StrTest-EURGBPM15.csv"), overwrite = TRUE)
#'
#' file.copy(from = system.file("extdata", "StrTest-EURJPYM15.csv", package = "lazytrade"),
#'           to = file.path(path_model, "StrTest-EURJPYM15.csv"), overwrite = TRUE)
#'
#' file.copy(from = system.file("extdata", "StrTest-EURNZDM15.csv", package = "lazytrade"),
#'           to = file.path(path_model, "StrTest-EURNZDM15.csv"), overwrite = TRUE)
#'
#' file.copy(from = system.file("extdata", "StrTest-EURUSDM15.csv", package = "lazytrade"),
#'           to = file.path(path_model, "StrTest-EURUSDM15.csv"), overwrite = TRUE)
#'
#' Pairs <- c("EURGBP","EURJPY","EURNZD","EURUSD")
#'
#' aml_consolidate_results(timeframe = 15,
#'                         used_symbols = Pairs,
#'                         path_model = path_model,
#'                         path_sbxm = path_sbxm,
#'                         path_sbxs = path_sbxs,
#'                         min_quality = 0.75,
#'                         get_quantile = FALSE)
#'
#'
#' aml_consolidate_results(timeframe = 15,
#'                         used_symbols = Pairs,
#'                         path_model = path_model,
#'                         path_sbxm = path_sbxm,
#'                         path_sbxs = path_sbxs,
#'                         min_quality = 0.75,
#'                         get_quantile = TRUE)
#'
#'
aml_consolidate_results <- function(timeframe = 15,
                                    used_symbols,
                                    path_model,
                                    path_sbxm,
                                    path_sbxs,
                                    min_quality = 0.75,
                                    get_quantile = FALSE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  #failsafe
  if(min_quality <= 0.1) {stop("parameter min_quality must be greater than 0.1",
                                        call. = FALSE)}

  if(min_quality >= 0.9) {stop("parameter min_quality must be less than 0.9",
                               call. = FALSE)}

  # analyse StrTestFiles to automatically define min model quality value
  ## Analysis of model quality records
  # file names
  filesToAnalyse1 <-list.files(path = path_model,
                               pattern = paste0("M",timeframe,".csv"),
                               full.names=TRUE)


  # aggregate all files into one
  for (VAR in filesToAnalyse1) {
    # VAR <- filesToAnalyse1[1]
    if(!exists("dfres1")){dfres1 <- readr::read_csv(VAR)}  else {
      dfres1 <- readr::read_csv(VAR) %>% dplyr::bind_rows(dfres1)
    }

  }

  # find the 1st quantile by sampling 25% of the data see ?quantile
  df <- dfres1 %>%
    dplyr::mutate(qrtl = stats::quantile(MaxPerf, min_quality)) %>%
    head(1) %$% qrtl %>% as_tibble() %>% rename(FrstQntlPerf = value)

 #if option get_quantile is TRUE then we return obtained value
  if(get_quantile){
    res <- df %$% FrstQntlPerf
    return(res)
  }


  # write the value of the 1st quantile into all files

  for (VAR in filesToAnalyse1) {
    # VAR <- filesToAnalyse1[1]
    readr::read_csv(VAR) %>% dplyr::bind_cols(df) %>% readr::write_csv(VAR)
  }

  # also move these files to sandboxes of the trading terminals
  for (PAIR in used_symbols) {
    #PAIR <- 'EURUSD'
    #timeframe <- 60
    file.copy(from = file.path(path_model, paste0('StrTest-', PAIR,"M",timeframe, ".csv")),
              to = file.path(path_sbxm, paste0('StrTest-', PAIR,"M",timeframe, ".csv")),
              overwrite = TRUE)

    file.copy(from = file.path(path_model, paste0('StrTest-', PAIR,"M",timeframe, ".csv")),
              to = file.path(path_sbxs, paste0('StrTest-', PAIR,"M",timeframe, ".csv")),
              overwrite = TRUE)
  }


}


