#' Load and Prepare Asset Data
#'
#' @description Function imports file with financial asset data. Each column represent one asset, rows represent observations.
#' Values in specific columns will be normalized by dividing them by 100. This is specifically done for pairs with JPY.
#' In addition, X1 column will be converted to the ymd_hms format
#'
#' `r lifecycle::badge('deprecated')`
#'
#' @details Works for both price and indicator values, function parameters allowing to import different files.
#' File names are selected to account different time periodicity and amount of the data
#'
#' @param path_terminal - path to the MT4 terminal, string
#' @param trade_log_file - csv file name where the data is stored, without ".csv"
#' @param time_period - data periodicity in minutes, can be 1, 15, 60
#' @param data_deepth - collected data deepth in rows. describe how many rows in original file to read
#'
#' @return - dataframe with asset data in columns where X1 column is in a POSIXct format
#' @export
#'
#' @examples
#'
#' library(readr)
#' library(dplyr)
#' library(lubridate)
#' library(magrittr)
#' path_terminal <- system.file("extdata", package = "lazytrade")
#'
#' # load and prepare prices data
#' prices <- load_asset_data(path_terminal = path_terminal,
#'                           trade_log_file = "AI_CP",
#'                           time_period = 60,
#'                           data_deepth = "300")
#'
#' # load and prepare indicator data
#' macd <- load_asset_data(path_terminal = path_terminal,
#'                         trade_log_file = "AI_Macd",
#'                         time_period = 60,
#'                         data_deepth = "300")
#'
#'
load_asset_data <- function(path_terminal, trade_log_file, time_period = 1, data_deepth = 50000){

  requireNamespace("readr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)

  lifecycle::deprecate_warn(
    "0.4.5",
    "load_asset_data()",
    details = "This function is not used."
  )


  DFT1 <- try(readr::read_csv(file = file.path(path_terminal, paste0(trade_log_file, time_period, "-", data_deepth, ".csv")),
                       col_names = F),
              silent = TRUE)
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  #add one column filled with zeroes   DFT1$X3 <- 0
  #detect if some columns are filled with zeroes...
  Z_detect <- lapply(DFT1, function(x) all(x == 0)) %>% as.data.frame()
  #evaluate results, if any values are equal to 0 provide a warning
  if(any(Z_detect == TRUE)){warning("Warning, one or more columns in the datafile contains zeroes")}

  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$X1 <- lubridate::ymd_hms(DFT1$X1)
    if(trade_log_file == "AI_CP"){
    ## divide JPY pairs by 100
    DFT2 <- DFT1[ , c(8,10,18,22,24,25,26)]/100
    DFT3 <- DFT1[, -c(8,10,18,22,24,25,26)] %>%
      dplyr::bind_cols(DFT2) %>%
      dplyr::select(1,2,3,4,5,6,7,8,
                    9,10,11,12,13,14,15,
                    16,17,18,19,20,21,22,
                    23,24,25,26,27,28,29)
    return(DFT3)
    }

    return(DFT1)
  } else {
    stop("Data log is empty!", call. = FALSE)
    }

}
