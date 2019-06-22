#' Load Asset Data Function
#'
#' @description Function imports file and changes date column type. Function return the dataframe with trade data.
#' -> update: data_deepth argument was added to optimize code and separate data needed to train and score the model
#'
#' @details https://www.udemy.com/self-learning-trading-robot/?couponCode=LAZYTRADE7-10
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
#' \dontrun{
#'
#' # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files"
#' # path_terminal <- file.path(getwd(), "test_data")
#' # trade_log_file <- "AI_CP"
#' # trade_log_file <- "AI_Macd"
#' # time_period <- 1
#' # data_deepth <- "50000"
#' library(tidyverse)
#' library(lubridate)
#' prices <- load_asset_data(path_terminal = "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files",
#'                     trade_log_file = "AI_CP",
#'                     time_period = 1,
#'                     data_deepth = "50000")
#'
#' }
#'
load_asset_data <- function(path_terminal, trade_log_file, time_period = 1, data_deepth = 50000){

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  DFT1 <- try(read_csv(file = file.path(path_terminal, paste0(trade_log_file, time_period, "-", data_deepth, ".csv")),
                       col_names = F),
              silent = TRUE)
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$X1 <- ymd_hms(DFT1$X1)
    if(trade_log_file == "AI_CP"){
    ## divide JPY pairs by 100
    DFT2 <- DFT1[ , c(8,10,18,22,24,25,26)]/100
    DFT3 <- DFT1[, -c(8,10,18,22,24,25,26)] %>% bind_cols(DFT2) %>% select(1,2,3,4,5,6,7,8,
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
