# import data function
# (C) 2018 Vladimir Zhbanko
# -------------------------
# Import Data to R
# -------------------------
#' Load Data Function
#' https://www.udemy.com/self-learning-trading-robot/?couponCode=LAZYTRADE7-10
#' 
#' Function imports file and change date column type. Function return the dataframe with trade data. 
#' -> update: data_deepth argument was added to optimize code and separate data needed to train and score the model
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
#' prices <- load_data(path_terminal = "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/",
#'                     trade_log_file = "AI_CP", 
#'                     time_period = 1,
#'                     data_deepth = "14200")
#' 
load_data <- function(path_terminal, trade_log_file, time_period = 1, data_deepth = 14200){
  # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/"
  # path_terminal <- file.path(getwd(), "test_data")
  # trade_log_file <- "AI_CP"
  # trade_log_file <- "AI_Macd"
  # time_period <- 1
  # data_deepth <- "100000"
  require(tidyverse)
  require(lubridate)
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
    DFT3 <- DFT1[, -c(8,10,18,22,24,25,26)] %>% bind_cols(DFT2) %>% select(X1,X2,X3,X4,X5,X6,X7,X8,
                                                                           X9,X10,X11,X12,X13,X14,X15,
                                                                           X16,X17,X18,X19,X20,X21,X22,
                                                                           X23,X24,X25,X26,X27,X28,X29)
    return(DFT3)
    }
    
    return(DFT1)
  } else {
    stop("Data log is empty!", call. = FALSE)
    }

}





















