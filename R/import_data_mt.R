# import data function
# (C) 2018 Vladimir Zhbanko
# -------------------------
# Import Data to R from the Sandbox


#' Import Market Type related Data to R
#'
#' @description Function imports file from the MetaTrader sandbox. Function performs necessary cleansing of the data column types
#'
#' @param system_number - magic number id of the trading system
#' @param path_terminal - path to the sandbox
#' @param trade_log_file - direct path to the log file (used for demo purposes)
#' @param demo_mode - when true, uses sample datafile stored in the package
#'
#' @return function return the data frame with 3 columns including market type code
#' @export
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @examples
#'
#' library(tidyverse)
#' DFT1 <- import_data_mt(trade_log_file = system.file("extdata", "OrdersResultsT1.csv", package = "lazytrade"), demo_mode = TRUE)
#'
#'
import_data_mt <- function(path_terminal, trade_log_file, system_number, demo_mode = FALSE){
  # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
  # system_number <- 8139124
  requireNamespace("tidyverse", quietly = TRUE)

  if(!demo_mode){

  trade_log_file <- paste0("MarketTypeLog", system_number, ".csv")
  DFT1 <- try(read_csv(file = file.path(path_terminal, trade_log_file),
               col_names = c("MagicNumber", "TicketNumber", "MarketType"),
               col_types = "iic"),
      silent = TRUE)

  } else {

    #trade_log_file <- system.file("extdata", "MarketTypeLog8132101.csv", package = "lazytrade")
    DFT1 <- try(read_csv(file = trade_log_file,
                         col_names = c("MagicNumber", "TicketNumber", "MarketType"),
                         col_types = "iic"),
                silent = TRUE)


  }

  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$MarketType <- as.factor(DFT1$MarketType)
    # removes duplicates
    DFT1 <- unique(DFT1)

    return(DFT1)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
    }

}
