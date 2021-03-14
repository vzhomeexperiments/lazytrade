#' Import Data file with Trade Logs to R.
#'
#' @description
#'
#' Function is capable to import file with executed trades log.
#' Files do not have column headers hence function will take care to name columns as well as to perform relevant cleansing
#'
#' `r lifecycle::badge('stable')`
#'
#' @param path_sbxm - String, Path to the sandbox with the log file where the file with data is written
#' @param trade_log_file - String, File name where the order results are written
#'
#'
#' @return Function will return the dataframe with trade data and automatically set proper column types
#' @export
#'
#' @author (C) 2019, 2020 Vladimir Zhbanko
#'
#' @examples
#'
#' library(lazytrade)
#' library(dplyr)
#' library(readr)
#' library(lubridate)
#'
#' path_sbxm <- normalizePath(tempdir(),winslash = "/")
#'
#' file.copy(from = system.file("extdata", "OrdersResultsT1.csv", package = "lazytrade"),
#'           to = file.path(path_sbxm, "OrdersResultsT1.csv"), overwrite = TRUE)
#'
#' DFT1 <- import_data(path_sbxm = path_sbxm,
#'                     trade_log_file = "OrdersResultsT1.csv")
#'
#'
#'
import_data <- function(path_sbxm, trade_log_file){
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)


  DFT1 <- try(readr::read_csv(file = file.path(path_sbxm, trade_log_file),
                              col_names = c("MagicNumber", "TicketNumber", "OrderStartTime",
                                     "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                       col_types = "iiccdci"),
              silent = TRUE)


  # evaluate content of the table, stop function if there was an error to read file
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  # clean data if table is not empty
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$OrderStartTime <- lubridate::ymd_hms(DFT1$OrderStartTime)
    DFT1$OrderCloseTime <- lubridate::ymd_hms(DFT1$OrderCloseTime)
    DFT1$OrderType      <- as.factor(DFT1$OrderType)
    # code below removes duplicates
    DFT1 <- unique(DFT1)

    return(DFT1)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
    }


}
