#' Import Market Type related Data to R from the Sandbox
#'
#' @description Function imports file from the MetaTrader sandbox. Function performs necessary cleansing of the data column types
#'
#' @param path_sbxm - String, Path to the sandbox with the log file (master terminal)
#' @param system_number - magic number id of the trading system
#'
#' @return function returns the data frame with 5 columns including market type code
#' @export
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @examples
#'
#' library(dplyr)
#' library(readr)
#' library(lazytrade)
#'
#' path_sbxm <- normalizePath(tempdir(),winslash = "/")
#'
#' file.copy(from = system.file("extdata", "MarketTypeLog9139106.csv", package = "lazytrade"),
#'           to = file.path(path_sbxm, "MarketTypeLog9139106.csv"), overwrite = TRUE)
#'
#' DF1 <- mt_import_data(path_sbxm = path_sbxm,
#'                       system_number = 9139106)
#'
#'
#'
mt_import_data <- function(path_sbxm, system_number){

  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  trade_log_file <- paste0("MarketTypeLog", system_number, ".csv")
  DFT1 <- try(readr::read_csv(file = file.path(path_sbxm, trade_log_file),
              col_names = c("MagicNumber", "TicketNumber", "MarketTypeCode","OrderDurationMin", "MarketType"),
              col_types = "iiiic"),
              silent = TRUE)

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
