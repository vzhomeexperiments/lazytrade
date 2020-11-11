#' Function check_if_optimize.
#'
#' @description Purpose of this function is to verify trading system functionality by analysing
#' profit factor on the last trades. Whenever trading robot has profit factor value below certain limit
#' function will write a file log indicating which trading systems need to be maintained.
#'
#' Learn by example how to manipulate data
#'
#' @details Whenever there will be not enough trades then empty file will be written to the destination
#'
#' @param x - dataframe containing trading results
#' @param system_list - dataframe containing a table with magic numbers used by robots. Stored in file Setup.csv
#' @param path_data - string, path to the folder where optimization file should be written
#' @param num_trades_to_consider - Number of trades to calculate profit factor
#' @param profit_factor_limit - Limit below which trading robot is considered not working properly
#' @param write_mode - When true function will write result to the file located in the temporary directory
#'
#' @return function returns a dataframe with systems that should be optimized
#'
#' @author (C) 2019,2020 Vladimir Zhbanko
#'
#' @export
#'
#' @examples
#'
#' library(lazytrade)
#' library(magrittr)
#' library(dplyr)
#' library(readr)
#' library(lubridate)
#'
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' file.copy(from = system.file("extdata", "Setup.csv", package = "lazytrade"),
#'           to = file.path(path_data, "Setup.csv"), overwrite = TRUE)
#'
#' system_list <- read_csv(file.path(path_data, "Setup.csv"))
#'
#' data(profit_factorDF)
#'
#'
#' # without writing to the file
#' check_if_optimize(x = profit_factorDF,
#'                   system_list = system_list,
#'                   path_data,
#'                   num_trades_to_consider = 3,
#'                   profit_factor_limit = 0.8,
#'                   write_mode = TRUE)
#'
#'
check_if_optimize <- function(x,
                              system_list,
                              path_data,
                              num_trades_to_consider = 3,
                              profit_factor_limit = 0.7,
                              write_mode = FALSE){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)


  y <- x %>%  # filtered to contain last 20 orders for each system
    dplyr::ungroup() %>%
        dplyr::group_by(MagicNumber) %>%
        dplyr::arrange(MagicNumber, desc(OrderCloseTime)) %>%
        dplyr::filter(row_number() <= num_trades_to_consider+1) %>%
        lazytrade::get_profit_factorDF(num_trades_to_consider) %>%
        dplyr::ungroup() %>%
        dplyr::filter(PrFact < profit_factor_limit) %>%
        dplyr::select(MagicNumber, PrFact) %>%
        dplyr::mutate(ToOptimize = 1) %>%
        dplyr::inner_join(y = system_list, by = c("MagicNumber" = "Magic"))

    if(write_mode){
        readr::write_csv(y, file = file.path(path_data, paste0(Sys.Date(), "-Re-Train", ".csv")))
    } else {return(y)}




}
