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
#' @param path_trading_robot - path of trading robot repository. must contain folder TEST with file Setup.csv.
#' File Setup.csv contains a table with magic numbers under test
#'
#' @param num_trades_to_consider - Number of trades to calculate profit factor
#' @param profit_factor_limit - Limit below which trading robot considered not working properly
#' @param demo_mode - When true function uses package test dataset
#' @param write_mode - When true function will write result to the file located in the temporary directory
#'
#' @return function returns a dataframe with systems that should be optimized
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @export
#'
#' @examples
#'
#' library(lazytrade)
#' library(tidyverse)
#' library(lubridate)
#' DFT1 <- import_data(trade_log_file = system.file("extdata",
#'                                                  "OrdersResultsT1.csv",
#'                                                  package = "lazytrade"),
#'                     demo_mode = TRUE)
#'
#'
#' # without writing to the file
#' DFT1 %>% check_if_optimize(num_trades_to_consider = 10,
#'                            profit_factor_limit = 1.2,
#'                            demo_mode = TRUE,
#'                            write_mode = FALSE)
#' \donttest{
#' # function will write to the temporary file
#' DFT1 %>% check_if_optimize(num_trades_to_consider = 10,
#'                            profit_factor_limit = 1.2,
#'                            demo_mode = TRUE,
#'                            write_mode = TRUE)
#' }
#'
check_if_optimize <- function(x, path_trading_robot = "",
                              num_trades_to_consider = 10,
                              profit_factor_limit = 0.7,
                              demo_mode = FALSE,
                              write_mode = FALSE){
  requireNamespace("tidyverse", quietly = TRUE)


  if(!demo_mode){

  y <- x %>%  # filtered to contain last 20 orders for each system
        group_by(MagicNumber) %>%
        arrange(MagicNumber, desc(OrderCloseTime)) %>%
        filter(row_number() <= num_trades_to_consider+1) %>%
        get_profit_factorDF(num_trades_to_consider) %>%
        ungroup() %>%
        filter(PrFact < profit_factor_limit) %>%
        select(MagicNumber, PrFact) %>%
        mutate(ToOptimize = 1) %>%
        inner_join(y = read_csv(file = file.path(path_trading_robot,"TEST", "Setup.csv"),
                                col_types = "cci"),
                   by = c("MagicNumber" = "Magic"))
  if(write_mode){write_csv(y, path = file.path(paste0(path_trading_robot, "TEST/", Sys.Date(), "-Re-Train", ".csv")))} else {return(y)}

  } else {

  y <- x %>%  # filtered to contain last 20 orders for each system
        group_by(MagicNumber) %>%
        arrange(MagicNumber, desc(OrderCloseTime)) %>%
        filter(row_number() <= num_trades_to_consider+1) %>%
        get_profit_factorDF(num_trades_to_consider) %>%
        ungroup() %>%
        filter(PrFact < profit_factor_limit) %>%
        select(MagicNumber, PrFact) %>%
        mutate(ToOptimize = 1) %>%
        inner_join(y = read_csv(file = system.file("extdata", "Setup.csv", package = "lazytrade"),
                                col_types = "cci"),
                   by = c("MagicNumber" = "Magic"))
  if(write_mode){write_csv(y, tempfile("Re-Train", fileext = ".csv"))} else {return(y)}

  }


}
