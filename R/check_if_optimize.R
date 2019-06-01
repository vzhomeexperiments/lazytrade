#' Function check_if_optimize. Purpose of this function is to verify trading system functionality by analysing
#' profit factor on the last trades. Whenever trading robot has profit factor value below certain limit
#' function will write a file log indicating which trading systems need to be maintained.
#' 
#' Whenever there will be not enough trades then empty file will be written to the destination
#'
#' @param x - dataframe containing trading results
#' @param path_trading_robot - path of trading robot repository. must contain folder TEST with file Setup.csv. 
#' File Setup.csv contains a table with magic numbers under test
#' 
#' @param num_trades_to_consider - Number of trades to calculate profit factor
#' @param profit_factor_limit - Limit below which trading robot considered not working properly
#' @param demo_mode - When true function uses data stored in the _TEST_DATA folder
#'
#' @return
#' @export
#'
#' @examples
check_if_optimize <- function(x, path_trading_robot, num_trades_to_consider = 10, profit_factor_limit = 0.7,
                              demo_mode = F){
  require(tidyverse)
  # commented code for debugging
  # source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/get_profit_factorDF.R")
  # x <- read_rds("_TEST_DATA/test_data_profit_factorDF.rds")
  # path_trading_robot <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_B/"
  # num_trades_to_consider <- 10
  # profit_factor_limit <- 0.7
  # demo_mode <- T
  
  if(!demo_mode){
  
  x %>%  # filtered to contain last 20 orders for each system
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
               by = c("MagicNumber" = "Magic")) %>% 
    write_csv(path = paste0(path_trading_robot, "TEST/", Sys.Date(), "-Re-Train", ".csv"))
    
  } else {
    
    x %>%  # filtered to contain last 20 orders for each system
      group_by(MagicNumber) %>% 
      arrange(MagicNumber, desc(OrderCloseTime)) %>% 
      filter(row_number() <= num_trades_to_consider+1) %>% 
      get_profit_factorDF(num_trades_to_consider) %>% 
      ungroup() %>% 
      filter(PrFact < profit_factor_limit) %>% 
      select(MagicNumber, PrFact) %>% 
      mutate(ToOptimize = 1) %>% 
      inner_join(y = read_csv(file = "_TEST_DATA/Setup.csv", 
                              col_types = "cci"), 
                 by = c("MagicNumber" = "Magic")) %>% 
      write_csv(path = paste0("_TEST_DATA/", Sys.Date(), "-Re-Train", ".csv"))
    
  }
    
    
}