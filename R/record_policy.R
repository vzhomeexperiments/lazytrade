#' Record Reinforcement Learning Policy.
#'
#' @description Function will write a policy 'decision' to the csv file specific for each Expert Advisor
#'
#'
#' @param x - Dataframe containing columns MarketType and Policy
#' @param trading_system - character vector of lenght 1 with Trading System Magic Number information
#' @param path_terminal - path to the sandbox where this Policy/Decision must be written
#' @param last_result - character vector of the last result of the trade
#'
#' @return function creates csv file
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(TradeStatePolicy)
#' trading_system <- 8118101
#' path_terminal <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"
#' record_policy(x = TradeStatePolicy, trading_system = trading_system, path_terminal = path_T4)
#'
#' }
#'
#'
record_policy <- function(x, last_result, trading_system, path_terminal){
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)
  # debugging
  # trading_system <- 8118101
  # last_result <- "tradeloss"
  # x <- read_rds("_TEST_DATA/TradeStatePolicy.rds")
  # x <- policy_tr_systDF
  # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"
# derive which terminal should be enabled (using path to sandbox) and using variable 'addition'
  is_T3 <- str_detect(path_terminal, "Terminal3")
  if(is_T3 == TRUE) { addition <- 200 }
  is_T4 <- str_detect(path_terminal, "Terminal4")
  if(is_T4 == TRUE) { addition <- 300 }

  # analyse latest result and extract action based on the RL policy
    y <- x %>% filter(TradeState == last_result) %$% Policy

  if(y == "ON"){
    # build dataframe for sending to the file
    decision_DF <- data.frame(MagicNumber = trading_system + addition,
                              IsEnabled = 1)
    # -------------------------
    # Write Decision/Update Policy
    # -------------------------
    # write the file for MQL4 usage
    write.csv(decision_DF, file = paste0(path_terminal, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
              quote = FALSE,
              row.names = FALSE)

  } else {
    decision_DF <- data.frame(MagicNumber = trading_system + addition,
                              IsEnabled = 0)
    # -------------------------
    # Write Decision/Update Policy
    # -------------------------
    # write the file for MQL4 usage
    write.csv(decision_DF, file = paste0(path_terminal, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
              quote = FALSE,
              row.names = FALSE)
  }


}
