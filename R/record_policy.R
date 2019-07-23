#' Record Reinforcement Learning Policy.
#'
#' @description Function will write a policy 'decision' to the csv file specific for each Expert Advisor
#'
#'
#' @param x - Dataframe containing columns MarketType and Policy
#' @param trading_system - character vector of lenght 1 with Trading System Magic Number information
#' @param path_terminal - path to the sandbox where this Policy/Decision must be written
#' @param last_result - character vector of the last result of the trade
#' @param fileName - string, desired control file prefix e.g. 'SystemControl'
#'
#' @return nothing is returned but function will write csv file to the supplied directory
#'
#' @export
#'
#' @examples
#'
#'
#' library(tidyverse)
#' library(magrittr)
#' data(TradeStatePolicy)
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#' record_policy(x = TradeStatePolicy,
#'               last_result = "tradewin",
#'               trading_system = 8118101,
#'               path_terminal = dir,
#'               fileName = "SystemControlRL")
#'
#'
#'
record_policy <- function(x, last_result, trading_system, path_terminal, fileName = "SystemControl"){
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)

# derive which terminal should be enabled (using path to sandbox) and using variable 'addition'
  is_T3 <- str_detect(path_terminal, "Terminal3")
  is_T4 <- str_detect(path_terminal, "Terminal4")
  if(is_T3 == TRUE) { addition <- 200 } else if(is_T4 == TRUE) { addition <- 300 } else { addition <- 0 }

  # analyse latest result and extract action based on the RL policy
    y <- x %>% filter(TradeState == last_result) %$% Policy

  if(y == "ON"){
    # build dataframe for sending to the file
    decision_DF <- data.frame(MagicNumber = trading_system + addition,
                              IsEnabled = 1)
    # compose file name
    composed_name <- paste0(fileName, as.character(decision_DF[1, 1]), ".csv")
    f_name <- file.path(path_terminal, composed_name)
    # -------------------------
    # Write Decision/Update Policy
    # -------------------------
    # write the file for MQL4 usage
    write.csv(decision_DF, file = f_name, quote = FALSE, row.names = FALSE)

  } else {
    decision_DF <- data.frame(MagicNumber = trading_system + addition,
                              IsEnabled = 0)

    # compose file name
    composed_name <- paste0(fileName, as.character(decision_DF[1, 1]), ".csv")
    f_name <- file.path(path_terminal, composed_name)
    # -------------------------
    # Write Decision/Update Policy
    # -------------------------
    # write the file for MQL4 usage
    write.csv(decision_DF, file = f_name, quote = FALSE, row.names = FALSE)
  }


}
