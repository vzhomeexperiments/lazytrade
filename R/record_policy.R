#' Record Reinforcement Learning Policy.
#' 
#' Function will write a policy 'decision' to the csv file specific for each Expert Advisor
#'
#' 
#' 
#' @param x - Dataframe containing columns MarketType and Policy
#' @param trading_system - character vector of lenght 1 with Trading System Magic Number information
#' @param path_sandbox - path to the sandbox where this Policy/Decision must be written
#' 
#' @return nothing is returned
#' @export csv file
#' @example record_policy(x = policy_tr_systDF, trading_system = trading_system, path_sandbox = path_T4)
#' 
record_policy <- function(x, trading_system, path_sandbox){
  # debugging
  # trading_system <- 8132125
  # x <- policy_tr_systDF
  # x <- read_rds("_TEST_DATA/policy_tr_systDF.rds")
  # path_sandbox <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"
# derive which terminal should be enabled (using path to sandbox) and using variable 'addition'
  is_T3 <- str_detect(path_sandbox, "Terminal3") 
  if(is_T3 == TRUE) { addition <- 200 }
  is_T4 <- str_detect(path_sandbox, "Terminal4")
  if(is_T4 == TRUE) { addition <- 300 }

  # -------------------------
  # Write Decision/Update Policy
  # -------------------------
  # write the file for MQL4 usage
  write.csv(x, file = paste0(path_sandbox, "SystemControlMT", as.character(trading_system + addition), ".csv"),
            quote = FALSE,
            row.names = FALSE)

}
