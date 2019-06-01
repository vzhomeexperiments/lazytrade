#' Apply Reinforcement Learning Policy.
#' Function that uses trading system magic number, model and uses policy of the model to write decision to the file
#'
#' Function will write a 'decision' to the csv file specific for each Expert Advisor
#'
#' @param trading_system - magic number id, typically 7 digit number according to Lazy Trading convention
#' @param model - Reinforcement learning model,
#' @param last_trade - status of the latest trade. Can be 'tradewin' or 'tradeloss'
#' @param path_sandbox - path of the MT4 terminal where trigger decision needs to be written
#'
#' @return - Function does not return anything
#' @export - Function writes the file to the specified path
#'
#' @examples apply_policy(trading_system = trading_system, model = model_old,
#'                        last_trade = latest_trade, path_sandbox = path_T4)
#'
#'
apply_policy <- function(trading_system, model, last_trade, path_sandbox){
  # debugging
  # trading_system
  # model <- read_rds("test_data/apply_policy_model.rds")
  # last_trade <- "tradeloss"
  # last_trade <- "tradewin"
  # check the sign of the ON function for policy state
  hash_val <- model$Q %>% as.data.frame() %>% select(ON) %>% filter(row.names(.) %in% last_trade)
# recover decision based on updated policy
  decision <- policy(model)[last_trade]
# derive which terminal should be enabled (using path to sandbox) and using variable 'addition'
  is_T3 <- str_detect(path_sandbox, "Terminal3")
  if(is_T3 == TRUE) { addition <- 200 }
  is_T4 <- str_detect(path_sandbox, "Terminal4")
  if(is_T4 == TRUE) { addition <- 300 }
# Check Q value, only consider good values when hash_value is positive!!!
# Motivation: Q value for Action 'ON' can be -2.5 and for Action 'OFF' can be -5...
  # in this case Policy will select 'ON' while it can still be a lousy system

if(decision == "ON" && hash_val > 0){
  # build dataframe for sending to the file
  decision_DF <- data.frame(MagicNumber = trading_system + addition,
                            IsEnabled = 1)
  # -------------------------
  # Write Decision/Update Policy
  # -------------------------
  # write the file for MQL4 usage
  write.csv(decision_DF, file = paste0(path_sandbox, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
            row.names = FALSE)

} else {
  decision_DF <- data.frame(MagicNumber = trading_system + addition,
                            IsEnabled = 0)
  # -------------------------
  # Write Decision/Update Policy
  # -------------------------
  # write the file for MQL4 usage
  write.csv(decision_DF, file = paste0(path_sandbox, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
            row.names = FALSE)
}

}
