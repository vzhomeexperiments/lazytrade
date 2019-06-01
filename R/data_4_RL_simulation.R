# -------------------------
# Perform Data Generation for RL
# -------------------------
# FUNCTION data_for_RL
# PURPOSE: This function will generate data for RL
# Function will return desired performance of the system
# x <- read_rds("test_data_4RL.rds")
# Notabene: No fail safe mechanism at the moment! Use on your own risk!!!

#' Title
#'
#' @param num_trades Number of trades we intend to simulate data for
#' @param factor_lot Defines how big is the win or loss. Parameter will be multiplied by max 1 (in currency units)
#' @param success_shift Defines what we have first loss or profit. 
#'  Values from -90 to 90 will define wins in the beginning, values 90 to 180 will define loss
#' @param success_period How frequent will the performance be dropping, min value 1, 
#' higher values will define more instability of the system (win, loss, win, etc)
#' @param PnL_offset Apply profit or loss to the entire series
#'
#' @return
#' @export
#'
#' @examples
data_4_RL_simulation <- function(num_trades = 40,
                                 factor_lot = 3,
                                 success_shift = -90,
                                 success_period = 1,
                                 PnL_offset = 0){
  
  require(NISTunits)
  # num_trades <- 360
  # factor_lot <- 3
  # success_shift <- -90
  # success_period <- 1
  # PnL_offset <- 0
  
  # generate vector of desired lenght
  trades_vector <- seq(from = 0, to = 10*num_trades, by = 10)
  result_vector <- PnL_offset + factor_lot*cos(NISTdegTOradian(success_period*trades_vector+success_shift))
  #plot(result_vector)
  return(result_vector)
  
}