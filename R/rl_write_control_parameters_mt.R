#' Function to find and write the best control parameters.
#'
#' @description This function is supposed to run on a weekly basis. Purpose of this function is to perform RL and
#' trading simulation and find out the best possible control parameters for the RL function.
#'
#' @param x - dataset containing the trading results for one trading robot
#' @param path_control_files - path where control parameters will be saved
#' @param num_trades_to_consider - number of last trades to use for RL modeling simulations, default value 100
#'
#' @details Function is used by the R script Adapt_RL_MT_control.R
#'
#' @return Function writes best control parameters to be used by the Reinforcement Learning Function
#'
#' @export
#'
#' @author (C) 2019, 2021 Vladimir Zhbanko
#'
#' @examples
#'
#' \donttest{
#' # test lasts 15 sec:
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#' library(dplyr)
#' library(readr)
#' library(ReinforcementLearning)
#' library(magrittr)
#' library(lazytrade)
#' data(trading_systemDF)
#'
#' # use optimal control parameters found by auxiliary function
#' rl_write_control_parameters_mt(x = trading_systemDF,
#'                                path_control_files = dir,
#'                                num_trades_to_consider = 100)
#' }
#'
#'
rl_write_control_parameters_mt <- function(x,
                                           path_control_files,
                                           num_trades_to_consider = 100){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

# delete DF_RES if it is exist
if(exists("DF_RES")){rm(DF_RES)}

  # only consider last 100 observations in the input dataset to make code running faster
  x <- x %>%
    dplyr::arrange(OrderCloseTime) %>%
    tail(num_trades_to_consider)

# sum(x$Profit)
#states <- c("tradewin", "tradeloss")
states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
# alpha - learning rate      0.1 <- slow       | fast        -> 0.9
# gamma - reward rate        0.1 <- short term | long term   -> 0.9
# epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
#control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)

# Create RL models for all the space
Alpha <- seq(from = 0.1, to = 0.9, by = 0.4)
Gamma <- seq(from = 0.1, to = 0.9, by = 0.4)
Epsyl <- seq(from = 0.1, to = 0.9, by = 0.4)

for (ALF in Alpha) {
  for (GAM in Gamma) {
    for (EPS in Epsyl) {
      # assign values to test or debug
      # ALF <- 0.1
      # GAM <- 0.1
      # EPS <- 0.1
      control <- list(alpha = ALF, gamma = GAM, epsilon = EPS)
      # retrieve RL model Q values progress
      DF_RESEARCH <- lazytrade::rl_log_progress_mt(x = x,states = states, actions = actions, control = control)
      # create object where all data can be aggregated
      if(!exists("DF_RES")){DF_RES <- DF_RESEARCH} else {
        DF_RES <- dplyr::bind_rows(DF_RES, DF_RESEARCH) }

    }
  }
}

# object DF_RES is generated, next goal is to simulate how the trading would go on Terminal 3.
# which means that separate column with trading result will be generated.
# this column will simulate trades commanded by RL policy in Terminal 3

# generate simulated trades in T3
DF_RES1 <- DF_RES %>%
  # create column with policy decision (1 - command on to trade in T3, 0 - no trade)
  dplyr::mutate(T3_trigger = ifelse(rewardseq.ON > rewardseq.OFF & rewardseq.ON > 0, 1, 0)) %>%
  # create shifted column with reward. This is to simulate future trades decisions of RL model
  dplyr::mutate(Prev_result = lag(totreward, 6)) %>%
  # TDL generate column with PNL in T3, this is based on trades executed by RL trigger
  dplyr::mutate(T3_PnL = ifelse(T3_trigger == 1, totreward, 0)) %>%
  # group by all parameters and generate sum of column T3_PnL
  dplyr::group_by(alpha,gamma,epsilon) %>%
  #summarise
  dplyr::summarise(Tot_pnlT3 = sum(T3_PnL)) %>%
  # find rows where Tot_pnlT3 column has a highest values
  #slice(which.max(Tot_pnlT3)) %>%
  # order
  dplyr::arrange(desc(Tot_pnlT3)) %>%
  # get the first one
  utils::head(1) %>%
  # select only control parameters
  dplyr::select(alpha, gamma, epsilon) %>%
  # convert to list
  as.list()

## We have the best control parameters

# record to file, use magic number as name
  # extract current magic number to be used as a file name
  m_number <- x %$% MagicNumber.x %>% head(1)
  # write rds file with control parameters
  readr::write_rds(DF_RES1, paste0(path_control_files,"/", m_number, ".rds"))



}

