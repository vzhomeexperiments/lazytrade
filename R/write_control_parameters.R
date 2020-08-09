#' Function to find and write the best control parameters.
#'
#' @description This function is supposed to run on a weekly basis. Purpose of this function is to perform RL and
#' trading simulation and find out the best possible control parameters for the RL function.
#'
#' @param x - dataset containing the trading results for one trading robot
#' @param path_control_files - path where control parameters will be saved
#'
#' @return Function writes best control parameters to be used by the Reinforcement Learning Function
#'
#' @details Function is used by the R script Adapt_RL_control.R
#'
#' @export
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @examples
#'
#' \donttest{
#' #test lasts 15 sec:
#' library(dplyr)
#' library(readr)
#' library(ReinforcementLearning)
#' library(magrittr)
#' data(data_trades)
#' write_control_parameters(data_trades, path_control_files = tempfile())
#'
#' }
#'
write_control_parameters <- function(x, path_control_files){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)

# delete DF_RES if it is exist
if(exists("DF_RES")){rm(DF_RES)}

# perform set of operations for every trading system

# sum(x$Profit)
states <- c("tradewin", "tradeloss")
actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
# alpha - learning rate      0.1 <- slow       | fast        -> 0.9
# gamma - reward rate        0.1 <- short term | long term   -> 0.9
# epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
#control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)

# Create RL models for all the space
Alpha <- seq(from = 0.1, to = 0.9, by = 0.2)
Gamma <- seq(from = 0.1, to = 0.9, by = 0.2)
Epsyl <- seq(from = 0.1, to = 0.9, by = 0.2)

for (ALF in Alpha) {
  for (GAM in Gamma) {
    for (EPS in Epsyl) {
      # assign values to test or debug
      # ALF <- 0.1
      # GAM <- 0.1
      # EPS <- 0.1
      control <- list(alpha = ALF, gamma = GAM, epsilon = EPS)
      # retrieve RL model Q values progress
      DF_RESEARCH <- lazytrade::log_RL_progress(x = x,states = states, actions = actions, control = control)
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
  dplyr::mutate(Prev_result = lag(totreward, 2)) %>%
  # TDL generate column with PNL in T3, this is based on trades executed by RL trigger
  dplyr::mutate(T3_PnL = ifelse(T3_trigger == 1 & Prev_result > 0 & trstate == "tradewin", totreward,
                         ifelse(T3_trigger == 1 & Prev_result < 1 & trstate == "tradeloss", totreward,0))) %>%
  # group by all parameters and generate sum of column T3_PnL
  dplyr::group_by(alpha,gamma,epsilon) %>%
  #summarise
  dplyr::summarise(Tot_pnlT3 = sum(T3_PnL)) %>%
  # find rows where Tot_pnlT3 column has a highest values
  #slice(which.max(Tot_pnlT3)) %>%
  # order
  dplyr::arrange(desc(Tot_pnlT3)) %>%
  # get the first one
  head(1) %>%
  # select only control parameters
  dplyr::select(alpha, gamma, epsilon) %>%
  # convert to list
  as.list()

## We have the best control parameters

# record to file
# recording control parameters for this system (this function is intended to run 1x week)

  # create directory where to write files if not exists yet
  if(!dir.exists(path_control_files)){
    dir.create(path_control_files)
  }
  # write the best control parameters, use magic number as name
  # extract current magic number to be used as a file name
  m_number <- x %$% MagicNumber %>% head(1)
  # write rds file with control parameters
  readr::write_rds(DF_RES1, paste0(path_control_files,"/", m_number, ".rds"))



}

