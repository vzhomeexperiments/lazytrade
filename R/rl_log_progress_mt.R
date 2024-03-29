#' Function to retrieve and help to log Q values during RL progress. This function is dedicated to the situations when
#' Market Types are used as a 'states' for the Environment.
#'
#' @description Function will record Q values during the model update. These values will be used by another function
#' Function was developed to help to estimate best control parameters during optimisation process
#'
#' `r lifecycle::badge('stable')`
#'
#' @param x - dataframe containing trading results
#' @param states  - Selected states of the System
#' @param actions - Selected actions executed under environment
#' @param control - control parameters as defined in the Reinforcement Learning Package
#'
#' @return dataframe with log of RL model reward sequences during model update
#'
#' @export
#'
#' @author (C) 2020, 2021 Vladimir Zhbanko
#'
#' @examples
#'
#' # retrieve RL model Q values progress
#' library(ReinforcementLearning)
#' library(dplyr)
#' library(magrittr)
#' library(lazytrade)
#' data(trading_systemDF)
#' x <- trading_systemDF
#' states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
#' actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
#' control <- list(alpha = 0.7, gamma = 0.3, epsilon = 0.1)
#'
#' rl_log_progress_mt(x = x,states = states, actions = actions, control = control)
#'
#'
rl_log_progress_mt <- function(x, states, actions, control){
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ReinforcementLearning", quietly = TRUE)

  # add dummy tupples with states and actions with minimal reward
  d_tupple <- data.frame(State = states,
                         Action = rep(actions,length(states)),
                         Reward = rep(0,length(states)),
                         NextState = states,
                         stringsAsFactors = F)
  # generate RL model
  model <- ReinforcementLearning::ReinforcementLearning(d_tupple, s = "State", a = "Action", r = "Reward",
                                 s_new = "NextState",iter = 1, control = control)

  # add rows of the x one by one to gradually update this model
  for (i in 2:nrow(x)) {
    # i <- 2
    # State
    State <- x[i-1,]$MarketType

    # predict on i
    Action <- ReinforcementLearning::computePolicy(model)[State]

    # reward
    Reward <- x[i-1,]$Profit

    # next state
    NextState <- x[i, ]$MarketType

    # combine data as dataframe
    i_tupple <- data.frame(State,Action,Reward,NextState,row.names = i, stringsAsFactors = F) %>%
      # change factor column to as.character (required by RL function)
      dplyr::mutate_if(is.factor, as.character)
    # join dummy tupple to current row in the new object
    if(!exists("df_tupple")){df_tupple <- dplyr::bind_rows(d_tupple, i_tupple)} else {
      df_tupple <- dplyr::bind_rows(df_tupple, i_tupple)
    }

    # update model with new data tupple
    model <- ReinforcementLearning::ReinforcementLearning(df_tupple, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", control = control, iter = 1, model = model)
    #model$Q

      # generate dataframe with reward sequence of this learning
      df_q <- data.frame(alpha = control$alpha, gamma = control$gamma, epsilon = control$epsilon,
                         trstate = model$States, rewardseq = model$Q, totreward = model$Reward)

      # create dataframe that will append data to previous records
      if(!exists("df_Q")){df_Q <- df_q} else {
        df_Q <- dplyr::bind_rows(df_Q, df_q)
      }

    #print(i)

  }


  #plot(model)
  # return log of RL model
  return(df_Q)

}
