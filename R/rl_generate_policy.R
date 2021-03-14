#' Function performs Reinforcement Learning using the past data to generate model policy
#'
#' @description  This function will perform Reinforcement Learning using Trading Data.
#' It will suggest whether or not it is better to keep using trading systems or not.
#' Function is just using results of the past performance to generate the recommendation (not a holy grail).
#'
#' `r lifecycle::badge('stable')`
#'
#' @details Initial policy is generated using a dummy zero values.
#' This way function starts working directly from the first observation.
#' However policy 'ON' value will only be generated once the Q value is greater than zero
#'
#' @param x - Dataframe containing trading data
#' @param states - Character vector, Selected states of the System
#' @param actions - Character vector, Selected actions executed under environment
#' @param control - List, control parameters as defined in the Reinforcement Learning Package
#'
#' @return Function returns data frame with reinforcement learning model policy
#' @export
#'
#' @author (C) 2019,2021 Vladimir Zhbanko
#'
#' @examples
#'
#'   library(dplyr)
#'   library(ReinforcementLearning)
#'   library(magrittr)
#'   library(lazytrade)
#'
#'   data(data_trades)
#'   states <- c("tradewin", "tradeloss")
#'   actions <- c("ON", "OFF")
#'   control <- list(alpha = 0.7, gamma = 0.3, epsilon = 0.1)
#'   rl_generate_policy(x = data_trades,
#'                      states, actions, control)
#'
#'
rl_generate_policy <- function(x, states, actions, control){
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
    # i <- 3
    # State
    State <- x[i-1,] %>% dplyr::mutate(State = ifelse(Profit>0, "tradewin", ifelse(Profit<0, "tradeloss", NA))) %$% State

    # predict on i
    Action <- ReinforcementLearning::computePolicy(model)[State]

    # reward
    Reward <- x[i,]$Profit

    # next state
    NextState <- x[i, ] %>% dplyr::mutate(State = ifelse(Profit>0, "tradewin", ifelse(Profit<0, "tradeloss", NA))) %$% State
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
    #print(i)

  }

  #plot(model)

  # extract custom policy from the obtained dataset
  df_Q <- model$Q %>% as.data.frame() %>%
    # create column with market periods
    dplyr::mutate(TradeState = row.names(.)) %>%
    # interpret policy as defined logic, value at ON must be >= 0!
    dplyr::mutate(Policy = ifelse(ON <= 0, "OFF", ifelse(ON > OFF, "ON", ifelse(OFF > ON, "OFF", NA)))) %>%
    dplyr::select(TradeState, Policy)
    # record this object for the function debugging
    # write_rds(df_Q, "_TEST_DATA/TradeStatePolicy.rds")
   #plot(model)
   return(df_Q)

}
