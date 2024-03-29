#' Function performs RL and generates model policy for each Market Type
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
#' @examples
#'
#' library(dplyr)
#' library(magrittr)
#' library(ReinforcementLearning)
#' library(lazytrade)
#' data(trading_systemDF)
#' states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
#' actions <- c("ON", "OFF")
#' control <- list(alpha = 0.7, gamma = 0.3, epsilon = 0.1)
#' rl_generate_policy_mt(x = trading_systemDF,
#'                       states = states,
#'                       actions = actions,
#'                       control = control)
#'
#'
rl_generate_policy_mt <- function(x, states, actions, control){

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
  for (i in 1:nrow(x)) {
    # i <- 1
    # State
    State <- x[i,]$MarketType
    # predict on i
    Action <- ReinforcementLearning::computePolicy(model)[x[i,]$MarketType]
    # reward
    Reward <- x[i,]$Profit
    # next state
    NextState <- x[i+1, ]$MarketType
    # combine data as dataframe
    i_tupple <- data.frame(State,Action,Reward,NextState,row.names = i, stringsAsFactors = F) %>%
      # change factor column to as.character (required by RL function)
      dplyr::mutate_if(is.factor, as.character)
    # join dummy tupple to current row in the new object
    if(!exists("df_tupple")){df_tupple <- bind_rows(d_tupple, i_tupple)} else {
      df_tupple <- dplyr::bind_rows(df_tupple, i_tupple)
    }

    # update model with new data tupple
    model <- ReinforcementLearning::ReinforcementLearning(df_tupple, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", control = control, iter = 1, model = model)
    #model$Q
    #print(i)
  }
  # extract custom policy from the obtained dataset
  df_Q <- model$Q %>% as.data.frame() %>%
    # create column with market periods
    dplyr::mutate(MarketType = row.names(.)) %>%
    # interpret policy as defined logic, value at ON must be >= 0!
    dplyr::mutate(Policy = ifelse(ON <= 0, "OFF", ifelse(ON > OFF, "ON", ifelse(OFF > ON, "OFF", NA)))) %>%
    dplyr::select(MarketType, Policy)

   #plot(model)
   return(df_Q)

}
