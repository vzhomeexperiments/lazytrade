#' Function to log RL progress. Function will record Q values during updating of the model
#'
#' @param x - dataframe containing trading results
#' @param states 
#' @param actions 
#' @param control 
#'
#' @return
#' @export
#'
#' @examples
log_RL_progress <- function(x, states, actions, control){
  require(tidyverse)
  require(ReinforcementLearning)
  require(magrittr)
  # uncomment to debug code inside the function
  # x <- read_rds("_TEST_DATA/data_trades.rds")
  # x <- trading_systemDF
  # rm(model, df_tupple)
  # Define state and action sets for Reinforcement Learning
  # states <- c("tradewin", "tradeloss")
  # states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
  # actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  # control <- list(alpha = 0.7, gamma = 0.3, epsilon = 0.1)
  # add dummy tupples with states and actions with minimal reward
  d_tupple <- data.frame(State = states,
                         Action = rep(actions,length(states)),
                         Reward = rep(0,length(states)),
                         NextState = states,
                         stringsAsFactors = F)
  # generate RL model
  model <- ReinforcementLearning(d_tupple, s = "State", a = "Action", r = "Reward", 
                                 s_new = "NextState",iter = 1, control = control)
  
  # add rows of the x one by one to gradually update this model
  for (i in 2:nrow(x)) {
    # i <- 2
    # State 
    State <- x[i-1,]$MarketType 
    
    # predict on i
    Action <- policy(model)[State]
    
    # reward
    Reward <- x[i-1,]$Profit
    
    # next state
    NextState <- x[i, ]$MarketType
    
    # combine data as dataframe
    i_tupple <- data.frame(State,Action,Reward,NextState,row.names = i, stringsAsFactors = F) %>%
      # change factor column to as.character (required by RL function)
      mutate_if(is.factor, as.character)
    # join dummy tupple to current row in the new object
    if(!exists("df_tupple")){df_tupple <- bind_rows(d_tupple, i_tupple)} else {
      df_tupple <- bind_rows(df_tupple, i_tupple)
    }
    
    # update model with new data tupple
    model <- ReinforcementLearning(df_tupple, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", control = control, iter = 1, model = model)
    #model$Q
    
      # generate dataframe with reward sequence of this learning
      df_q <- data.frame(alpha = control$alpha, gamma = control$gamma, epsilon = control$epsilon, 
                         trstate = model$States, rewardseq = model$Q, totreward = model$Reward)
    
      # create dataframe that will append data to previous records
      if(!exists("df_Q")){df_Q <- df_q} else {
        df_Q <- bind_rows(df_Q, df_q)
      }
      
    #print(i)
    
  }
  
  
  #plot(model)
  # return log of RL model
  return(df_Q)
  
}
