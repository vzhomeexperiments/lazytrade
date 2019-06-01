# -------------------------
# Perform RL and generate model policy
# -------------------------
# FUNCTION generate_RL_policy

#' This function will perform Reinforcement Learning using Trading Data. It should start working directly from the start
#'
#' @param x - Dataframe containing trading data 
#' @param states - possible states for Reinforcement Learning
#' @param actions - possible actions
#' @param control - control parameters
#'
#' @return Function returns data frame with reinforcement learning model policy
#' @export
#'
#' @examples
generate_RL_policy <- function(x, states, actions, control){
  require(tidyverse)
  require(ReinforcementLearning)
  # uncomment to debug code inside the function
  # x <- read_rds("test_data/data_trades_markettype.rds")
  # x <- trading_systemDF
  # rm(model, df_tupple)
  # Define state and action sets for Reinforcement Learning
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
  for (i in 1:nrow(x)) {
    # i <- 1
    # State 
    State <- x[i,]$MarketType
    # predict on i
    Action <- policy(model)[x[i,]$MarketType]
    # reward
    Reward <- x[i,]$Profit
    # next state
    NextState <- x[i+1, ]$MarketType
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
    #print(i)
  }
  # extract custom policy from the obtained dataset
  df_Q <- model$Q %>% as.data.frame() %>% 
    # create column with market periods
    mutate(MarketType = row.names(.)) %>% 
    # interpret policy as defined logic, value at ON must be >= 0!
    mutate(Policy = ifelse(ON <= 0, "OFF", ifelse(ON > OFF, "ON", ifelse(OFF > ON, "OFF", NA)))) %>% 
    select(MarketType, Policy)
  
   #plot(model)
   return(df_Q)

}