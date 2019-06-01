# -------------------------
# Perform Data Manipulation for RL
# -------------------------
# FUNCTION data_for_RL
# PURPOSE: This function will take dataframe with trade result and it will prepare data four RL function
# Function uses cumulative sum principle to define the output action for Reinforcement Learning
# The NextState for every trade will be obtained from the simple profit checking

# Function was created to make code more compact
# x <- read_rds("test_data_4RL.rds")
# Notabene: No fail safe mechanism at the moment! Use on your own risk!!!

data_4_RL_slave <- function(x, all_trades = TRUE, num_trades = 20){

if(all_trades == TRUE){
  
trading_systemDFRL <- x %>% 
  group_by(MagicNumber) %>% 
  # arrange as ascending
  arrange(OrderCloseTime) %>% 
  # create column with cumulative profit
  mutate(csum=cumsum(Profit)) %>% 
  # create column State
  mutate(NextState = ifelse(Profit>0, "tradewin",
                            ifelse(Profit<0, "tradeloss", NA)),
         # this terminal is always have action "ON"...
         Action = "ON",
         Reward =  Profit,
         State = lag(NextState)) %>% # State column will be shifted down
  # remove row with empty data
  na.omit() %>% 
  ungroup() %>% # to get rid of grouping column
  select(State, Action, Reward, NextState) %>% 
  as.data.frame.data.frame() # ReinforcementLearning function seems to only work with 'dataframe'

return(trading_systemDFRL)
}
  
if(all_trades == FALSE){
  
  ### ** RECENT TRADES BY THIS SYSTEM **
  # add additional column with cumulative profit # group_by(id)%>%mutate(csum=cumsum(value))
  trading_systemDFRL <- x %>% 
    group_by(MagicNumber) %>% 
    # arrange as ascending
    arrange(OrderCloseTime) %>% 
    # we will always consider more recent history
    tail(num_trades) %>% 
    # create column with cumulative profit
    mutate(csum=cumsum(Profit)) %>% 
    # create column State
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           # this terminal is always have action "ON"...
           Action = "ON",
           Reward =  Profit,
           State = lag(NextState)) %>% # State column will be shifted down
    # remove row with empty data
    na.omit() %>% 
    ungroup() %>% # to get rid of grouping column
    select(State, Action, Reward, NextState) %>% 
    as.data.frame.data.frame() # ReinforcementLearning function seems to only work with 'dataframe'
  
  return(trading_systemDFRL)
}  

}