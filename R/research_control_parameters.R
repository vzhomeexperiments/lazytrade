library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 
library(ReinforcementLearning) #install.packages("ReinforcementLearning")
library(magrittr)

source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R") 
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL/research_RL_control.R")

DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv", demo_mode = T), silent = TRUE)

DFT1_sum <- DFT1 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n(),
            Mean_profit = sum(Profit)) %>% 
  arrange(desc(Num_Trades))
# vector of all systems
vector_systems <- DFT1 %$% MagicNumber %>% unique() %>% sort()

# delete DF_RES if it is exist
if(exists("DF_RES")){rm(DF_RES)}

for (i in 1:length(vector_systems)) {
  
# perform set of operations for every trading system
  
trading_system <- vector_systems[1]
# get trading summary data only for one system 
trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
# sum(trading_systemDF$Profit)
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
      
      control <- list(alpha = ALF, gamma = GAM, epsilon = EPS)
      
      DF_RESEARCH <- research_RL_control(x = trading_systemDF,states = states, actions = actions, control = control)
      
      if(!exists("DF_RES")){DF_RES <- DF_RESEARCH} else {
        DF_RES <- bind_rows(DF_RES, DF_RESEARCH) }
      
    }
  }
  
}

}

# object DF_RES is generated, next goal is to simulate how the trading would go on Terminal 3. 
# which means that separate column with trading result will be generated. 
# this column will simulate trades commanded by RL policy in Terminal 3

# generate simulated trades in T3
DF_RES1 <- DF_RES %>% 
  # create column with policy decision (1 - command on to trade in T3, 0 - no trade)
  mutate(T3_trigger = ifelse(rewardseq.ON > rewardseq.OFF & rewardseq.ON > 0, 1, 0)) %>% 
  # create shifted column with reward. This is to simulate future trades decisions of RL model
  mutate(Prev_result = lag(totreward, 2)) %>% 
  # TDL generate column with PNL in T3, this is based on trades executed by RL trigger
  mutate(T3_PnL = ifelse(T3_trigger == 1 & Prev_result > 0 & trstate == "tradewin", totreward, 
                         ifelse(T3_trigger == 1 & Prev_result < 1 & trstate == "tradeloss", totreward,0))) %>% 
  # group by all parameters and generate sum of column T3_PnL
  group_by(alpha,gamma,epsylon) %>% 
  #summarise
  summarise(Tot_pnlT3 = sum(T3_PnL)) %>% 
  # find rows where Tot_pnlT3 column has a highest values
  slice(which.max(Tot_pnlT3)) %>% 
  # order
  arrange(desc(Tot_pnlT3)) %>% 
  # get the first one
  head(1)

## We have the best control parameters


