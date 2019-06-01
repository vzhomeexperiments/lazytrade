# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# This is a dedicated script for the Lazy Trading 6th Course: Detect Market Type with Artificial Intelligence
# Copyright (C) 2019 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/your-trading-control-reinforcement-learning/?couponCode=LAZYTRADE4-10
# https://www.udemy.com/detect-market-status-with-ai/?couponCode=LAZYTRADE6-10
# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# DETAILS: Trades are analysed and RL model is created for each single Expert Advisor
#        : Q states function is calculated, whenever Action 'ON' is > than 'OFF' trade trigger will be active   
#        : Results are written to the file of the MT4 Trading Terminal
#        : Reinforcement Learning Models are created for each specific system considering 6 Market Types
#        : RL would learn to select the best Market Types to switch ON/OFF Trading Systems
# !!!!!!!!!!! #
## ATTENTION ##
# !!!!!!!!!!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# THIS CODE IS NOT COMPATIBLE WITH CODE IN FOLDER _RL/TradeTriggerRL.R !!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 
library(ReinforcementLearning) #devtools::install_github("nproellochs/ReinforcementLearning")
library(magrittr)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: Data object DFT1 contains observations
# -- Pass: DFT1_sum contains trades summary
# -- Pass: Files SystemControlXXXXXXX.csv are generated in the Terminal 3 sandbox
# -- Pass: If file "01_MacroeconomicEvent.csv" exists trade policy is overwritten
# -- Fail: DFT1 class 'try-error'
# -- Fail: xxx

# ----------------
# Used Functions (to make code more compact). See detail of each function in the repository
#-----------------
# *** make sure to customize this path
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R") 
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data_mt.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL_MT/generate_RL_policy.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL_MT/record_policy.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/writeCommandViaCSV.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/evaluate_macroeconomic_event.R")
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# terminal 3 path *** make sure to customize this path
path_T3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"

# path where to read control parameters from
path_control_files = "C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL_MT/control"

# evaluate data on macroeconomic event (required to start trading)
evaluate_macroeconomic_event(setup_path = "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_F2/TEST/Setup.csv",
                             file_name = "01_MacroeconomicEvent.csv",
                             path_t1 = path_T1,path_t3 = path_T3)
# -------------------------
# read data from trades in terminal 1
# -------------------------
# # uncomment code below to test functionality without MT4 platform installed
# DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv",
#                         demo_mode = T),
#             silent = TRUE)
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"), silent = TRUE)

# check if we have no errors importing trading results
if(!class(DFT1)[1]=="try-error"){

# Vector with unique Trading Systems
vector_systems <- DFT1 %$% MagicNumber %>% unique() %>% sort()

# For debugging: summarise number of trades to see desired number of trades was achieved
DFT1_sum <- DFT1 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n(),
            Mean_profit = sum(Profit)) %>% 
  arrange(desc(Num_Trades))

### ============== FOR EVERY TRADING SYSTEM ###
for (i in 1:length(vector_systems)) {
  # tryCatch() function will not abort the entire for loop in case of the error in one iteration
  tryCatch({
    # execute this code below for debugging:
    # i <- 25
    
  # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
  # try to extract market type information for that system, filter rows where MarketType was not logged!
  DFT1_MT <- try(import_data_mt(path_T1, trading_system), silent = TRUE) %>% filter(MarketType != -1)
  # go to the next i if there is no data
  if(class(DFT1_MT)[1]=="try-error") { next }
    # joining the data with market type info
    trading_systemDF <- inner_join(trading_systemDF, DFT1_MT, by = "TicketNumber") 
    # write this data for further debugging or tests
    # write_rds(trading_systemDF,path = "test_data/data_trades_markettype.rds")
    
    #==============================================================================
    # Define state and action sets for Reinforcement Learning
    states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
    actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
    
    # Define reinforcement learning parameters (see explanation below or in vignette)
    # -----
    # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
    # gamma - reward rate        0.1 <- short term | long term   -> 0.9
    # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
    # iter 
    # ----- 
    # to uncomment desired learning parameters:
    #control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.5)
    #control <- list(alpha = 0.9, gamma = 0.9, epsilon = 0.9)
    #control <- list(alpha = 0.8, gamma = 0.3, epsilon = 0.5)
    #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
    # check existence of the file with control parameters, go to next if not exists
    if(!file.exists(paste0(path_control_files,"/", trading_system, ".rds"))) { next }
    # Use optimal control parameters found by auxiliary function
    control <- read_rds(paste0(path_control_files,"/", trading_system, ".rds"))
    #control <- read_rds(paste0(path_control_files,"/", 8118102, ".rds"))
    # -----
    #==============================================================================
    
    
    # perform reinforcement learning and return policy
    policy_tr_systDF <- generate_RL_policy(trading_systemDF, states = states,actions = actions,
                                           control = control)
    
    # # summarize results by Market Type
    # trading_systemDF %>% group_by(MarketType) %>% summarise(ProfitMT = sum(Profit))
    
    # record policy to the sandbox of Terminal 3, this should be analysed by EA
    record_policy(x = policy_tr_systDF, trading_system = trading_system, path_sandbox = path_T3)
    
    

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###

} # ============== END of condition to check  if(!class(DFT1_MT)[1]=="try-error")



