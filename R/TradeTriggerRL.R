# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/your-trading-control-reinforcement-learning/?couponCode=LAZYTRADE4-10
# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# DETAILS: Trades are analysed and RL model is created for each single Expert Advisor
#        : Q states function is calculated, whenever Action 'ON' is > than 'OFF' trade trigger will be active   
#        : Results are written to the file of the MT4 Trading Terminal
#        : Using dummy dataframe for the initial learning of the RL model

# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 
library(ReinforcementLearning) #install.packages("ReinforcementLearning")
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
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL/generate_RL_policy.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL/record_policy.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/writeCommandViaCSV.R")

 
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# terminal 3 path *** make sure to customize this path
path_T3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"

# path where to read control parameters from
path_control_files = "C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/_RL/control"

# -------------------------
# read data from trades in terminal 1
# -------------------------
# uncomment code below to test functionality without MT4 platform installed
# DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv",
#                         demo_mode = T),
#             silent = TRUE)
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"), silent = TRUE)

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
    # i <- 17 #policy off
    # i <- 2 #policy on
    
    # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
  
  ## -- Go to the next Loop iteration if there is too little trades! -- ##
  if(nrow(trading_systemDF) < 5) { next }
  
  #==============================================================================
  # Define state and action sets for Reinforcement Learning
  states <- c("tradewin", "tradeloss")
  actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  
  # Define reinforcement learning parameters (see explanation below or in vignette)
  # -----
  # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
  # gamma - reward rate        0.1 <- short term | long term   -> 0.9
  # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
  # iter 
  # ----- 
  #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
  # check existence of the file with control parameters, go to next if not exists
  if(!file.exists(paste0(path_control_files,"/", trading_system, ".rds"))) { next }
  # Use optimal control parameters found by auxiliary function
  control <- read_rds(paste0(path_control_files,"/", trading_system, ".rds"))
  #control <- read_rds(paste0(path_control_files,"/", 8118102, ".rds"))
  
  # perform reinforcement learning and return policy
  policy_tr_systDF <- generate_RL_policy(trading_systemDF, states = states,actions = actions,
                                         control = control)
  # get the latest trade of that system (will be used to match with policy of RL)
  latest_trade <- DFT1 %>% 
    filter(MagicNumber == trading_system) %>% 
    arrange(desc(OrderCloseTime)) %>% 
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           Reward =  Profit,
           State = NextState) %>% head(1) %$% NextState
  
  # record policy to the sandbox of Terminal 3, this should be analysed by EA
  record_policy(x = policy_tr_systDF, last_result = latest_trade, trading_system = trading_system, path_sandbox = path_T3)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###



##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------
if(file.exists(file.path(path_T1, "01_MacroeconomicEvent.csv"))){
  DF_NT <- read_csv(file= file.path(path_T1, "01_MacroeconomicEvent.csv"), col_types = "i")
  if(DF_NT[1,1] == 1) {
    # disable trades
    if(!class(DFT1)[1]=='try-error'){
      DFT1 %>%
        group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>% 
        # write commands to disable systems
        writeCommandViaCSV(path_T1)}
    if(!class(DFT3)[1]=='try-error'){
      DFT3 %>%
        group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>% 
        writeCommandViaCSV(path_T3)}
    
    
  }
  # enable systems of T1 in case they were disabled previously
  if(DF_NT[1,1] == 0) {
    # enable trades
    if(!class(DFT1)[1]=='try-error'){
      # temporary solution: enable trades from the working project
      read_csv("C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_A/TEST/Setup.csv") %>%
        group_by(Magic) %>% select(Magic) %>% mutate(IsEnabled = 1) %>% 
        # write commands to disable systems
        writeCommandViaCSV(path_T1)}

    
  }
  
}


