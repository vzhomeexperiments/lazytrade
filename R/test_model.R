#' Test model function. Goal of the function is to verify how good predicted results are.
#' 
#' This function should work to backtest any possible dataset lenght. It could be that we will need to use it for testing
#' 1 week or 1 month. It should also work for both Regression and Classification models. Note: strategy outcomes assumes trading on
#' all 28 major forex pairs
#' 
#' 
#' @param test_dataset      Dataset containing the column 'LABEL' which will correspond to the real outcome of Asset price change. This 
#' column will be used to verify the trading strategy
#' @param predictor_dataset  Dataset containing the column 'predict'. This column is corresponding to the predicted outcome of Asset 
#'       change. This column will be used to verify strategy outcomes
#' @param test_type can be either "regression" or "classification" used to distinguish which type of modelling is being used
#'
#' @return Function will return a data frame with several quality score metrics for the best model.
#'         In case quality score is positive or more than 1 the model would likely be working good.
#'         In case the score will be negative then the model is not predicting good.
#'         Internal logic will test several predictor thresholds and will indicate the best one
#'         
#' @export
#'
#' @examples
#' 
test_model <- function(test_dataset, predictor_dataset, test_type){
  require(tidyverse)
  # arguments for debugging for regression
  # test_dataset <- read_rds("test_data/dat21.rds")   
  # test_dataset <- dat21
  # test_dataset <- dat20
  # predictor_dataset <- read_rds("test_data/result_prev.rds")
  # predictor_dataset <- result
  # predictor_dataset <- result_10
  # test_type <- "regression"
  
  # arguments for debugging for classification
  # test_dataset <- read_rds("test_data/test_dataset_c.rds")
  # predictor_dataset <- read_rds("test_data/pred_dataset_c.rds")
  # test_type <- "classification"
  
  if(test_type == "regression"){
## evaluate hypothetical results of trading using the model
    
    # do this test for several trading trigger levels
    tp_sl_levels <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 
                      160, 170, 180, 190, 200)
    for (TPSL in tp_sl_levels) {
      # TPSL <- 5
      # do the testing
      if(!exists("dat31")){
        # join real values with predicted values
        dat31 <- test_dataset %>% select(LABEL) %>% bind_cols(predictor_dataset) %>% 
          # add column risk that has +1 if buy trade and -1 if sell trade, 0 (no risk) if prediction is exact zero
          # mutate(Risk = if_else(predict > 0, 1, if_else(predict < 0, -1, 0))) %>% 
          mutate(Risk = if_else(predict > TPSL, 1, if_else(predict < -TPSL, -1, 0))) %>% 
          # calculate expected outcome of risking the 'Risk': trade according to prediction
          mutate(ExpectedGain = predict*Risk) %>% 
          # calculate 'real' gain or loss. LABEL is how the price moved (ground truth) so the column will be real outcome
          mutate(AchievedGain = LABEL*Risk) %>% 
          # to account on spread
          mutate(Spread = if_else(AchievedGain > 0, - 5, if_else(AchievedGain < 0, + 5, 0))) %>% 
          # calculate 'net' gain
          mutate(NetGain = AchievedGain + Spread) %>% 
          # remove zero values to calculate presumed number of trades
          filter(AchievedGain != 0) %>% 
          # get the sum of both columns
          # Column Expected PNL would be the result in case all trades would be successful
          # Column Achieved PNL is the results achieved in reality
          summarise(ExpectedPnL = sum(ExpectedGain),
                    AchievedPnL = sum(NetGain),
                    TotalTrades = n(),
                    TPSL_Level = TPSL) %>% 
          # interpret the results
          mutate(FinalOutcome = if_else(AchievedPnL > 0, "VeryGood", "VeryBad"),
                 FinalQuality = AchievedPnL/(0.0001+ExpectedPnL)) 
      } else {
        # 
        dat311 <- test_dataset %>% select(LABEL) %>% bind_cols(predictor_dataset) %>% 
          # add column risk that has +1 if buy trade and -1 if sell trade, 0 (no risk) if prediction is exact zero
          # mutate(Risk = if_else(predict > 0, 1, if_else(predict < 0, -1, 0))) %>% 
          mutate(Risk = if_else(predict > TPSL, 1, if_else(predict < -TPSL, -1, 0))) %>% 
          # calculate expected outcome of risking the 'Risk': trade according to prediction
          mutate(ExpectedGain = predict*Risk) %>% 
          # calculate 'real' gain or loss. LABEL is how the price moved (ground truth) so the column will be real outcome
          mutate(AchievedGain = LABEL*Risk) %>% 
          # to account on spread
          mutate(Spread = if_else(AchievedGain > 0, - 5, if_else(AchievedGain < 0, + 5, 0))) %>% 
          # calculate 'net' gain
          mutate(NetGain = AchievedGain + Spread) %>% 
          # remove zero values to calculate presumed number of trades
          filter(AchievedGain != 0) %>% 
          # get the sum of both columns
          # Column Expected PNL would be the result in case all trades would be successful
          # Column Achieved PNL is the results achieved in reality
          summarise(ExpectedPnL = sum(ExpectedGain),
                    AchievedPnL = sum(NetGain),
                    TotalTrades = n(),
                    TPSL_Level = TPSL) %>% 
          # interpret the results
          mutate(FinalOutcome = if_else(AchievedPnL > 0, "VeryGood", "VeryBad"),
                 FinalQuality = AchievedPnL/(0.0001+ExpectedPnL)) 
        
        # join final results
        dat31 <- bind_rows(dat31, dat311)
      }
        
        
      
    }
    
    ### === interfpretation of the obtained results ===
    # step 1: keep significant number of trades
    max_trades <- 0.8 * max(dat31$TotalTrades) %>% round()
    min_trades <- 0.2 * max(dat31$TotalTrades) %>% round()
    # step 2: filter out only those results
    dat51 <- dat31 %>% filter(TotalTrades < max_trades, TotalTrades > min_trades) %>% 
      # step 3: keep only rows with the maximum quality
      slice(which.max(FinalQuality))
    
    # return the result of the function
    return(dat51)

}

  if(test_type == "classification"){

    dat31 <-  predictor_dataset %>% bind_cols(test_dataset) %>% 
      # generate column of estimated risk trusting the model
      mutate(RiskEstim = if_else(predict == "BU", 1, -1)) %>%
      # generate colmn of 'known' direction
      mutate(RiskKnown = if_else(LABEL > 0, 1, if_else(LABEL < 0, -1, 0))) %>% 
      # calculate expected outcome of risking the 'RiskEst'
      mutate(AchievedGain = RiskEstim*LABEL) %>% 
      # calculate 'real' gain or loss
      mutate(ExpectedGain = RiskKnown*LABEL) %>% 
      # get the sum of both columns
      summarise(ExpectedPnL = sum(ExpectedGain),
                AchievedPnL = sum(AchievedGain)) %>% 
      # interpret the results
      mutate(FinalOutcome = if_else(AchievedPnL > 0, "VeryGood", "VeryBad"),
             FinalQuality = AchievedPnL/(0.0001+ExpectedPnL))
    
  }
  
return(dat31)



}