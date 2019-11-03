library(testthat)
library(tidyverse)
library(lazytrade)


context("test_model")


test_that("test works", {

  data(x_test_model)
  data(result_R)

  dat31 <- x_test_model %>%
    # select only original value of the price change
    select(LABEL) %>%
    # add column with predicted price change
    bind_cols(result_R) %>%
    ## account for a label and predicted results changes by using cumulative sum
    # label column
    mutate(LABEL_CMSUM = cumsum(LABEL)) %>%
    # lag column 'predict' to 75 periods, column P_lag will match corresponding real price in the column 'LABEL'
    mutate(predict = lag(predict, 75)) %>%
    # omit na's
    na.omit() %>%
    # create a risk column, use 10 pips as a trigger
    mutate(Risk = if_else(predict > 10, 1, if_else(predict < -10, -1, 0))) %>%
    # predict column with cum sum value
    mutate(predict_CMSUM = cumsum(predict)) %>%
    # calculate expected outcome of risking the 'Risk': trade according to prediction
    mutate(ExpectedGain = predict_CMSUM*Risk) %>%
    # calculate 'real' gain or loss. LABEL is how the price moved (ground truth) so the column will be real outcome
    mutate(AchievedGain = LABEL_CMSUM*Risk) %>%
    # to account on spread
    mutate(Spread = if_else(AchievedGain > 0, - 5, if_else(AchievedGain < 0, -5, 0))) %>%
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
              TPSL_Level = 10) %>%
    # interpret the results
    mutate(FinalOutcome = if_else(AchievedPnL > 0, "VeryGood", "VeryBad"),
           FinalQuality = AchievedPnL/(0.0001+ExpectedPnL))

  expect_equal(ncol(dat31), 6)

})
