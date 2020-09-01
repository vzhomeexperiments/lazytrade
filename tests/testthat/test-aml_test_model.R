library(testthat)
library(dplyr)
library(magrittr)
library(lazytrade)


context("test_model")


test_that("strategy test works", {

  data(y)
  data(result_R1)

  dat31 <- y %>%
    # using last 600 observations
    head(600) %>%
    ## select columns:
    # X1 time index
    # X2 price at the time index
    # X3 price 34 bars ago
    # LABEL is a price difference X3-X2
    dplyr::select(X1, X2) %>%
    # add column with predicted price change
    dplyr::bind_cols(result_R1) %>%
    ## create columns:
    # dP_34 - price difference now vs 34 bars (only for check)
    # dplyr::mutate(dP_34 = X3-X2) %>%
    ## setup condition to enter the trade
    # create a risk column, use 20 pips as a trigger
    dplyr::mutate(Risk = if_else(predict > 20, 1, if_else(predict < -20, -1, 0))) %>%
    ## create several columns with shifted X2 price down:
    # X2_3, X2_5, X2_10 where 3, 5, 10 indicates number of bars we will hold this position
    dplyr::mutate(X2_3 = lag(X2, 3),
                  X2_5 = lag(X2, 5),
                  X2_10 = lag(X2, 10),
                  X2_34 = lag(X2, 34)) %>%
    # clean up this dataset
    na.omit() %>%
    # now calculate several scenarios:
    dplyr::mutate(Hold_3 = Risk*(X2_3 - X2),
                  Hold_5 = Risk*(X2_5 - X2),
                  Hold_10 = Risk*(X2_10 - X2),
                  Hold_34 = Risk*(X2_34 - X2)) %>%


    # remove zero values to calculate presumed number of trades
    dplyr::filter(Risk != 0) %>%
    # get the sum of columns
    # Column Expected PNL would be the result in case all trades would be successful
    # Column Achieved PNL is the results achieved in reality
    dplyr::summarise(PnL_3 = sum(Hold_3),
                     PnL_5 = sum(Hold_5),
                     PnL_10 = sum(Hold_10),
                     PnL_34 = sum(Hold_34),
                     TotalTrades = n(),
                     TPSL_Level = 20)

  expect_equal(ncol(dat31), 1)

})
