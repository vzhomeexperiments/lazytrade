library(testthat)
library(dplyr)
library(magrittr)
library(lazytrade)


context("test_model")


test_that("strategy test works", {

  data(y)
  data(result_R1)

  NB <- 20
  TR <- 20
  symbol <- 'USDJPY'

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
    dplyr::mutate(Risk = if_else(predict > TR, 1, if_else(predict < -TR, -1, 0))) %>%
    ## create a columns with shifted X2 price down:
    # value BR will indicate number of bars we will hold this position
    dplyr::mutate(X2_NB = lag(X2, NB)) %>%
    # clean up this dataset
    na.omit() %>%
    # now calculate the scenario
    dplyr::mutate(Hold_NB = Risk*(X2_NB - X2)) %>%
    # remove zero values to calculate presumed number of trades
    dplyr::filter(Risk != 0) %>%
    # get the sum of columns
    # Column Expected PNL would be the result in case all trades would be successful
    # Column Achieved PNL is the results achieved in reality
    dplyr::summarise(PnL_NB = sum(Hold_NB),
                     TotalTrades = n(),
                     TR_Level = TR,
                     NB_hold = NB,
                     Symbol = symbol) %>%

    # interpret the results
    dplyr::mutate(FinalOutcome = if_else(PnL_NB > 0, "Good", "Bad"))
  expect_equal(ncol(dat31), 6)

})
