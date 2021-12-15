library(lazytrade)
library(testthat)
library(magrittr)
library(dplyr)

context("util_profit_factor")

test_that("test value of the calculation", {

  data(profit_factor_data)

  DF_Stats <- profit_factor_data %>%
    group_by(X1) %>%
    summarise(PnL = sum(X5),
              NumTrades = n(),
              PrFact = util_profit_factor(X5)) %>%
    select(PrFact) %>%
    head(1) %>%
    round(3)

  expect_equal(DF_Stats$PrFact, 0.68)

})
