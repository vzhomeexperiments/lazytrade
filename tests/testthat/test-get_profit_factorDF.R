library(testthat)
library(lazytrade)
library(magrittr)
library(dplyr)

context("profit_factor")

data(profit_factorDF)

test_that("filter works", {

  x <- profit_factorDF
  num_orders <- 11
  DF_L <- x %>%
    ungroup %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n())


  expect_equal(DF_L[1,2]$nOrders, 11)
})
