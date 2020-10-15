library(testthat)
library(lazytrade)
library(magrittr)
library(dplyr)

context("profit_factor")

data(profit_factorDF)

test_that("summarise works", {

  x <- profit_factorDF
  x <- x %>% ungroup()

  DF_L <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n(), .groups = "drop_last")


  expect_equal(DF_L[1,2]$nOrders, 11)
})

test_that("filter works", {

  x <- profit_factorDF
  x <- x %>% ungroup()
  num_orders <- 10

  DF_L <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n(), .groups = "drop_last") %>%
    filter(nOrders > num_orders) %>%
    select(MagicNumber) %>%
    as.data.frame() %>%
    # subset only rows that contans magic numbers from x
    inner_join(x, by = "MagicNumber") %>%
    group_by(MagicNumber) %>%
    filter(Profit < 0) %>%
    summarise(Loss = abs(sum(Profit)), .groups = "drop_last")

  expect_gt(sum(DF_L$Loss), expected = 45000)

})
