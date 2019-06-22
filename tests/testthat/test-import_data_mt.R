library(tidyverse)
library(testthat)

context("import_data_mt")

test_that("import mt works", {

  DFT1 <- import_data_mt(trade_log_file = system.file("extdata", "MarketTypeLog8132101.csv", package = "lazytrade"), demo_mode = TRUE)

  expect_s3_class(DFT1, class = 'data.frame')

})
