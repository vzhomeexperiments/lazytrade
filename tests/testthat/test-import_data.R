library(testthat)
library(readr)
library(lubridate)


context("import_data")


test_that("import works", {

  DFT1 <- import_data(trade_log_file = system.file("extdata", "OrdersResultsT1.csv", package = "lazytrade"), demo_mode = TRUE)

  expect_s3_class(DFT1, class = 'data.frame')

})
