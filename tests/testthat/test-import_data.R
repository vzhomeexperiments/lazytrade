library(testthat)
library(readr)
library(lubridate)
library(lazytrade)


context("import_data")


test_that("import works", {

  path_sbxm <- normalizePath(tempdir(),winslash = "/")

  file.copy(from = system.file("extdata", "OrdersResultsT1.csv", package = "lazytrade"),
            to = file.path(path_sbxm, "OrdersResultsT1.csv"), overwrite = TRUE)


  DFT1 <- import_data(path_sbxm = path_sbxm,
                      trade_log_file = "OrdersResultsT1.csv")

  expect_s3_class(DFT1, class = 'data.frame')

})
