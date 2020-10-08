library(readr)
library(testthat)
library(lubridate)
library(lazytrade)

context("mt_import_data")

test_that("import mt works", {
  path_sbxm <- normalizePath(tempdir(),winslash = "/")

  file.copy(from = system.file("extdata", "MarketTypeLog9139214.csv", package = "lazytrade"),
            to = file.path(path_sbxm, "MarketTypeLog9139214.csv"), overwrite = TRUE)

  DFT1 <- mt_import_data(path_sbxm = path_sbxm,system_number = 9139214)

  expect_s3_class(DFT1, class = 'data.frame')

})
