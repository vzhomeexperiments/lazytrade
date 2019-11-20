library(testthat)
library(readr)

context("test_import")

test_that("data error is detected", {

  path_terminal <- system.file("extdata", package = "lazytrade")
  trade_log_file = "AI_CP"
  time_period = 60
  data_deepth = "300"

  DFT1 <- try(read_csv(file = file.path(path_terminal, paste0(trade_log_file, time_period, "-", data_deepth, ".csv")),
                       col_names = F),
              silent = TRUE)

  #introduce 'error'
  DFT1$X3 <- 0

  #detect if some columns are filled with zeroes...
  Z_detect <- lapply(DFT1, function(x) all(x == 0)) %>% as.data.frame()

  expect_true(any(Z_detect == TRUE))

})
