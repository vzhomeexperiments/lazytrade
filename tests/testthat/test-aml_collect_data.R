library(testthat)
library(tidyverse)
library(lazytrade)
library(lubridate)

context("collect_data")

test_that("column selection works", {

  symbol <- "GBPUSD"
  # Vector of currency pairs
  Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
            "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
            "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
            "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")
  # find the index of this pair in the vector of columns
  cn <- which(symbol %in% Pairs) + 1

  expect_equal(cn, 2)


})

test_that("collect data works", {

  path_terminal <- system.file("extdata", package = "lazytrade")
  macd <- load_asset_data(path_terminal = path_terminal, trade_log_file = "AI_Macd",
                          time_period = 15, data_deepth = "300")

  prices <- load_asset_data(path_terminal = path_terminal, trade_log_file = "AI_CP",
                            time_period = 15, data_deepth = "300")

  path_data <- normalizePath(tempdir(),winslash = "/")



  aml_collect_data(price_dataset = prices,
                    indicator_dataset = macd,
                    symbol = 'EURUSD',
                    num_bars = 75,
                    timeframe = 15,
                    path_data = path_data)

  f_name <- "EURUSDM15X75.rds"
  full_path <- file.path(path_data,  f_name)
  EURUSDM15X75 <- read_rds(full_path)

  expect_equal(ncol(EURUSDM15X75), 76)


})

test_that("data trimming works", {

  path_data <- normalizePath(tempdir(),winslash = "/")
  f_name <- "EURUSDM15X75.rds"
  full_path <- file.path(path_data,  f_name)

  sample(1000) %>% matrix(10,byrow = T) %>% as_tibble() %>%
    write_rds(full_path)

  # check number of rows
  x1_nrows <- read_rds(full_path) %>% nrow()
  # what to do if too much rows?
  if(x1_nrows > 9){
    # read all the data
    read_rds(full_path) %>%
      # use only last 5 rows for test
      tail(5) %>%
      # write them back
      write_rds(full_path)
  }

  EURUSDM15X75 <- read_rds(full_path)
  # ---

  expect_equal(nrow(EURUSDM15X75), 5)


})
