library(lazytrade)
library(magrittr)
library(dplyr)
library(readr)
library(lubridate)
library(testthat)

context("data manipulation")

path_data <- normalizePath(tempdir(),winslash = "/")

file.copy(from = system.file("extdata", "Setup.csv", package = "lazytrade"),
          to = file.path(path_data, "Setup.csv"), overwrite = TRUE)

system_list <- read_csv(file.path(path_data, "Setup.csv"))

data(profit_factorDF)



test_that("data trnasformation", {

  x <- profit_factorDF
  num_trades_to_consider <- 3
  profit_factor_limit <- 1

  y <- x %>%  # filtered to contain last 20 orders for each system
    dplyr::ungroup() %>%
    dplyr::group_by(MagicNumber) %>%
    dplyr::arrange(MagicNumber, desc(OrderCloseTime)) %>%
    dplyr::filter(row_number() <= num_trades_to_consider+1) %>%
    lazytrade::get_profit_factorDF(num_trades_to_consider) %>%
    dplyr::ungroup() %>%
    dplyr::filter(PrFact < profit_factor_limit) %>%
    dplyr::select(MagicNumber, PrFact) %>%
    dplyr::mutate(ToOptimize = 1) %>%
    dplyr::inner_join(y = system_list, by = c("MagicNumber" = "Magic"))

   expect_equal(nrow(y),7)

})
