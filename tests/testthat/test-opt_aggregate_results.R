library(testthat)
library(dplyr)
library(magrittr)
library(lazytrade)

context("data transformation")

test_that("join to vector works", {

  DFOLDER <- system.file("extdata/RES", package = "lazytrade")

  # join paths into a vector
  DFOLDER <- dir(DFOLDER, full.names = TRUE)

  expect_length(DFOLDER, 2)

})

test_that("data aggregation works", {

  DFOLDER <- system.file("extdata/RES", package = "lazytrade")

  # join paths into a vector
  DFOLDER <- dir(DFOLDER, full.names = TRUE)

  for (FOLDER in DFOLDER) {
    # FOLDER <- DFOLDER[2]

    filesToRead <-list.files(FOLDER, pattern="*.csv", full.names=F)
    #error management (if empty folder)
    if(length(filesToRead) == 0) { next()}

    for (FILE in filesToRead) {
      #FILE <- filesToRead[1]
      # import data
      DF_TEST <- import_data(path_terminal = FOLDER, trade_log_file = FILE)
      DF_TEST$Symbol <- as.character(DF_TEST$Symbol)
      DF_TEST$OrderType <- as.character(DF_TEST$OrderType)
      #agregate
      if (!exists("DF_TEMP")) {
        DF_TEMP <- DF_TEST
      } else {
        DF_TEMP <- DF_TEMP %>% bind_rows(DF_TEST)
      }

    }

    #agregate
    if (!exists("DF_TEMP1")) {
      DF_TEMP1 <- DF_TEMP
    } else {
      DF_TEMP1 <- DF_TEMP1 %>% bind_rows(DF_TEMP)
    }

    # remove agregated results from the first folder
    rm(DF_TEMP)

  }

  #check that result is a dataframe
  expect_is(DF_TEMP1, class = "data.frame")


})
