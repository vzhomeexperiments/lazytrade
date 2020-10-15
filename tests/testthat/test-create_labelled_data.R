library(testthat)
library(dplyr)
library(magrittr)
library(readr)
library(lazytrade)




context("data manipulation")


# usind a sample data
data(price_dataset)


test_that("split to list works", {

  x <- price_dataset
  n <- 75
  nr <- nrow(x)
  namesdfr12 <- paste0("X", 1:n) #generated names for dataset useful later in the code
  dat11 <- x %>%
    # remove column 1 with data and time information
    dplyr::select(-1) %>%
    # split dataset into several objects each containing n rows (it will be a list)
    base::split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  # remove last element of the list
  dat11[length(dat11)] <- NULL


  expect_equal(length(dat11), 13)
})
