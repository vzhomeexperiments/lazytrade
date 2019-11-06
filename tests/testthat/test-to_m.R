library(testthat)
library(tidyverse)

context("convert_to_matrix")

test_that("round to whole number works", {

  n_cols <- 10
  x <- seq(1:99) %>% as.data.frame()
  # get intermediate object and dimension
  Step1 <- x
  # find number of rows of data frame
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  # WN <- 19.2
  # WN <- 19.8
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}

  #expect that this example works and we have 9 complete rows
  expect_equal(WN, 9)

})

test_that("the output is matrix", {

  n_cols <- 10
  Step1 <- seq(1:99) %>% as.data.frame()
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}
  # find number of rows to extract data
  n <- n_cols * WN
  # extract relevant matrix
  Step2 <- Step1 %>%
    head(n) %>% #only use whole number to avoid errors
    t() %>%  # this brings us a matrix
    matrix(nrow = WN, ncol = n_cols, byrow = TRUE) # transforming that into matrix size 20x150

  expect_is(Step2, "matrix")


})
