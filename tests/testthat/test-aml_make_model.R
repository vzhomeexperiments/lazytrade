library(testthat)
library(magrittr)

context("model_data")


test_that("random structures works", {

  ### random network structure
  nn_sets <- sample.int(n = 200, 24) %>% matrix(ncol = 4)

  expect_lt(max(nn_sets), 201)

})
