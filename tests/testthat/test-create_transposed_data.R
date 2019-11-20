library(testthat)
library(dplyr)
library(magrittr)
library(lazytrade)

context("data transformation")

data(indicator_dataset)

test_that("object is list", {

  n <- 100
  x <- indicator_dataset

  nr <- nrow(x)
  namesdfr12 <- paste0("X", 1:n) #generated names for dataset useful later in the code

  dat11 <- x %>% select(-1) %>% split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  dat11[length(dat11)] <- NULL

  expect_type(dat11, "list")

})

test_that("column names", {

  n <- 100
  x <- indicator_dataset

  nr <- nrow(x)
  namesdfr12 <- paste0("X", 1:n) #generated names for dataset useful later in the code

  dat11 <- x %>% select(-1) %>% split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  dat11[length(dat11)] <- NULL

  # operations within the list
  for (i in 1:length(dat11)) {
    #i <- 1

    if(!exists("dfr12")){
      dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "minimal", verbose =F)
      names(dfr12) <- namesdfr12
    } else {
      dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "minimal", verbose =F)
      names(dfr12) <- namesdfr12
      dfr12 <- dfr12 %>% bind_rows(dfr12)
    }

  }

  #check that new object has the same names
  expect_equal(names(dfr12), namesdfr12)

})
