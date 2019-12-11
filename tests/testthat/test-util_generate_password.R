library(testthat)
library(magrittr)
library(stringr)
library(openssl)
library(readr)

context("password_generation")

salt <- "random text"

test_that("hash is created", {


  # generate 'draft' part
  draft <- paste(Sys.Date(), salt) %>%
    as.character.Date() %>%
    sha512(key = as.character(Sys.time()))

  expect_type(draft, 'character')

})



test_that("lenght works", {

  pass_len <- 8
  # generate 'draft' part
  draft <- paste(Sys.Date(), salt) %>%
    as.character.Date() %>%
    sha512(key = as.character(Sys.time()))

  # take first part
  p1 <- draft %>%
    substring(first = 1, last = round(pass_len/2))

  # take second part, convert to upper case
  p2 <- draft %>%
    substring(first = 11, last = 10+round(pass_len/2)) %>% toupper()

  # join both parts
  outpass <- paste0(p1,p2)

  #check that generated password is of the right lenght
  expect_equal(nchar(outpass), pass_len)
})
