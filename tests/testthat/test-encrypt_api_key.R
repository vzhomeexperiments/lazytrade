library(testthat)
library(openssl)
library(magrittr)
library(readr)

context("encryption")


path_ssh <- normalizePath(tempdir(),winslash = "/")
rsa_keygen() %>% write_pem(path = file.path(path_ssh, 'id_api'))
# extract and write your public key
read_key(file = file.path(path_ssh, 'id_api'), password = "") %>%
  `[[`("pubkey") %>% write_pem(path = file.path(path_ssh, 'id_api.pub'))

path_private_key <- file.path(path_ssh, "id_api")
path_public_key <- file.path(path_ssh, "id_api.pub")

test_that("encryption cycle works", {

  # path private key
  private_key_path <- file.path(path_ssh, "id_api")

  ## Encrypt with your public key
  "api_key" %>%
    # serialize the object
    serialize(connection = NULL) %>%
    # encrypt the object
    encrypt_envelope(private_key_path) %>%
    # write encrypted data to File to your working directory
    write_rds(file.path(path_ssh, 'api_key.enc.rds'))

  ## Decrypt
  out <- read_rds(file.path(path_ssh, "api_key.enc.rds"))

  api_key <- decrypt_envelope(out$data,
                              out$iv,
                              out$session,
                              path_private_key, password = "") %>%
    unserialize()

  # checking if decrypted content is identical to encrypted one
  expect_identical(api_key, 'api_key')


})
