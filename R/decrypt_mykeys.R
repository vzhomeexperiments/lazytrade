#' Function that decrypt encrypted content
#'
#' @param path_encrypted_content - path to the encrypted content of the API key
#' @param path_private_key - path to the private RSA key, should be without password
#'
#' @details It is possible to generate private/public key pair using R-Studio Project Options Menu.
#' Alternatively possible to use 'openssl' R package
#'
#' @return - a string with decrypted key
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(magrittr)
#' library(openssl)
#' library(readr)
#'
#' path_ssh <- normalizePath(tempdir(),winslash = "/")
#' rsa_keygen() %>% write_pem(path = file.path(path_ssh, 'id_api'))
#' # extract and write your public key
#' read_key(file = file.path(path_ssh, 'id_api'), password = "") %>%
#' `[[`("pubkey") %>% write_pem(path = file.path(path_ssh, 'id_api.pub'))
#'
#' path_private_key <- file.path(path_ssh, "id_api")
#' path_public_key <- file.path(path_ssh, "id_api.pub")
#'
#' #encrypting string 'my_key'...
#' encrypt_api_key(api_key = 'my_key', enc_name = 'api_key.enc.rds',path_ssh = path_ssh)
#'
#' #encrypted content
#' out <- read_rds(file.path(path_ssh, "api_key.enc.rds"))
#'
#' # Consumer API keys
#' ConsumerAPIkeys <- decrypt_mykeys(path_encrypted_content = file.path(path_ssh,
#'                                   'api_key.enc.rds'),
#'                                   path_private_key = path_private_key)
#'
#'
decrypt_mykeys <- function(path_encrypted_content, path_private_key) {

  requireNamespace("readr", quietly = TRUE)
  requireNamespace("openssl", quietly = TRUE)

  # get back our encrypted API key
  out <- readr::read_rds(path_encrypted_content)
  # path to our key
  api_key <- openssl::decrypt_envelope(out$data, out$iv, out$session, path_private_key, password = "") %>%
    unserialize()
  return(api_key)
}
