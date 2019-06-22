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
#' \dontrun{
#'
#' # Consumer API keys
#' ConsumerAPIkeys <- decrypt_mykeys(path_encrypted_content = file.path(path_encrypted_keys,
#'                                   "ConsumerAPIkeys.enc.rds"),
#'                                   path_private_key = path_private_key)
#'
#' }
#'
#'
decrypt_mykeys <- function(path_encrypted_content, path_private_key) {

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("openssl", quietly = TRUE)
  # get back our encrypted API key
  out <- read_rds(path_encrypted_content)
  # path to our key
  # path_private_key <- file.path("C:/Users/fxtrams/.ssh", "id_api")
  api_key <- decrypt_envelope(out$data, out$iv, out$session, path_private_key, password = "") %>%
    unserialize()
  return(api_key)
}
