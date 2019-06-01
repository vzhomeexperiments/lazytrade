#' Function that decrypt encrypted content
#'
#' @param path_encrypted_content - path to the encrypted content of the API key
#' @param path_private_key - path to the private RSA key, should be without password
#'
#' @return - decrypted key
#' @export
#'
#' @examples
decrypt_mykeys <- function(path_encrypted_content, path_private_key) {
  require(openssl)
  require(tidyverse)
  # get back our encrypted API key
  out <- read_rds(path_encrypted_content)
  # path to our key
  # path_private_key <- file.path("C:/Users/fxtrams/.ssh", "id_api")
  api_key <- decrypt_envelope(out$data, out$iv, out$session, path_private_key, password = "") %>% 
    unserialize()
  return(api_key)
}