#' Encrypt api keys
#'
#' @description Provide easy interface to encrypt the api key.
#' In order to use function simply provide a string with an API key.
#' In addition provide the path to the .ssh folder and names of the private and public keys
#'
#' @details Make sure to clean the history of the R session
#'
#' @references for more info on how to use RSA cryptography in R check my course
#' https://www.udemy.com/course/keep-your-secrets-under-control/?referralCode=5B78D58E7C06AFFD80AE
#'
#' @param api_key String with API key
#' @param path_ssh String with path to the file with rsa keys. Same place will be used to store encrypted data
#' @param enc_name String with a name of the file with encrypted key. Default name is 'api_key.enc.rds'
#' @param file_rsa String with a name of the file with a private key. Default name is 'id_api'
#' @param file_rsa_pub String with a name of the file with a public key. Default name is 'id_api.pub'
#'
#' @return Writes a file with encrypted key
#' @export
#'
#' @examples
#'
#' library(openssl)
#' library(magrittr)
#' library(readr)
#' path_ssh <- normalizePath(tempdir(),winslash = "/")
#' rsa_keygen() %>% write_pem(path = file.path(path_ssh, 'id_api'))
#' # extract and write your public key
#' read_key(file = file.path(path_ssh, 'id_api'), password = "") %>%
#' `[[`("pubkey") %>% write_pem(path = file.path(path_ssh, 'id_api.pub'))
#'
#'
#' path_private_key <- file.path(path_ssh, "id_api")
#' path_public_key <- file.path(path_ssh, "id_api.pub")
#'
#' #encrypting string 'my_key'...
#' encrypt_api_key(api_key = 'my_key', enc_name = 'api_key.enc.rds',path_ssh = path_ssh)
#'
#' out <- read_rds(file.path(path_ssh, "api_key.enc.rds"))
#'
#' # decrypting the password using public data list and private key
#' api_key <- decrypt_envelope(out$data,
#'                             out$iv,
#'                             out$session,
#'                             path_private_key, password = "") %>%
#'            unserialize()
#'
#' # outcome of the encryption will be a string 'my_key'
#'
#'
encrypt_api_key <- function(api_key, enc_name = 'api_key.enc.rds',
                            path_ssh = 'path_ssh', file_rsa = 'id_api',
                            file_rsa_pub = 'id_api.pub'){

  # check if the required packages are installed
  if (!requireNamespace(c("magrittr", "openssl", "readr"),
                        quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }

  requireNamespace("magrittr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("openssl", quietly = TRUE)

  # path private key
  private_key_path <- file.path(path_ssh, "id_api")

## Encrypt with your public key
  api_key %>%
  # serialize the object
  serialize(connection = NULL) %>%
  # encrypt the object
  encrypt_envelope(private_key_path) %>%
  # write encrypted data to File to your working directory
  write_rds(file.path(path_ssh, enc_name))

}
