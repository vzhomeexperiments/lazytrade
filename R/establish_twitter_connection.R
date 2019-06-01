#' Function to establish twitter connection. Must be personalized!!!
#'
#' @param path_encrypted_keys - path where the API keys are encrypted in the private form
#' @param path_private_key - path to this computer private key, which was used to encrypt twitter api keys
#'
#' @return
#' @export
#'
#' @examples
establish_twitter_connection <- function(path_encrypted_keys, path_private_key){
  source("C:/Users/fxtrams/Documents/000_TradingRepo/R_NewsReading/TWITTER_FINSEN/Functions/decrypt_mykeys.R")
  # path to encrypted keys - replace with those of your computer
  # path_encrypted_keys <- "C:/Users/fxtrams/Documents/003_Udemy/good_news/Keys"
  
  # private and public key path - replace paths with those of your computer
  #path_private_key <- file.path("C:/Users/fxtrams/.ssh", "id_api")
  
  # Consumer API keys
  ConsumerAPIkeys <- decrypt_mykeys(path_encrypted_content = file.path(path_encrypted_keys, "ConsumerAPIkeys.enc.rds"),
                                    path_private_key = path_private_key)
  # 
  # API secret key)
  APIsecretkey <- decrypt_mykeys(path_encrypted_content = file.path(path_encrypted_keys, "APIsecretkey.enc.rds"),
                                 path_private_key = path_private_key)
  
  # Access token & access token secret
  Accesstoken <- decrypt_mykeys(path_encrypted_content = file.path(path_encrypted_keys, "Accesstoken.enc.rds"),
                                path_private_key = path_private_key) 
  # 
  # Access token secret)
  Accesstokensecret <- decrypt_mykeys(path_encrypted_content = file.path(path_encrypted_keys, "Accesstokensecret.enc.rds"),
                                      path_private_key = path_private_key)
  
  # creating twitter connection using function from twitterR
  setup_twitter_oauth(consumer_key = ConsumerAPIkeys,
                      consumer_secret = APIsecretkey,
                      access_token = Accesstoken,
                      access_secret = Accesstokensecret)
  
  
}
