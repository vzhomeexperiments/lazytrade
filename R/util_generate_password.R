#' R function to generate random passwords for MT4 platform or other needs
#'
#' @description Utility function to generate random passwords.
#' Wrapper of cryptographic functions from 'openssl' library in R.
#' Password length can be customized.
#' By default function just output randomly generated 8 symbol password suitable for MT4 logins.
#' It is also possible to create other passwords and include special symbols.
#' When required, it's possible to write resulting password to the txt file.
#' Once generated, password is written to the destination supplied by the user.
#'
#' @details Passwords are generated using sha512 cryptographic function from openssl package.
#' System date and user 'salt' is used to supply initial text for cryptographic function.
#' Hashing function is using additional 'salt' which will be based on the current System time.
#' Additionally, only a part of generated string is selected and used for password.
#' Some letters of generated string are converted from lower to upper case.
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#'
#' @param salt string, random text supplied by the user
#' @param pass_len integer, number specifying how long should the password be
#' @param file_name string, indicate path of the file where to write text result
#' @param write_file bool, if true writes result to the txt file
#' @param special_symbols bool, if true adds special symbols
#'
#' @return string or text file with password
#' @export
#'
#' @examples
#'
#' library(stringr)
#' library(magrittr)
#' library(openssl)
#' library(readr)
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#' file_path <- file.path(dir, 'p.txt')
#'
#' #write to file
#' util_generate_password(salt = 'random text', file_name = file_path)
#'
#' #generate 8digit
#' util_generate_password(salt = 'random text')
#'
#' #generate password with special symbols
#' util_generate_password(salt = 'random text', special_symbols = TRUE)
#'
#' #generate longer password with special symbols
#' util_generate_password(salt = 'random text', pass_len = 10, special_symbols = TRUE)
#'
util_generate_password <- function(salt = "something random",
                                   pass_len = 8,
                                   write_file = FALSE,
                                   file_name = "",
                                   special_symbols = FALSE) {

  #special symbols
  specials <- c('@', '-', '!', '&', '?')

  s1 <- sample(specials, 1)
  s2 <- sample(specials, 1)
  #vector for 10 attempts
  tries <- 1:100

  for (VAR in tries) {
    #VAR <- tries[1]

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

    # add s1 if selected
    if (special_symbols == TRUE) {
      p1 <- paste0(p1, s1)
      p2 <- paste0(p2, s2)
    }

    # join both parts
    outpass <- paste0(p1,p2)

    # check if both numbers, lower and upper case letters are present
    num_yes <- str_detect(outpass, pattern = '[0-9]')
    low_yes <- str_detect(outpass, pattern = '[:lower:]')
    upp_yes <- str_detect(outpass, pattern = '[:upper:]')

    if(num_yes == TRUE && low_yes == TRUE && upp_yes == TRUE) {
      # write generated and checked password to the file
    ready_pass <- outpass  %>%
      as.data.frame.character()

    if (write_file == TRUE) {
      write_tsv(ready_pass, file_name)
    } else { return(ready_pass)}

      break() }

  }



}
