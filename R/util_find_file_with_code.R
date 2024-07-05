#' R function to find file with specific code within it's content
#'
#' @description Utility function to find the file path which contains specific code
#' 
#' `r lifecycle::badge('experimental')`
#'
#' @details Function is reading content of file and checks each file for a specific code
#' Function will require a vector with a valid file paths
#' Function will output the path to that file which contains a give code
#' If no desired code is found, function will return NULL
#' Optionally, if enabled, function will replace their values
#' Note: supply same kind of parameters (either boolean or integers)
#' 
#'
#' @author (C) 2024 Vladimir Zhbanko
#'
#' @param files - string, vector with file paths
#' @param code_to_find - string or integer with a pattern value to search within the file
#' @param option_replace boolean, option to indicate if function should also replace parameter values
#' @param v_settings string vector with the parameters that needs to be found
#' @param v_values vector with the corresponding values of the parameters
#'
#' @return string with a file path containing a code
#' @export
#'
#' @examples
#'
#' library(readr)
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#' file.copy(from = system.file("extdata/default", package = "lazytrade"),
#'           to = dir, recursive = TRUE)
#'
#' files_chr <- list.files(path = file.path(dir, "default"), all.files = TRUE,
#' pattern = ".chr", full.names = TRUE)
#' code_to_find <- 9142301
#' 
#' # find the path to the file with numeric code
#' file_i_need <- util_find_file_with_code(files = files_chr, 
#'                                    code_to_find = 9142301)
#' 
#' # find the path to the file with a character code
#' file_i_need <- util_find_file_with_code(files = files_chr,
#'                                    code_to_find = 'BITCOIN')
#'                                    
#' # find the path to the file with a character code that is not exists in those files
#' file_i_need <- util_find_file_with_code(files = files_chr,
#'                                    code_to_find = 'Banana')
#'                                    
#' # define a vector with settings to search
#' v_par <- c("StartHour")
#' v_val <- c(15)
#'                                     
#' # Replace integer values
#' file_i_need <- util_find_file_with_code(files = files_chr,
#'                                    code_to_find = 9142301,
#'                                    option_replace = TRUE,
#'                                    v_settings = v_par,
#'                                    v_values = v_val)
#'                                    
#' # define a vector with settings to search
#' v_par <- "UseMAFilter"
#' v_val <- FALSE
#'                                      
#' # Replace boolean values
#' file_i_need <- util_find_file_with_code(files = files_chr,
#'                                    code_to_find = 9142301,
#'                                    option_replace = TRUE,
#'                                    v_settings = v_par,
#'                                    v_values = v_val)
#'                                      
#' # Replace boolean values in specified file
#' file.copy(from = system.file("extdata/Falcon_D.set", package = "lazytrade"),
#'           to = dir, recursive = TRUE)
#' file_i_need <- util_find_file_with_code(files = file.path(dir, "Falcon_D.set"),
#'                                    code_to_find = 9142301,
#'                                    option_replace = TRUE,
#'                                    v_settings = v_par,
#'                                    v_values = v_val)
#'            
#'
util_find_file_with_code <- function(files, code_to_find,
                                     option_replace = FALSE,
                                     v_settings,
                                     v_values) {
  
  requireNamespace("readr", quietly = TRUE)
  
  for (file in files) {
    #file = files[6]
    #file = files[1]
    # Read the file lines into a character vector
    lines <- readr::read_lines(file)
    
    # Find the line containing the numeric code
    code_line_index <- which(grepl(code_to_find, lines))
    
    if (length(code_line_index) > 0) {
      message("Desired code found in file: ", file)
      if (option_replace == TRUE) {
        for (i in 1:length(v_settings)) {
          #i = 1
          #v_values[i] = TRUE
          #find desired elements of the object lines
          index <- grep(v_settings[i], lines)
          #replace the values
          if (length(index) > 0) {
           if(is.logical(v_values[i])){
             #replace logical variables
             lines[index] <- gsub(paste0(v_settings[i], "=(TRUE|FALSE|true|false)"),
                                 paste0(v_settings[i],"=", v_values[i]), 
                                 lines[index])
           } else {
             #replace numeric variables
             lines[index] <- gsub("=\\d+",
                                  paste0("=", v_values[i]), 
                                  lines[index])
           }
          }
        }
        #write file back
        readr::write_lines(lines, file)
        message("Desired replacements completed in file: ", file)
      }
      
      
      return(file)
    }
  }
  
  message("Desired code not found in any file.")
  return(NULL)
}