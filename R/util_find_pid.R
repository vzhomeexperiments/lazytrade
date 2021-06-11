#' R function to find PID of active applications
#'
#' @description Utility function to find PID of the working terminal.exe application
#' Function is created to generate a system call to programmatically close
#' any given application
#'
#' `r lifecycle::badge('experimental')`
#'
#' @details Function is executing a system command to get all processes running on the OS
#' Retrieved data is cleaned and organized to filter on required process
#' Function can also be used to track specific applications defined by the user
#'
#' @author (C) 2021 Vladimir Zhbanko
#'
#' @param tasks_running - string, vector with system tasks
#' @param pid_pattern - string, pattern value to search application with
#'
#' @return string with system kill command to close selected application
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' library(tibble)
#' library(stringr)
#' library(dplyr)
#'
#' #library(readr)
#'
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#'
#' #tasks_running <- system("tasklist", intern = TRUE)
#' #writeLines(tasks_running, con = file.path(dir,'tasks_running.txt'))
#' #t_running <- readLines(con = file.path(dir,'tasks_running.txt'))
#'
#' tasks_list = system.file("extdata", "tasks_running.txt",
#'                   package = "lazytrade")
#'
#' t_running <- readLines(con = tasks_list)
#'
#' #generate task kill command for this application
#' util_find_pid(tasks_running = t_running,
#'               pid_pattern = 'terminal.exe')
#'
#' util_find_pid(tasks_running = t_running,
#'               pid_pattern = 'chrome.exe')
#'
#'
util_find_pid <- function(tasks_running = t_running,
                          pid_pattern = 'terminal.exe') {

  # remove 1st and 3rd elements of a vector
  df_t <- tasks_running[-c(1,3)]
  df_l <- df_t %>% stringr::str_detect(pattern = pid_pattern)

  df_t1 <- df_t[df_l]

  df_s <- stringr::str_split(df_t1, pattern = " ")

  for (IN in 1:length(df_s)) {
    #IN <- 1
    x <- df_s[[IN]]
    y <- unique(x[x != ""]) %>% tibble::as_tibble()

    if(!exists("z")){
      z <- y
    } else {
      z <- z %>% dplyr::bind_cols(y)
    }

  }

  z1 <- z[1:2, ]

  z_tasks <- vector(mode = "character",length = ncol(z1))

  ## execute system calls on these tasks

  # command should look like this:
  # TASKKILL /PID [val] /PID [val] ... /F

  sys_command_kill <- paste0("TASKKILL ")

  for (SS in 1:ncol(z1)) {
    # SS <- 1
    z_tasks[SS] <- z1[[2, SS]]
    sys_command_kill <- paste(sys_command_kill, "/PID", z_tasks[SS])
  }

  sys_command_kill <- paste(sys_command_kill, "/F")
  return(sys_command_kill)

}
