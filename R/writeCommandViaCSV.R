#===============================
# WRITE COMMAND VIA CSV FUNCTION
#===============================
# write a function that will write csv files to the folder with specified address
writeCommandViaCSV <- function(x, pathToTerminal, fileName = "SystemControl"){
  if(exists("x") == TRUE && nrow(x) != 0) {
    for(i in 1:nrow(x))
    {
      # write the file for MQL4 usage
      write.csv(x[i, ], file = paste0(pathToTerminal, fileName, as.character(x[i, 1]), ".csv"),
                row.names = FALSE)
    }
  } 
}