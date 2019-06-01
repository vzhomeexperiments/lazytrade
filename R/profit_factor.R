
#===============================
# PROFIT FACTOR FUNCTION
#===============================
# function that returns the profit factors of the systems
#
# x - column vector with profit or loss of the orders for one system
#     function should calculate profit factor for this vector and return one value also as vector
# require(magrittr)
# x <- read_rds('test_data.rds') %>% select(X5) %$% X5 
# x <- read_rds('test_data.rds') %>% filter(X5 > 0) %>% select(X5) %$% X5 
# x <- read_rds('test_data.rds') %>% filter(X5 < 0) %>% select(X5) %$% X5 

profit_factor <- function(x){
  
  res <- sum(x[x>0])/(0.0001+sum(abs(x[x<0])))
  
  # limit profit factor to maximum value of 10 (in case no losses)
  if(res > 100) res <- 10 
  return(res)
  
  }
  
# test this function
# profit_factor(x)
