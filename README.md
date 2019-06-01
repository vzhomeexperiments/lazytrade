# lazytrade

all functions and scripts of the lazy trade project

# Notes to remind myself how to create R package

taken from http://r-pkgs.had.co.nz

## Generating Documentation

Run this code to re-generate documentation
`devtools::document()`

## Adding data to the package for internal tests

Run this code to add data to the folder `data/`
`x <- sample(1000)`
`usethis::use_data(x)`

Note: use option 'LazyLoad` to make data available only when user wants it
always include LazyData: true in your DESCRIPTION. 

## Adding examples to test package function



