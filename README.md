
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazytrade

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/vzhomeexperiments/lazytrade.svg?branch=master)](https://travis-ci.org/vzhomeexperiments/lazytrade)
[![codecov](https://codecov.io/gh/vzhomeexperiments/lazytrade/branch/master/graph/badge.svg)](https://codecov.io/gh/vzhomeexperiments/lazytrade)
<!-- badges: end -->

The goal of lazytrade is to keep all functions and scripts of the lazy
trade educational project. Provided functions are providing an
opportunity to learn Computer and Data Science using example of
Algorithmic Trading

## Installation

You can install the released version of lazytrade from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lazytrade")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vzhomeexperiments/lazytrade")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lazytrade)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

# Notes to remind myself how to create R package

taken from <http://r-pkgs.had.co.nz> and <https://r-pkgs.org/intro.html>

## Generating Documentation

### Title of the package

Create right title case for the title of the package By running this
command… `tools::toTitleCase("Learn computer and data science using
algorithmic trading")` the Title will become: “Learn Computer and Data
Science using Algorithmic Trading”

### Re-generating documentation

Run this code to re-generate documentation `devtools::document()`

### Fixing Licencse

Run this code to fix license: `usethis::use_mit_license(name = "Vladimir
Zhbanko")`

## Adding data to the package for internal tests

Run this code to add data to the folder `data/` `x <- sample(1000)`
`usethis::use_data(x)`

Note: use option ’LazyLoad\` to make data available only when user wants
it always include LazyData: true in your DESCRIPTION. Note: to document
dataset see
<https://stackoverflow.com/questions/2310409/how-can-i-document-data-sets-with-roxygen>

## Adding examples to test package function

### Tests setup first time

Run this command to setup tests ‘usethis::use\_testthat()’

This will create a folder with the name `tests`

Inside this folder there will be another folder `testthat`.

### Examples in Roxygen code

@examples …

code to execute during package checks

@examples

/donttest{

code to NOT execute during package checks

}

## Testing a package

### Create a test script

Run this command to create a new script with the test skeleton:

`usethis::use_test("profit_factor.R")`

### Enrich the test script

Details:

1.  add libraries used for test
2.  add function `context("profit_factor")`
3.  add function test\_that(“test description”, {test process})
4.  load data using function `data(named_data_object)`

<!-- end list -->

``` r
library(testthat)
library(tidyverse)
#> -- Attaching packages ------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --
#> v ggplot2 3.2.0     v purrr   0.3.2
#> v tibble  2.1.3     v dplyr   0.8.3
#> v tidyr   0.8.3     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.4.0
#> -- Conflicts --------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter()  masks stats::filter()
#> x purrr::is_null() masks testthat::is_null()
#> x dplyr::lag()     masks stats::lag()
#> x dplyr::matches() masks testthat::matches()
context("profit_factor")

test_that("test value of the calculation", {

  data(profit_factor_data)

  DF_Stats <- profit_factor_data %>%
    group_by(X1) %>%
    summarise(PnL = sum(X5),
              NumTrades = n(),
              PrFact = profit_factor(X5)) %>%
    select(PrFact) %>%
    head(1) %>%
    as.vector() %>%
    round(3)

  expect_equal(DF_Stats$PrFact, 0.68)

})
```

### Test of the coverage for the script

Test coverage shows you what you’ve tested
devtools::test\_coverage\_file()

`devtools::test_coverage_file()`

### Automated checks

This will add automatic test coverage badge to the readme file on github
`usethis::use_coverage()`

## Checking package

Step 1. `devtools::document()` Step 2. `devtools::run_examples()` Step
3. Menu ‘Build’ `Clean and Rebuild` Step 4. ‘Check’

`devtools::check()`

## Handling functions that write files

In case functions are writing files there are few considerations to take
into account:

  - examples section must contain working example of code that writes
    files
  - example code must write to the temporary directory defined by
    `tempdir()` function
  - after package check performed with `devtools::check()` there should
    nothing remain in the ‘tmp/’ directory

### Considerations

File names defined by function `tempdir()` would look like this:

``` r
# > tempdir()
# [1] "/tmp/RtmpkaFStZ"
```

File names defined by function `tempfile()` would look like this:

``` r
# > tempfile()
# [1] "/tmp/RtmpkaFStZ/file7a33be992b4"
```

This is example of how function `write_csv` example works:

``` r
tmp <- tempfile()
write_csv(mtcars, tmp)
```

results of this code are correctly stored to the temporary file

however this example from `readr` package function `write_csv` is
showing that file will be written to the ‘/tmp/’ directory

``` r
dir <- tempdir()
write_tsv(mtcars, file.path(dir, "mtcars.tsv.gz"))
```

### Deleting files after running examples:

We use function `unlink()` to do this:

``` r
unlink("/tmp/*.csv", recursive = TRUE, force = TRUE)
```

and we check that there is nothing more remained:

``` r
dir("/tmp/*.csv")
```

## CRAN Note Avoidance

see
<https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when>
see <https://github.com/HughParsonage/grattan/blob/master/R/zzz.R>

## Versioning of the package

<http://r-pkgs.had.co.nz/description.html#version>

<major>.<minor>.<patch>

## Test Environments

Clone package from GitHub and test check it in Docker Container

  - started docker container vladdsm/docker-r-studio
  - new project package
  - clone from vzhomeexperiments/lazytrade.git
  - use check button to pass the test

## Build package

`devtools::build()`

## Adding Readme Rmd

`usethis::use_readme_rmd()`

## Automatic check with Travis

`usethis::use_travis()`

## Upload package to CRAN

### before release checks

spelling `devtools::spell_check()`

checking on R hub `devtools::check_rhub()`

checking win devel `devtools::check_win_devel()`

Update news.md file

releasing the package (questions) devtools::release()
usethis::use\_release\_issue()

### uploading the package archive to CRAN

<https://cran.r-project.org/submit.html>
