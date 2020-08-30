# lazytrade (development version)

## Planned Changes

* Add folder generation function
* Remove warnings:

1: `as_data_frame()` is deprecated as of tibble 2.0.0. Please use `as_tibble()` instead.
2: `data_frame()` is deprecated as of tibble 1.1.0. Please use `tibble()` instead.

# lazytrade 0.3.12

# Version 0.3.12

## Changes

### Implemented

* Re-write functions aml*.R


# lazytrade 0.3.11

# Version 0.3.11

## Changes

### Implemented

* Add badges 'Maturing', 'CRAN status'
* Add Methods source in Description file
* Add packages name syntax e.g. `'readr::write_csv'`
* Solve Namespace dependencies notes

# lazytrade 0.3.10

# Version 0.3.10

## Changes

* check of examples marked as /donttest, rerun with --run-donttest
* removed some examples with /donttest
* added functions `encrypt_api_key.R`, `mt_make_model.R`
* minor enhancements

# lazytrade 0.3.9

# Version 0.3.9

## Changes

* simplification of R/h2o updates, function `aml_make_model()` may conditionally force model update
* added function to simplify creation of configuration files to launch MT4 platform

# lazytrade 0.3.8

# Version 0.3.8

## Changes

* added utility `util_generate_password()` to generate passwords e.g. for MT4 platform

# lazytrade 0.3.7

# Version 0.3.7

## Changes

* new function `opt_aggregate_results()` to aggregate results of the trading robots performance
* new function `opt_create_graphs()` to create graphs of the trading robots performance
* added more tests and removal of warning by using `ungroup()` function

# lazytrade 0.3.6

# Version 0.3.6

## Changes

* removal of duplicate rows in the `aml_collect_data()` function
* added a test for `to_m()` function
* add warning to indicate if 'zeroes' are present in the data file column(s)
* removing dependency from tidyverse

# lazytrade 0.3.5

# Version 0.3.5

## Changes

* added more datasets (files) to test functionality
* building a functionality to create separate models for each asset (adding functions with prefix aml_xxx_xxx.R)
* build more simple functions to collect data, build model, score data to predict
* added tests for several functions
* function aml_make_model() will be finding the best neural network structure automatically using 6 random structures
* fixing of warnings due to column name assignments

# lazytrade 0.3.4

# Version 0.3.4

## Changes

* corrected bug on file naming for Terminal 3 on function record_policy_mt.R
* better defined R dependency

# lazytrade 0.3.3

# Version 0.3.3

## Changes

* corrected file writing example to be aligned with CRAN Policy
* added test coverage integration

# lazytrade 0.3.2

# Version 0.3.2

## Changes

* corrected file writing example to be aligned with CRAN Policy

# lazytrade 0.3.1

# Version 0.3.1

## Changes

* corrected according to the CRAN team comments

# lazytrade 0.3.0

# Version 0.3.0

## Changes

* Initial release
* Passing all checks


