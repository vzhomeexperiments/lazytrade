# lazytrade (development version)

* new function `opt_aggregate_results()` to aggregate results of the trading robots performance
* new function `opt_create_graphs()` to create graphs of the trading robots performance

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


