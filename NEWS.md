# lazytrade (development version)

## Planned Changes

* setup github actions
* add fail safe for function input parameters

# lazytrade 0.5.4

# Version 0.5.4

## Changes

* added function to find a file path based on code and replace parameters
* updated web links
* reduced execution time by reducing certain examples
* used R version 4.4.1

# lazytrade 0.5.3

# Version 0.5.3

## Changes

* removed as.vector from example code to fix error in R-devel builds
* increased min R version to 3.6.0

# lazytrade 0.5.2

# Version 0.5.2

## Changes

* add second parameter to simulation function `aml_simulation`
* option to use full columns for model training when selecting 0 as a parameter `num_cols_used`
* add suppress messages option during `readr::read_csv()` function calls
* fail safe in `aml_collect_data` function will delete already recorded rds file if it has different amount of columns
* add new function `util_find_pid` to find the PIDs of the terminal.exe application
* function `mt_stat_transf` is now using a rule to assign 3 market type classes
* rewrite function `mt_make_model` with the same philosophy as in `aml_make_model`

# lazytrade 0.5.1

# Version 0.5.1

## Changes

### New Function `aml_simulation`

* designed to test different inputs

### Function `aml_make_model`

* fix bug in not allowing use of full input to the model
* create new option with fix neural network structure
* add parameter to split data with specific n of rows
* add parameter num_bars_ahead, default is 34 bars
* add options for deep learning function parameters search, `num_epochs`, `fixed_nn_struct`
* add option `objective_test` to allow strategy test inside the function to select best model based on specific objective

### Function `aml_collect_data`

* more accurate calculation of Label column using tick data
* added fail safe scenarios

# lazytrade 0.4.5

# Version 0.4.5

## Changes

* add option in function `aml_consolidateresults.R`to help understand overall model performances and log results
* removed unused functions and datasets
* add lifecycle badges



# lazytrade 0.4.4

# Version 0.4.4

## Changes

* added fail safe stops in function `check_if_optimize.R`
* experimenting with time series statistical transformation adding functions `mt_stat_transf` and `mt_stat_evaluate`. Learning opportunity around clustering for automatic market type generation
* added fail safe and change the way how to find control parameters for Reinforcement Learning to avoid slow code execution

# lazytrade 0.4.3

# Version 0.4.3

## Changes

* solving warnings by changing example in *mt_make_model* function
* change function *mt_make_model* to read manually checked dataset
* update documentation

# lazytrade 0.4.2

# Version 0.4.2

## Changes

* solving warnings
* added parameter min_perf to adjust model performance
* consider tick value in the calculations of strategy testing

# lazytrade 0.4.1

# Version 0.4.1

## Changes

### Implemented

* added option to balance classes in mt_make_model function
* add and delete deprecated functions
* change names of functions to reflect the purpose: mt - market type, rl- reinforcement learning, util - utility
* change example methods to unify them across functions
* align names of the variables across several functions
* remove warnings caused by new dplyr functionalities .groups
* remove tidyr from imports to solve Note

# lazytrade 0.4.0

# Version 0.4.0

## Changes

### Implemented

* Added extended package features in the Readme file
* Re-write functions aml*.R
* aml_collect.. will keep the time index
* aml_make_model ... will build a model
* aml_score... is designed to use model and predict
* aml_test... is designed to perform a strategy test
* mt_make_model .. rewrite in the similar fashion as in aml function
* examples and tests are available

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


