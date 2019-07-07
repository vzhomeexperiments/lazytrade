## Test environments
* ubuntu 18.04 (on docker container), R 3.6.0
* ubuntu (on travis-ci), R 3.6.0
* win-builder (devel and release), R 3.6.0

## R CMD check results
There were no ERRORs or WARNINGs or NOTES

  * utils global variables in R/zzz.R was used to avoid Notes
  * donttest examples of the functions that would be writing to the files

## Downstream dependencies
Checked with R CMD check

## File writing examples

All functions with examples that would be writing files are excluded from tests
