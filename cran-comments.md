## Test environments
* windows, R 3.6.1
* ubuntu 18.04 (on docker container), R 3.6.1
* ubuntu (on travis-ci), R 3.6.1
* win-builder (devel and release), R 3.6.1

## R CMD check results
There were no ERRORs or WARNINGs or NOTES

  * utils global variables in R/zzz.R was used to avoid Notes
  * used normalizePath() and file.path() to make sure examples are working and not writing to the tmp/ folder

## Downstream dependencies
Checked with R version 3.5.3 (2019-03-11)
