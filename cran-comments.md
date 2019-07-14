## Test environments
* windows, R 3.6.1
* ubuntu 18.04 (on docker container), R 3.6.1
* ubuntu (on travis-ci), R 3.6.1
* win-builder (devel and release), R 3.6.1

## R CMD check results
There were no ERRORs or WARNINGs or NOTES

  * utils global variables in R/zzz.R was used to avoid Notes
  * used function `tempfile()` instead of `tempdir()` to make sure no files remained in '/tmp/' directory
  * most of examples are enabled in any case these would not write to the tmp anyway

## Downstream dependencies
Checked with R version 3.5.3 (2019-03-11)
