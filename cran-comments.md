## Test environments
* ubuntu 18.04 (on docker container), R 3.6.0
* ubuntu (on travis-ci), R 3.6.0
* win-builder (devel and release), R 3.6.0

## R CMD check results
There were no ERRORs or WARNINGs or NOTES

  * utils global variables in R/zzz.R was used to avoid Notes
  * used function `testfile()` instead of `testdir()` to make sure no files remained

## Downstream dependencies
Checked with R CMD check

## File writing examples

Figured out that having indicated function `tempdir()` in the examples would result in having files in the '/tmp/' directory. To avoid that similar function `tempfile()` was used with good results. No more files are generated in '/tmp/'
