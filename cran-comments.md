*Using format from http://r-pkgs.had.co.nz/release.html*

## Test environments

* local Windows 10 Enterprise install, R 3.4.2

## R CMD check results

There were no ERRORs or WARNINGs.

* Some of the examples are bit lazy. We'd like to release now, because many people will find it useful, and come back later to fill out the examples.

There was one NOTE:

* New submission (this is in fact a new submission).

## Downstream dependencies

This is the initial release. easyr has no downstream dependencies.

## Prior CRAN Feedback

2019-10-11 **Swetlana Herbrandt**:

* Maybe just "Actuarial Consulting" as title? 
  * Response: This is managed by our company, not the consulting profession in general. The title would be misleading if it where just "Actuarial Consulting".

* Please do not comment out your examples and use \donttest{} instead
  * Response: We have used donttest as recommended.

* Things like fldict( 'tests/testthat/test-files' ) cannot run. Please use system.file() to get the correct path.
  * Response: system.file() didn't work so I have also used donttest and changed to dummy paths for examples.

* Please ensure that your functions do not modify (save or delete) the user's home filespace in your examples/vignettes/tests. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples/vignettes/tests you can write to tempdir().
  * Response: I believe cache.init was the only function that did this. The test and examples for w() was also an issue. I've made the file path a required input for cache. init so the user must choose it, and the test and examples for w() now uses tempdir().

* Please do not install software in your examples, tests or vignettes.
  * Response: I'm not sure what this is referring to. As far as I know, I am not installing any software.
