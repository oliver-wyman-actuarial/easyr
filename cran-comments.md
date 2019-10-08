*Using format from http://r-pkgs.had.co.nz/release.html*

## Test environments

* local Windows 10 Enterprise install, R 3.4.2

## R CMD check results

There were no ERRORs or WARNINGs.

* Some of the examples are bit lazy. We'd like to release now and come back later to fill out the examples.
* read.txt, read.any have a commented-out example because I couldn't find a way to read a file in a test that passes R CMD check --as-cran.

There was one NOTE:

* New submission (this is in fact a new submission).

## Downstream dependencies

This is the initial release. easyr has no downstream dependencies.