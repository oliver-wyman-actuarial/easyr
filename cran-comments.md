*Using format from http://r-pkgs.had.co.nz/release.html*

## Test environments

Passed CMD check on:

* local Windows 10 Enterprise install, R 4.0.4
* R-devel https://win-builder.r-project.org/upload.aspx
* noLD via rhub::check(platform = "debian-gcc-devel-nold")

## R CMD check results

noLD returned NOTE about rprojroot import not being used, but this is not accurate. It is used at tests\testthat\helper.R.

There were no other NOTEs, ERRORs, or WARNINGs.

## Downstream dependencies

easyr has no known downstream dependencies.

## Prior CRAN Feedback

Updating to fix failed tests using `format(digits = 0, ...)`.

