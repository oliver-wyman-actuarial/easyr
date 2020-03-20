*Using format from http://r-pkgs.had.co.nz/release.html*

This update is to address the recent stringAsFactors change.

## Test environments

Passed CMD check on:

* local Windows 10 Enterprise install, R 3.5.2
* R-devel https://win-builder.r-project.org/upload.aspx

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

easyr has no known downstream dependencies.

## Prior CRAN Feedback

Previously failed noLD checks. I fixed and verified via `rhub::check(platform = "debian-gcc-devel-nold")`.

