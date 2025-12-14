## Test environments
- Github Actions R-CMD-check (ubuntu-20.04): r-devel, r-release, r-oldrel
- Github Actions R-CMD-check (windows): r-release
- NOT tested on Github Actions R-CMD-check (macOS): r-release as it is currently broken

No NOTES on any environment.

## Changes from previous version
  This is a trivial update to deal with changes in the `xgboost` package.

## Issues with previous version on CRAN
* There are errors due to `xgboost` changing its object structures, and thus making
  some of the saved objects in `tidysdm` incompatible with the new version of
  `xgboost`. This is fixed in this version.