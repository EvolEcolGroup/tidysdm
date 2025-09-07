## Test environments
- Github Actions R-CMD-check (ubuntu-20.04): r-devel, r-release, r-oldrel
- Github Actions R-CMD-check (windows): r-release
- Github Actions R-CMD-check (macOS): r-release
- R-hub r-devel: linux, m1-san, macos, macos-arm64, windows
- devtools::check_win_devel

No NOTES on any environment.

## Changes from previous version
  This is a trivial update to make the package compatible with upcoming changes
  in `ggplot2` 4.0.0.

## Issues with previous version on CRAN
* There was a NOTE about `xgboost` not being used; it is now used explicitly.