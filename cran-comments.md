## Test environments
- Github Actions R-CMD-check (ubuntu-20.04): r-devel, r-release, r-oldrel
- Github Actions R-CMD-check (windows): r-release
- Github Actions R-CMD-check (macOS): r-release
- R-hub r-devel: linux, m1-san, macos, macos-arm64, windows
- devtools::check_win_devel

No NOTES on any environment.

## Issues with previous version on CRAN
* There was an error due to a suggested package, BlockCV, being removed from
  CRAN, but it is now reinstated, so everything works again.
  
* There were some tests and examples failing if not all suggested packages were
  installed. This is now fixed, and tests/examples relying on suggested packages
  are only run if those packages are installed.