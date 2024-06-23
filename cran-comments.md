This is a resubmission. The previous version raised a problem of some remaining missing package
anchors. All documentation should now include the required package anchors.

## Test environments
- R-hub linux (r-devel)
- R-hub macos (r-devel)
- R-hub macos-arm64 (r-devel)
- devtools::check_mac_release
- devtools::check_win_devel

## R CMD check results
Not issues on any system.

## Issues with previous version on CRAN
A few tests showed a problem related to a recent change in `pastclim`, where a variable
is now returned as a discrete factor and is thus incompatible with a continuous 
colour scale used for plotting. The plotting code has now been fixed to use a
discrete scale.