This is a new package not present on CRAN yet.

On all testing environment (see below), we only get a note because of the
package being a new submission, and a false positive for mispelling 
(author name in the reference, and tidymodels).

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- devtools::check_mac_release
- devtools::check_win_devel()

## R CMD check results
On all systems:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Andrea Manica <am315@cam.ac.uk>’

New submission

Possibly misspelled words in DESCRIPTION:
    Leonardi (9:6)
    Tidymodels (2:41)
    al (9:18)
    et (9:15)
    tidymodels (8:56)

0 errors ✔ | 0 warnings ✔ | 1 notes ✖

## Changes from initial submission
The vignettes and one example were flagged for CPU time >2.5 times elapsed time.
