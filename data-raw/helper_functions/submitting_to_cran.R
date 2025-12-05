# to this in the dev branch to make sure that all the fundamental issues have been resolved
# run the spell checking
usethis::use_spell_check(lang = "en-GB")

# check the links
urlchecker::url_check()

# enhanced local checks
devtools::check(remote = TRUE, manual = TRUE)
# if unicode characters present and crashing latex
# grep --color='auto' -P -n "[^\x00-\x7F]" -r *

# to reduce file sizes:
tools::resaveRdaFiles("./data")
source("./data-raw/helper_functions/resaveRDSfiles.R")
resaveRDSfiles("./inst/extdata/")


source("./data-raw/helper_functions/check_returns_in_documentation.R")

# now create a cran_submission branch for the final remote tests

# TO DO MANUALLY: update cran-comments.md accordingly
# update version number
# update news


# NEXT SUBMISSION:
# try removing donttest in examples to check if we have some problems with data.table that
# we can remove by limiting threads




# first check with rhub
# git config --global credential.helper store
usethis::create_github_token()
# then run
gitcreds::gitcreds_set()

rhub::rhub_check()
rhub::rhub_doctor()

# answer 1,2,3,4,5



# check on macos and windows via devtools
devtools::check_mac_release()
devtools::check_win_devel()

# TO DO MANUALLY: if everything passes, edit the cran-comments.md to explain any notes
devtools::release()



################################################################################
################################################################################
# For errors from r-hub, get a Docker image from:
# https://hub.docker.com/r/rhub/debian-gcc-release
# and then run with:
# docker run -ti rhub/fedora-clang-devel bash
