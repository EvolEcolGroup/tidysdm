# to this in the dev branch to make sure that all the fundamental issues have been resolved
# run the spell checking
usethis::use_spell_check(lang = "en-GB")

# check the links
urlchecker::url_check()

# enhanced local checks
devtools::check(remote = TRUE, manual = TRUE)
# if unicode characters present and crashing latex
# grep --color='auto' -P -n "[^\x00-\x7F]" -r * 


source("./data-raw/helper_functions/check_returns_in_documentation.R")

# now create a cran_submission branch for the final remote tests

# TO DO MANUALLY: update cran-comments.md accordingly
# update version number
# update news

# first check with rhub
tidysdm_checks <- rhub::check_for_cran()

# check on macos and windows via devtools
devtools::check_mac_release()
devtools::check_win_devel()

# TO DO MANUALLY: if everything passes, edit the cran-comments.md to explain any notes
devtools::release()
