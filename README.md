# tidysdm <img src="./man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EvolEcolGroup/tidysdm/branch/main/graph/badge.svg?token=KLOzxJoLBO)](https://app.codecov.io/gh/EvolEcolGroup/tidysdm)
[![CRAN status](https://www.r-pkg.org/badges/version/tidysdm)](https://CRAN.R-project.org/package=tidysdm)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tidysdm)](https://github.com/r-hub/cranlogs.app)
<!-- badges: end -->

The goal of `tidysdm` is to implement Species Distribution Models using the
`tidymodels` framework. The advantage of `tidymodels` is that the model syntax and the results
returned to the user are standardised, thus providing a coherent interface to
modelling. Given the variety of models required for SDM, `tidymodels` is an
ideal framework. `tidysdm` provides a number of wrappers and specialised
functions to facilitate the fitting of SDM with `tidymodels`.

Besides modelling contemporary species, `tidysdm` has a number of functions
specifically designed to work with palaeontological data. 

Whilst users are free
to use their own environmental data, the articles showcase the potential integration
with [`pastclim`](https://evolecolgroup.github.io/pastclim/dev/index.html), 
which helps downloading and manipulating present day data,
future predictions, and palaeoclimate reconstructions.

An overview of the capabilities of `tidysdm` is given in [Leonardi et al.
(2024)]( https://doi.org/10.1111/2041-210X.14406).

## Installation

`tidysdm` is on CRAN, and the easiest way to install it is with:

```         
install.packages("tidysdm")
```

The version on CRAN is recommended for every day use. New features and
bug fixes appear first on the `dev` branch on GitHub, before they make
their way to CRAN. If you need to have early access to these new
features, you can install the latest `dev` version of `tidysdm` 
from r-universe with:

``` r
install.packages("tidysdm", repos = c("https://evolecolgroup.r-universe.dev", 
                                      "https://cloud.r-project.org"))
```

Alternatively, you can also use `devtools` and install the package from source
directly from GitHub, but you
might need to set up your development environment first:

``` r
# install.packages("devtools") # if you haven't installed devtools yet
devtools::install_github("EvolEcolGroup/tidysdm", ref = "dev")
```

## Overview of functionality

On its dedicated [website](https://evolecolgroup.github.io/tidysdm/),
you can find Articles giving you a step-by-step [overview of the
fitting SDMs to contemporary species](https://evolecolgroup.github.io/tidysdm/articles/a0_tidysdm_overview.html),
as well as an equivalent [tutorial for using palaeontological data](https://evolecolgroup.github.io/tidysdm/articles/a1_palaeodata_application.html).
Furthermore, there is an [Article with examples of how to leverage various
features of tidymodels](https://evolecolgroup.github.io/tidysdm/articles/a2_tidymodels_additions.html) that are not commonly adopted in SDM pipelines

There is also a [dev
version](https://evolecolgroup.github.io/tidysdm/dev/) of the site
updated for the `dev` branch of `tidysdm` (on the top left of the dev
website, the version number is in red and in the format x.x.x.9xxx,
indicating it is a development version). If you want to contribute, make
sure to read our [contributing guide](https://evolecolgroup.github.io/tidysdm/CONTRIBUTING.html).

--------------------------------------------------------------------------------

## Getting help

If some of your models are failing, first look at our [Article on how to diagnose failing
models](https://evolecolgroup.github.io/tidysdm/dev/articles/a3_troubleshooting.html).
It is not a fully comprehensive list of everything that could go wrong, but it will
hopefully give you ideas on how to dig deeper in what is wrong.

If after reading the article you are still unsure what is going wrong, there are 
two places to get help with `tidysdm`:

1) If you are unsure how to do something, go to [StackOverflow](https://stackoverflow.com/) and,
after checking that a similar question has not been asked yet, tag your question 
with `tidymodels` and `r` (there is no `tidysdm` tag yet, as there aren't enough questions),
and make sure `tidysdm` is in the title. This will ensure that the developers
see your question and can help you. If you have not received an answer after a couple of days,
feel free to drop us an email in case we missed your post.

2) If you think you have found a bug, or have a feature request, open an issue on our
[GitHub repository](https://github.com/EvolEcolGroup/tidysdm/issues). Before doing so, please 
make sure that you have installed the latest **development** version of
`tidysdm` (as the bug might have already been fixed), as well as updating 
all other packages on your system. If the problem persists, and there is no issue
already opened that deals with your bug, file a new issue **providing** a [reproducible
example](https://reprex.tidyverse.org/)
for the developers to investigate the problem. A small **reproducible example** is
crucial in allowing us to understand the problem and fix it, so please do your best to
come up with the shortest bit of code needed to show the bug.
