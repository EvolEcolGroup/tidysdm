# tidysdm <img src="./man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EvolEcolGroup/tidysdm/branch/main/graph/badge.svg?token=KLOzxJoLBO)](https://app.codecov.io/gh/EvolEcolGroup/tidysdm)
[![CRAN status](https://www.r-pkg.org/badges/version/tidysdm)](https://CRAN.R-project.org/package=tidysdm)
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
(2023)](https://doi.org/10.1101/2023.07.24.550358).

## Installation

`tidysdm` is on CRAN, and the easiest way to install it is with:

```         
install.packages("tidysdm")
```

The version on CRAN is recommended for every day use. New features and
bug fixes appear first on the `dev` branch on GitHub, before they make
their way to CRAN. If you need to have early access to these new
features, you can install `tidysdm` directly from GitHub. To install
from GitHub, you will need to use `devtools`; if you haven't done so
already, get it from CRAN with `install.packages("devtools")`.


You can install the latest `dev` version of tidysdm from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvolEcolGroup/tidysdm", ref = "dev")
```

## Overview of functionality

On its dedicated [website](https://evolecolgroup.github.io/tidysdm/),
you can find Articles giving you a step-by-step [overview of the
fitting SDMs to contemporary species](https://evolecolgroup.github.io/tidysdm/articles/a0_tidysdm_overview.html),
as well as an equivalent [tutorial for using palaeontological data](https://evolecolgroup.github.io/tidysdm/articles/a1_palaeodata_application.html).
Furthermore, there is an [Article with examples of how to leverage various
features of tidymodels](https://evolecolgroup.github.io/tidysdm/dev/articles/a2_tidymodels_additions.html) that are not commonly adopted in SDM pipelines

There is also a [dev
version](https://evolecolgroup.github.io/tidysdm/dev/) of the site
updated for the `dev` branch of `tidysdm` (on the top left of the dev
website, the version number is in red and in the format x.x.x.9xxx,
indicating it is a development version). If you want to contribute, make
sure to read our [contributing guide](https://evolecolgroup.github.io/tidysdm/CONTRIBUTING.html).

## When something does not work

What should you do if you get an error when trying to fit a model? `tidysdm`
is a relatively new package, so it might well be that, when you get an
error, you might have encountered a bug. However, it is also possible that you
have misspecified your model (and so the error comes from `tidymodels`, because
your model is not valid). We have prepared an [Article on how to diagnose failing
models](https://evolecolgroup.github.io/tidysdm/dev/articles/a3_troubleshooting.html).
It is not a fully comprehensive list of everything that could go wrong, but it will
hopefully give you ideas on how to dig deeper in what is wrong. You should also
check the [issues on
GitHub](https://github.com/EvolEcolGroup/tidysdm/issues) to see whether
the problem has already been reported. 

If you are convinced
that the problem is a bug in `tidysdm`, feel free to create an
new issue. Please make sure you have updated to the latest version of
`tidysdm`, as well as updating all other packages on your
system, and provide [a reproducible
example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example)
for the developers to investigate the problem. If you think you can help
with fixing that bug, read our [contributing guide](https://evolecolgroup.github.io/tidysdm/CONTRIBUTING.html).
