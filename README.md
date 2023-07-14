# tidysdm <img src="./man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EvolEcolGroup/tidysdm/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EvolEcolGroup/tidysdm/branch/main/graph/badge.svg?token=KLOzxJoLBO)](https://codecov.io/gh/EvolEcolGroup/tidysdm)
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

## Installation

`tidysdm` is still at the **beta** stage of development, and there is no official release on
CRAN yet.

You can install the latest version of tidysdm from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvolEcolGroup/tidysdm")
```

For the development version of `tidysdm`, which includes experimental features
that might not be mature yet, use:
``` r
devtools::install_github("EvolEcolGroup/tidysdm", ref = "dev")
```


To take advantage of the integration with `pastclim` highlighted in the articles, you
will need the `dev` version (NOT the one on CRAN). You can obtain it with:
``` r        
install.packages('terra', repos='https://rspatial.r-universe.dev')

devtools::install_github("EvolEcolGroup/pastclim", ref="dev")
```


## Overview of functionality

On its dedicated [website](https://evolecolgroup.github.io/tidysdm/),
you can find Articles giving you a step-by-step [overview of the
fitting SDMs to contemporary species](https://evolecolgroup.github.io/tidysdm/articles/a0_tidysdm_overview.html),
as well as an equivalent [tutorial for using palaeontological data](https://evolecolgroup.github.io/tidysdm/articles/a1_palaeodata_application.html). There is also a [dev
version](https://evolecolgroup.github.io/tidysdm/dev/) of the site
updated for the `dev` branch of `tidysdm` (on the top left of the dev
website, the version number is in red and in the format x.x.x.9xxx,
indicating it is a development version).
