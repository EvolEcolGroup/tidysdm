# Convert an object created with `blockCV` to an `rsample` object

This function converts objects created with `blockCV` to `rsample`
objects that can be used by `tidysdm`. BlockCV provides more
sophisticated sampling options than the `spatialsample` library. For
example, it is possible to stratify the sampling to ensure that
presences and absences are evenly distributed among the folds (see the
example below).

## Usage

``` r
blockcv2rsample(x, data)
```

## Arguments

- x:

  a object created with a `blockCV` function

- data:

  the `sf` object used to create `x`

## Value

an `rsample` object

## Details

Note that currently only objects of type `cv_spatial`, `cv_cluster`,
`cv_nndm` and `cv_buffer` are supported. The latter two are
one-out-cross validation methods, so the resulting `rsample` object will
have `n` splits, where `n` is the number of folds in the original
`blockCV` object (which can be very large!).

## Examples

``` r
library(blockCV)
points <- read.csv(system.file("extdata/", "species.csv",
  package = "blockCV"
))
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
sb1 <- cv_spatial(
  x = pa_data,
  column = "occ", # the response column to balance the folds
  k = 5, # number of folds
  size = 350000, # size of the blocks in metres
  selection = "random", # random blocks-to-fold
  iteration = 10
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======                                                               |  10%  |                                                                              |==============                                                        |  20%  |                                                                              |=====================                                                 |  30%  |                                                                              |============================                                          |  40%  |                                                                              |===================================                                   |  50%  |                                                                              |==========================================                            |  60%  |                                                                              |=================================================                     |  70%  |                                                                              |========================================================              |  80%  |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%
#>   train_0 train_1 test_0 test_1
#> 1     209     183     48     60
#> 2     216     175     41     68
#> 3     198     195     59     48
#> 4     188     212     69     31
#> 5     217     207     40     36


sb1_rsample <- blockcv2rsample(sb1, pa_data)
class(sb1_rsample)
#> [1] "cv_spatial"   "spatial_rset" "rset"         "tbl_df"       "tbl"         
#> [6] "data.frame"  
autoplot(sb1_rsample)
```
