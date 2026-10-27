# Create an initial train/test split from a blockCV object

Converts a `blockCV` object to an `rsample` object and randomly selects
one fold as the assessment (test) set, with the remaining folds used as
the analysis (training) set.

## Usage

``` r
blockcv2initial_split(x, data)
```

## Arguments

- x:

  A blockCV object (currently we only support classes `cv_spatial`,
  `cv_cluster`, `cv_nndm` and `cv_buffer`).

- data:

  The sf object used to create `x`.

## Value

An `rsplit` object that can be used with
[`rsample::training()`](https://rsample.tidymodels.org/reference/initial_split.html)
and
[`rsample::testing()`](https://rsample.tidymodels.org/reference/initial_split.html).

## Examples

``` r
library(blockCV)
#> blockCV 4.0.1
# import presence-absence species data
points <- read.csv(system.file("extdata/", "species.csv",
  package = "blockCV"
))
# make an sf object from data.frame
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
sb1 <- blockCV::cv_spatial(
  x = pa_data, column = "occ",
  k = 5, size = 350000
)

split <- blockcv2initial_split(sb1, pa_data)
training(split)
#> Simple feature collection with 387 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -1733041 ymin: -4911422 xmax: 1887137 ymax: -1342594
#> Projected CRS: GDA2020 / GA LCC
#> First 10 features:
#>    occ                  geometry
#> 1    0 POINT (-1733041 -3644787)
#> 2    0 POINT (-1698808 -3670463)
#> 3    0 POINT (-1621782 -3807396)
#> 4    1 POINT (-1707366 -3807396)
#> 5    0 POINT (-1570432 -3764604)
#> 6    0 POINT (-1690249 -3670463)
#> 7    0 POINT (-1698808 -3773163)
#> 8    0 POINT (-1407824 -3815954)
#> 9    0 POINT (-1279449 -3764604)
#> 10   0 POINT (-1365032 -3961446)
testing(split)
#> Simple feature collection with 113 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -1741599 ymin: -4757372 xmax: 1801554 ymax: -1342594
#> Projected CRS: GDA2020 / GA LCC
#> First 10 features:
#>    occ                  geometry
#> 1    1 POINT (-1681691 -3884421)
#> 2    1 POINT (-1733041 -3918654)
#> 3    1 POINT (-1707366 -3918654)
#> 4    1 POINT (-1467732 -4021354)
#> 5    0 POINT (-1467732 -4012796)
#> 6    0 POINT (-1561874 -3833071)
#> 7    1 POINT (-1733041 -3952888)
#> 8    0 POINT (-1587549 -3875863)
#> 9    0 POINT (-1493407 -4004238)
#> 10   1 POINT (-1442057 -4012796)
```
