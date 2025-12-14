# Get default grid cellsize for a given dataset

This function facilitates using
[spatialsample::spatial_block_cv](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
multiple times in an analysis.
[spatialsample::spatial_block_cv](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
creates a grid based on the object in `data`. However, if spatial blocks
are generated multiple times in an analysis (e.g. for a
[`spatial_initial_split()`](https://evolecolgroup.github.io/tidysdm/reference/spatial_initial_split.md),
and then subsequently for cross-validation on the training dataset), it
might be desirable to keep the same grid). By applying this function to
the largest dataset, usually the full dataset before
[`spatial_initial_split()`](https://evolecolgroup.github.io/tidysdm/reference/spatial_initial_split.md).
The resulting cellsize can be used as an option in
[spatialsample::spatial_block_cv](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html).

## Usage

``` r
grid_offset(data)
```

## Arguments

- data:

  a [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) dataset
  used to size the grid

## Value

the grid offset
