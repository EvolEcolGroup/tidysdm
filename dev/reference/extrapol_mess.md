# Multivariate environmental similarity surfaces (MESS)

Compute multivariate environmental similarity surfaces (MESS), as
described by Elith et al., 2010.

## Usage

``` r
extrapol_mess(x, training, .col, ...)

# Default S3 method
extrapol_mess(x, training, ...)

# S3 method for class 'stars'
extrapol_mess(x, ...)

# S3 method for class 'SpatRaster'
extrapol_mess(x, training, .col, filename = "", ...)

# S3 method for class 'data.frame'
extrapol_mess(x, training, .col, ...)

# S3 method for class 'SpatRasterDataset'
extrapol_mess(x, training, .col, ...)
```

## Arguments

- x:

  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  `stars`,
  [`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`data.frame`](https://rdrr.io/r/base/data.frame.html)

- training:

  matrix or data.frame or sf object containing the reference values;
  each column should correspond to one layer of the
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, with the exception of the presences column defined in `.col`
  (optional).

- .col:

  the column containing the presences (optional). If specified, it is
  excluded when computing the MESS scores.

- ...:

  additional arguments as for
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

- filename:

  character. Output filename (optional)

## Value

a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
(data.frame) with the MESS values.

## Details

This function is a modified version of `mess` in package `predicts`,
with a method added to work on
[`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
Note that the method for
[`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
assumes that each variables is stored as a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with time information within `x`. Time is also assumed to be in `years`.
If these conditions are not met, it is possible to manually extract a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
for each time step, and use `extrapol_mess` on those
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)s

## References

Elith J., M. Kearney M., and S. Phillips, 2010. The art of modelling
range-shifting species. Methods in Ecology and Evolution 1:330-342.

## Author

Jean-Pierre Rossi, Robert Hijmans, Paulo van Breugel, Andrea Manica
