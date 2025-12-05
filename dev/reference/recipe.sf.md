# Recipe for `sf` objects

This method for
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
handles the case when `x` is an
[sf::sf](https://r-spatial.github.io/sf/reference/sf.html) object, as
commonly used in Species Distribution Model, and generates a
`spatial_recipe`.

## Usage

``` r
# S3 method for class 'sf'
recipe(x, ...)

spatial_recipe(x, ...)
```

## Arguments

- x:

  An [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) data
  frame.

- ...:

  parameters to be passed to
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)

## Value

An object of class `spatial_recipe`, which is a derived version of
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
, see the manpage for
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
for details.

## Details

[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
are not natively compatible with
[sf::sf](https://r-spatial.github.io/sf/reference/sf.html) objects. The
problem is that the `geometry` column of
[sf::sf](https://r-spatial.github.io/sf/reference/sf.html) objects is a
list, which is incompatible with the translation of formulae in
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).
This method strips the `geometry` column from the
[data.frame](https://rdrr.io/r/base/data.frame.html) and replaces it
with a simple `X` and `Y` columns before any further operations, thus
allowing the usual processing by
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
to succeed (`X` and `Y` are give the role of coords in a spatial
recipe). When prepping and baking a `spatial_recipe`, if a data.frame or
tibble without coordinates is used as `training` or `new_data`, dummy
`X` and `Y` columns are generated and filled with NAs. NOTE that order
matters! You need to use the syntax `recipe(x=sf_obj, formula=class~.)`
for the method to successfully detect the
[sf::sf](https://r-spatial.github.io/sf/reference/sf.html) object.
Starting with `formula` will fail.
