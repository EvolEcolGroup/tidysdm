# Create a formula for gam

This function takes the formula from a recipe, and turns numeric
predictors into smooths with a given k. This formula can be passed to a
workflow or workflow set when fitting a gam.

## Usage

``` r
gam_formula(object, k = 10)
```

## Arguments

- object:

  a
  [recipes::recipe](https://recipes.tidymodels.org/reference/recipe.html),
  already trained

- k:

  the *k* value for the smooth

## Value

a formula
