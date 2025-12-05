# Convert a geographic distance from km to m

This function takes distance in km and converts it into meters, the
units generally used by geographic operations in `R`. This is a trivial
conversion, but this functions ensures that no zeroes are lost along the
way!

## Usage

``` r
km2m(x)
```

## Arguments

- x:

  the number of km

## Value

the number of meters

## Examples

``` r
km2m(10000)
#> [1] 1e+07
km2m(1)
#> [1] 1000
```
