# Convert a time interval from years to days

This function takes takes a time interval in years and converts into
days, the unit commonly used in time operations in `R`. The simple
conversion x \* 365 does not work for large number of years, due to the
presence of leap years.

## Usage

``` r
y2d(x)
```

## Arguments

- x:

  the number of years of the interval

## Value

a `difftime` object (in days)

## Examples

``` r
y2d(1)
#> Time difference of 365 days
y2d(1000)
#> Time difference of 365243 days
```
