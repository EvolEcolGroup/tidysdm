# Get the response variable from a formula

This is the counterpart of
[rsample::form_pred](https://rsample.tidymodels.org/reference/form_pred.html).

## Usage

``` r
form_resp(x)
```

## Arguments

- x:

  a formula

## Value

character the name of the response

## Details

Note: this might not behave well with functions such as log(y). But
neither does
[rsample::form_pred](https://rsample.tidymodels.org/reference/form_pred.html).

modified from
https://stackoverflow.com/questions/13217322/how-to-reliably-get-dependent-variable-name-from-formula-object
