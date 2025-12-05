# Check that the column with presences is correctly formatted

In `tidysdm`, the string defining presences should be the first level of
the response factor. This function checks that the column is correctly
formatted.

## Usage

``` r
check_sdm_presence(.data, .col, presence_level = "presence")
```

## Arguments

- .data:

  a `data.frame` or `tibble`, or a derived object such as an `sf`
  data.frame, or a factor (e.g. the column with the response variable)

- .col:

  the column containing the presences

- presence_level:

  the string used to define the presence level of `.col`

## Value

TRUE if correctly formatted
