# Distance between the distribution of climate values for presences vs background

For each environmental variable, this function computes the density
functions of presences and absences and returns (1-overlap), which is a
measure of the distance between the two distributions. Variables with a
high distance are good candidates for SDMs, as species occurrences are
confined to a subset of the available background.

## Usage

``` r
dist_pres_vs_bg(.data, .col)
```

## Arguments

- .data:

  a `data.frame` (or derived object, such as `tibble`, or `sf`) with
  values for the bioclimate variables for presences and background

- .col:

  the column containing the presences; it assumes presences to be the
  first level of this factor

## Value

a name vector of distances

## Examples

``` r
# This should be updated to use a dataset from tidysdm
data("bradypus", package = "maxnet")
bradypus_tb <- tibble::as_tibble(bradypus) %>%
  dplyr::mutate(presence = relevel(
    factor(
      dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
    ),
    ref = "presence"
  )) %>%
  select(-ecoreg)

bradypus_tb %>% dist_pres_vs_bg(presence)
#> pre6190_l10 frs6190_ann tmn6190_ann pre6190_ann vap6190_ann  pre6190_l7 
#>   0.4366602   0.4299480   0.4295013   0.4096230   0.3945855   0.3933454 
#>       h_dem tmp6190_ann dtr6190_ann  pre6190_l4 tmx6190_ann cld6190_ann 
#>   0.3647375   0.3316686   0.3288771   0.2544976   0.2418274   0.1812527 
#>  pre6190_l1 
#>   0.1297035 
```
