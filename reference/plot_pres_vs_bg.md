# Plot presences vs background

Create a composite plots contrasting the distribution of multiple
variables for presences vs the background.

## Usage

``` r
plot_pres_vs_bg(.data, .col)
```

## Arguments

- .data:

  a [`data.frame`](https://rdrr.io/r/base/data.frame.html) (or derived
  object, such as
  [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html),
  or [`sf::st_sf`](https://r-spatial.github.io/sf/reference/sf.html))
  with values for the bioclimate variables for presences and background

- .col:

  the column containing the presences; it assumes presences to be the
  first level of this factor

## Value

a `patchwork` composite plot

## Examples

``` r
data("bradypus", package = "maxnet")
bradypus_tb <- tibble::as_tibble(bradypus) %>%
  dplyr::mutate(presence = relevel(
    factor(
      dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
    ),
    ref = "presence"
  )) %>%
  select(-ecoreg)

bradypus_tb %>% plot_pres_vs_bg(presence)
```
