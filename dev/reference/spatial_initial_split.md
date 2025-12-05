# Simple Training/Test Set Splitting for spatial data

`spatial_initial_split` creates a single binary split of the data into a
training set and testing set. All strategies from the package
`spatialsample` are available; a random split from that strategy will be
used to generate the initial split.

## Usage

``` r
spatial_initial_split(data, prop, strategy, ...)
```

## Arguments

- data:

  A dataset (data.frame or tibble)

- prop:

  The proportion of data to be retained for modelling/analysis.

- strategy:

  A sampling strategy from `spatialsample`

- ...:

  parameters to be passed to the `strategy`

## Value

An `rsplit` object that can be used with the
[rsample::training](https://rsample.tidymodels.org/reference/initial_split.html)
and
[rsample::testing](https://rsample.tidymodels.org/reference/initial_split.html)
functions to extract the data in each split.

## Examples

``` r
set.seed(123)
block_initial <- spatial_initial_split(boston_canopy,
  prop = 1 / 5, spatial_block_cv
)
testing(block_initial)
#> Simple feature collection with 153 features and 18 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 745098 ymin: 2915630 xmax: 805045.8 ymax: 2969840
#> Projected CRS: NAD83 / Massachusetts Mainland (ftUS)
#> # A tibble: 153 × 19
#>    grid_id land_area canopy_gain canopy_loss canopy_no_change canopy_area_2014
#>    <chr>       <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
#>  1 M-9      2690727.      52443.      53467.          304239.          357706.
#>  2 Q-21     2690727.      54712.     101816.         1359305.         1461121.
#>  3 AB-23     725043.      13737.      13278.           52628.           65906.
#>  4 AC-15    1175032.      24517.      24010.          111148.          135158.
#>  5 U-25     2691491.      83740.     117496.          601040.          718536.
#>  6 Y-13     2691490.      79215.      41676.          312299.          353975.
#>  7 M-10     2578879.      27026.      41240.          161115.          202355.
#>  8 T-22     2691490.      80929.     140490.          573628.          714118.
#>  9 AO-16    1717547.      64863.      52390.          465563.          517953.
#> 10 X-23     2690728.      85198.     109044.          458205.          567249.
#> # ℹ 143 more rows
#> # ℹ 13 more variables: canopy_area_2019 <dbl>, change_canopy_area <dbl>,
#> #   change_canopy_percentage <dbl>, canopy_percentage_2014 <dbl>,
#> #   canopy_percentage_2019 <dbl>, change_canopy_absolute <dbl>,
#> #   mean_temp_morning <dbl>, mean_temp_evening <dbl>, mean_temp <dbl>,
#> #   mean_heat_index_morning <dbl>, mean_heat_index_evening <dbl>,
#> #   mean_heat_index <dbl>, geometry <MULTIPOLYGON [US_survey_foot]>
training(block_initial)
#> Simple feature collection with 529 features and 18 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 739826.9 ymin: 2908294 xmax: 812069.7 ymax: 2970073
#> Projected CRS: NAD83 / Massachusetts Mainland (ftUS)
#> # A tibble: 529 × 19
#>    grid_id land_area canopy_gain canopy_loss canopy_no_change canopy_area_2014
#>    <chr>       <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
#>  1 AB-4      795045.      15323.       3126.           53676.           56802.
#>  2 I-33      265813.       8849.      11795.           78677.           90472.
#>  3 AO-9      270153        6187.       1184.           26930.           28114.
#>  4 H-10     2691490.      73098.      80362.          345823.          426185.
#>  5 V-7       107890.        219.       3612.             240.            3852.
#>  6 Q-22     2648089.     122211.     154236.         1026632.         1180868.
#>  7 X-4       848558.       8275.       1760.            6872.            8632.
#>  8 P-18     2690726.     110928.     113146.          915137.         1028283.
#>  9 J-29     2574479.      38069.      15530.         2388638.         2404168.
#> 10 G-28     2641525.      87024.      39246.         1202528.         1241774.
#> # ℹ 519 more rows
#> # ℹ 13 more variables: canopy_area_2019 <dbl>, change_canopy_area <dbl>,
#> #   change_canopy_percentage <dbl>, canopy_percentage_2014 <dbl>,
#> #   canopy_percentage_2019 <dbl>, change_canopy_absolute <dbl>,
#> #   mean_temp_morning <dbl>, mean_temp_evening <dbl>, mean_temp <dbl>,
#> #   mean_heat_index_morning <dbl>, mean_heat_index_evening <dbl>,
#> #   mean_heat_index <dbl>, geometry <MULTIPOLYGON [US_survey_foot]>
```
