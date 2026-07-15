# Extract a simple ensemble out of a repeat ensemble

This function extracts a simple ensemble out of a repeat ensemble, by
index or by name.

## Usage

``` r
get_repeat(x, i)
```

## Arguments

- x:

  a repeat ensemble object

- i:

  index of the simple ensemble to extract, or name of the simple
  ensemble to extract.

## Value

a simple ensemble object

## Details

Note that, when using a numerical index, that index refers to the order
of the repeats for which at least one algorithm passed the appropriate
thresholds set when creating the ensemble. For example, if "rep_02" had
not good algorithms, the repeat ensemble would only contain labels
"rep_01", "rep_03", etc. So, index 2 would refer to "rep_03" in this
case. If you want to be sure to get a given repeat, it is safer to use
the name of the repeat (e.g. "rep_03") rather than the index.

## Examples

``` r
# extract the second simple ensemble out of the repeat ensemble
get_repeat(lacerta_rep_ens, i = "rep_02")
#> A simple_ensemble of models
#> 
#> Members:
#> • default_glm
#> • default_maxent
#> 
#> Available metrics:
#> • boyce_cont
#> • roc_auc
#> • tss_max
#> 
#> Metric used to tune workflows:
#> • boyce_cont
```
