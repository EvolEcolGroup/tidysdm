# Make a confusion matrix dataframe for multiple thresholds

Create the confusion matrix for multiple thresholds, using to optimise
tss

## Usage

``` r
conf_matrix_df(presences, absences)
```

## Arguments

- presences:

  Probabilities for presences

- absences:

  probabilities for absences

## Value

A data.frame of thresholds with columns *thres*, *tp*, *fp*, *fn*, and
*tn*
