# Find threshold that maximises Kappa

This is an internal function returns the threshold to turn probabilities
into binary classes to maximise kappa

## Usage

``` r
optim_thresh_kap_max(presences, absences)
```

## Arguments

- presences:

  Probabilities for presences.

- absences:

  Provabilities for absences

## Value

the probability threshold for the event
