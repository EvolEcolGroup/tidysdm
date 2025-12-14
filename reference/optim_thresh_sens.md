# Find threshold that gives a target sensitivity

This is an internal function returns the threshold to turn probabilities
into binary classes for a given target sensitivity

## Usage

``` r
optim_thresh_sens(presences, absences, sens_target)
```

## Arguments

- presences:

  Probabilities for presences.

- absences:

  Provabilities for absences

- sens_target:

  the target sensitivity

## Value

the probability threshold for the event
