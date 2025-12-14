# Find threshold that maximises TSS

This is an internal function returns the threshold to turn probabilities
into binary classes to maximise TSS

## Usage

``` r
optim_thresh_tss_max(presences, absences)
```

## Arguments

- presences:

  Probabilities for presences.

- absences:

  Provabilities for absences

## Value

the probability threshold for the event
