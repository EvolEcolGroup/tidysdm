# Simple ensemble

A simple ensemble is a collection of workflows for which predictions
will be combined in a simple way (e.g. by taking either the mean or
median). Usually these workflows will consists each of the best version
of a given model algorithm following tuning based on a chosen metric
(note that the metric is defined when tuning the workflows, it can not
be changed at this stage). The workflows are fitted to the full training
dataset before making predictions.

## Usage

``` r
simple_ensemble(...)
```

## Arguments

- ...:

  not used, this function just creates an empty `simple_ensemble`
  object. Members are added with `add_best_candidates()`

## Value

an empty `simple_ensemble`. This is a tibble with columns:

- `wflow_id`: the name of the workflows for which the best model was
  chosen

- `workflow`: the trained workflow objects

- `metrics`: metrics based on the crossvalidation resampling used to
  tune the models
