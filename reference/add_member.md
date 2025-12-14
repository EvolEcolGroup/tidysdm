# Add best member of workflow to a simple ensemble

This function adds member(s) to a
[`simple_ensemble()`](https://evolecolgroup.github.io/tidysdm/reference/simple_ensemble.md)
object, taking the best member from each workflow provided. It is
possible to pass individual `tune_results` objects from a tuned
`workflow`, or a
[`workflowsets::workflow_set()`](https://workflowsets.tidymodels.org/reference/workflow_set.html).

## Usage

``` r
add_member(x, member, ...)

# Default S3 method
add_member(x, member, ...)

# S3 method for class 'tune_results'
add_member(x, member, metric = NULL, id = NULL, ...)

# S3 method for class 'workflow_set'
add_member(x, member, metric = NULL, ...)
```

## Arguments

- x:

  a
  [simple_ensemble](https://evolecolgroup.github.io/tidysdm/reference/simple_ensemble.md)
  to which member(s) will be added

- member:

  a `tune_results`, or a
  [`workflowsets::workflow_set`](https://workflowsets.tidymodels.org/reference/workflow_set.html)

- ...:

  not used at the moment.

- metric:

  A character string (or NULL) for which metric to optimize. If NULL,
  the first metric is used.

- id:

  the name to be given to this workflow in the `wflow_id` column.

## Value

a
[simple_ensemble](https://evolecolgroup.github.io/tidysdm/reference/simple_ensemble.md)
with additional member(s)
