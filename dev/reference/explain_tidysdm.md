# Create explainer from your tidysdm ensembles.

DALEX is designed to explore and explain the behaviour of Machine
Learning methods. This function creates a DALEX explainer (see
[`DALEX::explain()`](https://modeloriented.github.io/DALEX/reference/explain.html)),
which can then be queried by multiple functions from the DALEX package
to create explanations of the model.

## Usage

``` r
explain_tidysdm(
  model,
  data,
  y,
  predict_function,
  predict_function_target_column,
  residual_function,
  ...,
  label,
  verbose,
  precalculate,
  colorize,
  model_info,
  type,
  by_workflow
)

# Default S3 method
explain_tidysdm(
  model,
  data = NULL,
  y = NULL,
  predict_function = NULL,
  predict_function_target_column = NULL,
  residual_function = NULL,
  ...,
  label = NULL,
  verbose = TRUE,
  precalculate = TRUE,
  colorize = !isTRUE(getOption("knitr.in.progress")),
  model_info = NULL,
  type = "classification",
  by_workflow = FALSE
)

# S3 method for class 'simple_ensemble'
explain_tidysdm(
  model,
  data = NULL,
  y = NULL,
  predict_function = NULL,
  predict_function_target_column = NULL,
  residual_function = NULL,
  ...,
  label = NULL,
  verbose = TRUE,
  precalculate = TRUE,
  colorize = !isTRUE(getOption("knitr.in.progress")),
  model_info = NULL,
  type = "classification",
  by_workflow = FALSE
)

# S3 method for class 'repeat_ensemble'
explain_tidysdm(
  model,
  data = NULL,
  y = NULL,
  predict_function = NULL,
  predict_function_target_column = NULL,
  residual_function = NULL,
  ...,
  label = NULL,
  verbose = TRUE,
  precalculate = TRUE,
  colorize = !isTRUE(getOption("knitr.in.progress")),
  model_info = NULL,
  type = "classification",
  by_workflow = FALSE
)
```

## Arguments

- model:

  object - a model to be explained

- data:

  data.frame or matrix - data which will be used to calculate the
  explanations. If not provided, then it will be extracted from the
  model. Data should be passed without a target column (this shall be
  provided as the `y` argument). NOTE: If the target variable is present
  in the `data`, some of the functionalities may not work properly.

- y:

  numeric vector with outputs/scores. If provided, then it shall have
  the same size as `data`

- predict_function:

  function that takes two arguments: model and new data and returns a
  numeric vector with predictions. By default it is `yhat`.

- predict_function_target_column:

  Character or numeric containing either column name or column number in
  the model prediction object of the class that should be considered as
  positive (i.e. the class that is associated with probability 1). If
  NULL, the first column of the output will be taken for binary
  classification.

- residual_function:

  function that takes four arguments: model, data, target vector y and
  predict function (optionally). It should return a numeric vector with
  model residuals for given data. If not provided, response residuals
  (\\y-\hat{y}\\) are calculated. By default it is
  `residual_function_default`.

- ...:

  other parameters

- label:

  character - the name of the model. By default it's extracted from the
  'class' attribute of the model

- verbose:

  logical. If TRUE (default) then diagnostic messages will be printed

- precalculate:

  logical. If TRUE (default) then `predicted_values` and `residual` are
  calculated when explainer is created. This will happen also if
  `verbose` is TRUE. Set both `verbose` and `precalculate` to FALSE to
  omit calculations.

- colorize:

  logical. If TRUE (default) then `WARNINGS`, `ERRORS` and `NOTES` are
  colorized. Will work only in the R console. Now by default it is
  `FALSE` while knitting and `TRUE` otherwise.

- model_info:

  a named list (`package`, `version`, `type`) containing information
  about model. If `NULL`, `DALEX` will seek for information on its own.

- type:

  type of a model, only `classification` is supported.

- by_workflow:

  boolean determining whether a list of explainer, one per model, should
  be returned instead of a single explainer for the ensemble

## Value

explainer object
[`DALEX::explain`](https://modeloriented.github.io/DALEX/reference/explain.html)
ready to work with DALEX

## Details

By default, the response variable is extracted form the ensemble object.
Note that, if the response variable is passed directly, `y` should be a
factor with presence as a reference level. To check that `y` is
formatted correctly, use
[`check_sdm_presence()`](https://evolecolgroup.github.io/tidysdm/dev/reference/check_sdm_presence.md).
For this reason, `predict_function_target_column` uses by default the
first column of the prediction output, which is assumed to be the
presence class.

## Examples

``` r
# \donttest{
# using the whole ensemble
lacerta_explainer <- explain_tidysdm(tidysdm::lacerta_ensemble)
#> Preparation of a new explainer is initiated
#>   -> model label       :  data.frame  (  default  )
#>   -> data              :  448  rows  4  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  448  values 
#>   -> predict function  :  predict_function 
#>   -> predicted values  :  Predict function column set to:  1 (  OK  )
#>   -> model_info        :  package tidysdm , ver. 1.0.4.9006 , task classification (  default  ) 
#>   -> model_info        :  type set to  classification 
#>   -> predicted values  :  numerical, min =  0.02117606 , mean =  0.2977721 , max =  0.8709933  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  0.1290067 , mean =  1.452228 , max =  1.978824  
#>   A new explainer has been created!  
# by workflow
explainer_list <- explain_tidysdm(tidysdm::lacerta_ensemble,
  by_workflow = TRUE
)
#> Warning: Unknown or uninitialised column: `pre`.
#> Preparation of a new explainer is initiated
#>   -> model label       :  default_glm 
#>   -> data              :  448  rows  4  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  448  values 
#>   -> predict function  :  yhat.workflow  will be used (  default  )
#>   -> predicted values  :  Predict function column set to:  1 (  OK  )
#>   -> model_info        :  package tidymodels , ver. 1.5.0 , task classification (  default  ) 
#>   -> model_info        :  type set to  classification 
#>   -> predicted values  :  numerical, min =  0.01618118 , mean =  0.25 , max =  0.7445644  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  0.3032477 , mean =  1.5 , max =  1.983819  
#>   A new explainer has been created!  
#> Warning: Unknown or uninitialised column: `pre`.
#> Preparation of a new explainer is initiated
#>   -> model label       :  default_rf 
#>   -> data              :  448  rows  4  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  448  values 
#>   -> predict function  :  yhat.workflow  will be used (  default  )
#>   -> predicted values  :  Predict function column set to:  1 (  OK  )
#>   -> model_info        :  package tidymodels , ver. 1.5.0 , task classification (  default  ) 
#>   -> model_info        :  type set to  classification 
#>   -> predicted values  :  numerical, min =  0 , mean =  0.2506163 , max =  0.9258611  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  0.07413889 , mean =  1.499384 , max =  2  
#>   A new explainer has been created!  
#> Warning: Unknown or uninitialised column: `pre`.
#> Preparation of a new explainer is initiated
#>   -> model label       :  default_gbm 
#>   -> data              :  448  rows  4  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  448  values 
#>   -> predict function  :  yhat.workflow  will be used (  default  )
#>   -> predicted values  :  Predict function column set to:  1 (  OK  )
#>   -> model_info        :  package tidymodels , ver. 1.5.0 , task classification (  default  ) 
#>   -> model_info        :  type set to  classification 
#>   -> predicted values  :  numerical, min =  0.0002722652 , mean =  0.2500301 , max =  0.9969552  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  0.003044844 , mean =  1.49997 , max =  1.999728  
#>   A new explainer has been created!  
#> Warning: Unknown or uninitialised column: `pre`.
#> Preparation of a new explainer is initiated
#>   -> model label       :  default_maxent 
#>   -> data              :  448  rows  4  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  448  values 
#>   -> predict function  :  yhat.workflow  will be used (  default  )
#>   -> predicted values  :  Predict function column set to:  1 (  OK  )
#>   -> model_info        :  package tidymodels , ver. 1.5.0 , task classification (  default  ) 
#>   -> model_info        :  type set to  classification 
#>   -> predicted values  :  numerical, min =  0.06587215 , mean =  0.4404422 , max =  0.9522016  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  0.07163309 , mean =  1.309558 , max =  1.934128  
#>   A new explainer has been created!  
# }
```
