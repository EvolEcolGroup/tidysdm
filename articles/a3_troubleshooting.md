# Troubleshooting models that fail

In this vignette, we illustrate how to troubleshoot tuning errors. This
is not a comprehensive list (yet), but rather an attempt to illustrate
how an error can be approached.

## NAs in the data

Several algorithms do not allow NAs. We can generate a problematic
dataset by loading the *Lacerta* dataset, and manually add an NA:

``` r
library(tidysdm)
#> Loading required package: tidymodels
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.4.1 ──
#> ✔ broom        1.0.11     ✔ recipes      1.3.1 
#> ✔ dials        1.4.2      ✔ rsample      1.3.1 
#> ✔ dplyr        1.1.4      ✔ tailor       0.1.0 
#> ✔ ggplot2      4.0.1      ✔ tidyr        1.3.1 
#> ✔ infer        1.0.9      ✔ tune         2.0.1 
#> ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
#> ✔ parsnip      1.4.0      ✔ workflowsets 1.1.1 
#> ✔ purrr        1.2.0      ✔ yardstick    1.3.2
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
#> Loading required package: spatialsample
lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
  package = "tidysdm"
))

lacerta_thin$bio05[37] <- NA
```

Let us set up a recipe and fit workflow_set

``` r
lacerta_rec <- recipe(lacerta_thin, formula = class ~ .) %>%
  step_rm(all_of(c(
    "bio01", "bio02", "bio03", "bio04", "bio07", "bio08",
    "bio09", "bio10", "bio11", "bio12", "bio14", "bio16",
    "bio17", "bio18", "bio19", "altitude"
  )))

lacerta_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = lacerta_rec),
    models = list(
      # the standard glm specs
      glm = sdm_spec_glm(),
      # maxent specs with tuning
      maxent = sdm_spec_maxent()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
```

``` r
set.seed(100)
lacerta_cv <- spatial_block_cv(lacerta_thin, v = 5)
lacerta_models <-
  lacerta_models %>%
  workflow_map("tune_grid",
    resamples = lacerta_cv, grid = 3,
    metrics = sdm_metric_set(), verbose = TRUE
  )
#> Warning: Using `all_of()` outside of a selecting function was deprecated in tidyselect
#> 1.2.0.
#> ℹ See details at
#>   <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 2 resampling: default_glm
#> ✔ 1 of 2 resampling: default_glm (576ms)
#> i 2 of 2 tuning:     default_maxent
#> → A | error:   NA values in data table. Please remove them and rerun.
#> There were issues with some computations   A: x1
#> → B | error:   Assigned data `.ind` must be compatible with existing data.
#>                ✖ Existing data has 87 rows.
#>                ✖ Assigned data has 88 rows.
#>                ℹ Only vectors of size 1 are recycled.
#>                Caused by error in `vectbl_recycle_rhs_rows()`:
#>                ! Can't recycle input of size 88 to size 87.
#> There were issues with some computations   A: x1There were issues with some computations   A: x21   B: x6
#> Warning: All models failed. Run `show_notes(.Last.tune.result)` for more
#> information.
#> There were issues with some computations   A: x24   B: x6
#> Warning: Unknown or uninitialised column: `.notes`.
#> ✖ 2 of 2 tuning:     default_maxent failed with 
```

We can see that the error for maxent is self-explanatory. Also, note
that the error impacts all data splits (technically, `rset` objects):
error A is repeated 12 times (4 splits for 3 hyperparameter values; one
split would have the NA in the testing set, which does not cause an
error in this case).

Prepping the recipe (which trains it on the dataset) can help diagnosing
problems:

``` r
lacerta_prep <- lacerta_rec %>% prep(lacerta_thin)
#> Warning: The `strings_as_factors` argument of `prep.recipe()` is deprecated as of
#> recipes 1.3.0.
#> ℹ Please use the `strings_as_factors` argument of `recipe()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
lacerta_prep
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:    1
#> predictor: 20
#> coords:     2
#> 
#> ── Training information
#> Training data contained 448 data points and 1 incomplete row.
#> 
#> ── Operations
#> • Variables removed: bio01, bio02, bio03, bio04, bio07, bio08, ... | Trained
```

Note that, in the training information, we were warned that 1 row was
incomplete. You could use `step_naomit` to deal with this
programmatically, or ascertain why you are generating missing data (we
prefer the latter, as a good SDM pipeline should not generate
observations, presences or pseudoabsences, with missing data).

## Recipes and the response variable

The response variable is treated in a special way in `recipes`, and this
can lead to problems. It is best not to manipulate (e.g. transform
character into factor) the response variable in a recipe, since that
response variable will only be available when we train and test models,
but not when we make projections. If we hard-coded a step in a recipe
that includes the response variable, the model will fit, but then it
will fail when we start making predictions.

Another potential mistake is to remove the response variable when
selecting variables of interest. This can happen if we use `step_select`
to choose variables of interest (**Note** that `step_select()` is now
deprecated in `recipes`; so it is best avoided), and the error is less
than clear:

Let’s load the data and create a recipe with `step_select`:

``` r
lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
  package = "tidysdm"
))
suggested_vars <- c("bio05", "bio06", "bio13", "bio14", "bio15")
lacerta_rec_sel <- recipe(lacerta_thin, formula = class ~ .) %>%
  step_select(all_of(suggested_vars))
#> Warning: `step_select()` was deprecated in recipes 1.3.0.
#> ℹ See `?select_select()` for recommended alternatives.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

Now we create the workflow set and fit it:

``` r
lacerta_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = lacerta_rec_sel),
    models = list(
      # the standard glm specs
      glm = sdm_spec_glm(),
      # rf specs with tuning
      rf = sdm_spec_rf()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())

set.seed(100)
lacerta_cv <- spatial_block_cv(lacerta_thin, v = 5)
lacerta_models <-
  lacerta_models %>%
  workflow_map("tune_grid",
    resamples = lacerta_cv, grid = 3,
    metrics = sdm_metric_set(), verbose = TRUE
  )
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 2 resampling: default_glm
#> → A | error:   ! `logistic_reg()` was unable to find an outcome.
#>                ℹ Ensure that you have specified an outcome column and that it hasn't been
#>                  removed in pre-processing.
#> ✖ 1 of 2 resampling: default_glm failed with: Error in `$<-`(`*tmp*`, ".predictions", value = list(NULL, NULL)) :   Assigned data `purrr::map(1:nrow(return_tbl), function(x) NULL)` must becompatible with existing data.✖ Existing data has 0 rows.✖ Assigned data has 2 rows.ℹ Only vectors of size 1 are recycled.Caused by error in `vectbl_recycle_rhs_rows()`:! Can't recycle input of size 2 to size 0.
#> i 2 of 2 tuning:     default_rf
#> i Creating pre-processing data to finalize 1 unknown parameter: "mtry"
#> → A | error:   ! `rand_forest()` was unable to find an outcome.
#>                ℹ Ensure that you have specified an outcome column and that it hasn't been
#>                  removed in pre-processing.
#> ✖ 2 of 2 tuning:     default_rf failed with: Error in `$<-`(`*tmp*`, ".predictions", value = list(NULL, NULL)) :   Assigned data `purrr::map(1:nrow(return_tbl), function(x) NULL)` must becompatible with existing data.✖ Existing data has 0 rows.✖ Assigned data has 2 rows.ℹ Only vectors of size 1 are recycled.Caused by error in `vectbl_recycle_rhs_rows()`:! Can't recycle input of size 2 to size 0.
```

The errors are not very intuitive. However, all models have failed for
all algorithms, which suggests that the problem lies with the data
preparation side (either the data themselves, or what we did with the
recipe).

Ideally, you should have already had a look at your data (with `summary`
or `glimpse`). So, in this case, we know that the data are fine. Whilst
prepping (and sometimes baking) the recipe is generally informative for
predictor variables, it is hard to diagnose problems with the outcome
variable in a recipe. Prepping will not show anything obvious:

``` r
lacerta_prep_sel <- lacerta_rec_sel %>% prep(lacerta_thin)
lacerta_prep_sel
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:    1
#> predictor: 20
#> coords:     2
#> 
#> ── Training information
#> Training data contained 448 data points and no incomplete rows.
#> 
#> ── Operations
#> • Variables selected: bio05, bio06, bio13, bio14, bio15 | Trained
```

In this case, it is a process of exclusion. Everything seems fine, but
the models don’t work. Then ask yourself if the outcome variable might
be problematic. As a general rule, we have found it easier to rely on
`step_rm` to remove variables (e.g. correlated variables highlighted by
[`tidysdm::filter_collinear()`](https://evolecolgroup.github.io/tidysdm/reference/filter_collinear.md)).

## Using the desired formula with GAM

General Additive Models have an unusual syntax, as the user has to
define which variables are fitted with splines. `tidysdm` has some
functions to simplify this process, assuming that the user just wants to
fit a standard smooth to every continuous predictor.

``` r
lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
  package = "tidysdm"
))

lacerta_rec <- recipe(lacerta_thin, formula = class ~ .) %>%
  step_rm(all_of(c(
    "bio01", "bio02", "bio03", "bio04", "bio07", "bio08",
    "bio09", "bio10", "bio11", "bio12", "bio14", "bio16",
    "bio17", "bio18", "bio19", "altitude"
  )))

lacerta_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = lacerta_rec),
    models = list(
      # the standard glm specs
      glm = sdm_spec_glm(),
      # the standard gam specs
      gam = sdm_spec_gam()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # set formula for gams
  update_workflow_model("default_gam",
    spec = sdm_spec_gam(),
    formula = gam_formula(lacerta_rec)
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
```

``` r
set.seed(100)
lacerta_cv <- spatial_block_cv(lacerta_thin, v = 5)
lacerta_models <-
  lacerta_models %>%
  workflow_map("tune_grid",
    resamples = lacerta_cv, grid = 3,
    metrics = sdm_metric_set(), verbose = TRUE
  )
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 2 resampling: default_glm
#> ✔ 1 of 2 resampling: default_glm (523ms)
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 2 of 2 resampling: default_gam
#> ✔ 2 of 2 resampling: default_gam (907ms)
```

Note that the step of defining a formula is incompatible with using
`step_cor` in a recipe. `step_cor` removes correlated variables in
recipes, using a similar algorithm to `filter_collinear` using method
`cor_caret`. However, the algorithm is fitted to each data split when
cross-validating. This means that different variables will eventually be
presented to the model when it is fitted for each split, leading to an
error as there will be a mismatch between the formula and the available
variables. This is a known issue of how GAMs are implemented in
`tidymodels`.

## When only some splits fail

In the examples above, all the splits used for cross-validation of a
given algorithms failed. However, it is also possible that failures
occur only on some splits for certain algorithms (technically, a
specific `rsplit` within certain `workflows`). When this type of problem
occurs, it is best to extract the problematic workflow, and potentially
investigate fitting it to the specific `rsplit`.

We generate a problematic dataset by subsampling the lacerta dataset:

``` r
lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
  package = "tidysdm"
))
set.seed(123)
lacerta_thin <- lacerta_thin[sample(
  seq_len(nrow(lacerta_thin)),
  nrow(lacerta_thin) / 7
), ]

lacerta_rec <- recipe(lacerta_thin, formula = class ~ .) %>%
  step_rm(all_of(c(
    "bio01", "bio02", "bio03", "bio04", "bio07", "bio08",
    "bio09", "bio10", "bio11", "bio12", "bio14", "bio16",
    "bio17", "bio18", "bio19", "altitude"
  )))

lacerta_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = lacerta_rec),
    models = list(
      # the standard glm specs
      glm = sdm_spec_glm(),
      # the standard gam specs
      gam = sdm_spec_gam(),
      # rf specs with tuning
      rf = sdm_spec_rf()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # set formula for gams
  update_workflow_model("default_gam",
    spec = sdm_spec_gam(),
    formula = gam_formula(lacerta_rec)
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
```

We then create 3 folds and attempt to fit the models:

``` r
set.seed(100)
lacerta_cv <- spatial_block_cv(lacerta_thin, v = 3)
lacerta_models <-
  lacerta_models %>%
  workflow_map("tune_grid",
    resamples = lacerta_cv, grid = 3,
    metrics = sdm_metric_set(), verbose = TRUE
  )
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 3 resampling: default_glm
#> ✔ 1 of 3 resampling: default_glm (332ms)
#> i  No tuning parameters. `fit_resamples()` will be attempted
#> i 2 of 3 resampling: default_gam
#> → A | warning: Fitting terminated with step failure - check results carefully
#> There were issues with some computations   A: x1
#> There were issues with some computations   A: x1
#> 
#> ✔ 2 of 3 resampling: default_gam (1.1s)
#> i 3 of 3 tuning:     default_rf
#> i Creating pre-processing data to finalize 1 unknown parameter: "mtry"
#> ✔ 3 of 3 tuning:     default_rf (905ms)
```

We see that one of the folds gives us an error when using GAMs. The
error (“Fitting terminated with step failure - check results carefully”)
comes from the gam function in the package `mgcv`. A quick google on
StackOverflow\[<https://stats.stackexchange.com/questions/576273/gam-model-warning-message-step-failure-in-theta-estimation>\]
gives us an idea of where this error comes from.

We start by extracting the results of the gam fits:

``` r
gam_results <- extract_workflow_set_result(lacerta_models, id = "default_gam")
gam_results
#> # Resampling results
#> # 3-fold spatial block cross-validation 
#> # A tibble: 3 × 5
#>   splits          id    .metrics         .notes           .predictions     
#>   <list>          <chr> <list>           <list>           <list>           
#> 1 <split [46/18]> Fold1 <tibble [3 × 4]> <tibble [0 × 4]> <tibble [18 × 5]>
#> 2 <split [41/23]> Fold2 <tibble [3 × 4]> <tibble [0 × 4]> <tibble [23 × 5]>
#> 3 <split [41/23]> Fold3 <tibble [3 × 4]> <tibble [1 × 4]> <tibble [23 × 5]>
#> 
#> There were issues with some computations:
#> 
#>   - Warning(s) x1: Fitting terminated with step failure - check results carefully
#> 
#> Run `show_notes(.Last.tune.result)` for more information.
```

We can

We see that, in the `.notes` column, the third item is not empty (it
does not have zero rows). We can check that it indeed contains the error
that we wanted:

``` r
gam_results$.notes
#> [[1]]
#> # A tibble: 0 × 4
#> # ℹ 4 variables: location <chr>, type <chr>, note <chr>, trace <list>
#> 
#> [[2]]
#> # A tibble: 0 × 4
#> # ℹ 4 variables: location <chr>, type <chr>, note <chr>, trace <list>
#> 
#> [[3]]
#> # A tibble: 1 × 4
#>   location                    type    note                            trace     
#>   <chr>                       <chr>   <chr>                           <list>    
#> 1 preprocessor 1/1, model 1/1 warning Fitting terminated with step f… <rlng_trc>
```

We can now get the problematic data split, and extract the training
data:

``` r
problem_split <- gam_results$splits[3][[1]]
summary(training(problem_split))
#>         class             geometry      bio01            bio02       
#>  presence  :12   POINT        :41   Min.   : 7.757   Min.   : 6.786  
#>  background:29   epsg:NA      : 0   1st Qu.:13.055   1st Qu.: 8.901  
#>                  +proj=aea ...: 0   Median :14.198   Median : 9.506  
#>                                     Mean   :13.902   Mean   : 9.785  
#>                                     3rd Qu.:15.580   3rd Qu.:10.861  
#>                                     Max.   :18.111   Max.   :13.009  
#>      bio03           bio04           bio05           bio06       
#>  Min.   :36.53   Min.   :328.5   Min.   :21.65   Min.   :-3.143  
#>  1st Qu.:38.00   1st Qu.:446.1   1st Qu.:24.57   1st Qu.: 2.724  
#>  Median :39.85   Median :546.6   Median :27.66   Median : 3.597  
#>  Mean   :40.57   Mean   :522.7   Mean   :27.78   Mean   : 3.488  
#>  3rd Qu.:42.93   3rd Qu.:579.3   3rd Qu.:30.74   3rd Qu.: 5.085  
#>  Max.   :46.16   Max.   :721.1   Max.   :34.39   Max.   : 8.134  
#>      bio07           bio08            bio09           bio10      
#>  Min.   :16.33   Min.   : 2.534   Min.   :14.81   Min.   :14.95  
#>  1st Qu.:21.45   1st Qu.: 8.148   1st Qu.:18.43   1st Qu.:19.03  
#>  Median :24.27   Median : 9.423   Median :19.82   Median :20.94  
#>  Mean   :24.29   Mean   : 9.631   Mean   :20.38   Mean   :20.70  
#>  3rd Qu.:27.49   3rd Qu.:11.306   3rd Qu.:22.68   3rd Qu.:22.75  
#>  Max.   :32.79   Max.   :15.847   Max.   :25.23   Max.   :25.42  
#>      bio11            bio12            bio13            bio14       
#>  Min.   : 1.604   Min.   : 450.6   Min.   : 59.80   Min.   : 1.000  
#>  1st Qu.: 7.109   1st Qu.: 615.5   1st Qu.: 88.01   1st Qu.: 6.005  
#>  Median : 8.273   Median : 750.2   Median :109.13   Median :14.008  
#>  Mean   : 7.985   Mean   : 895.4   Mean   :126.04   Mean   :18.392  
#>  3rd Qu.: 9.433   3rd Qu.:1191.8   3rd Qu.:148.15   3rd Qu.:26.601  
#>  Max.   :11.852   Max.   :1635.6   Max.   :227.81   Max.   :51.974  
#>      bio15           bio16           bio17            bio18       
#>  Min.   :21.49   Min.   :145.1   Min.   : 15.77   Min.   : 22.52  
#>  1st Qu.:40.18   1st Qu.:254.0   1st Qu.: 38.17   1st Qu.: 42.01  
#>  Median :52.94   Median :313.2   Median : 75.24   Median : 78.96  
#>  Mean   :49.48   Mean   :352.9   Mean   : 81.17   Mean   : 90.91  
#>  3rd Qu.:57.78   3rd Qu.:417.8   3rd Qu.:121.37   3rd Qu.:146.25  
#>  Max.   :76.74   Max.   :635.7   Max.   :178.73   Max.   :189.18  
#>      bio19           altitude      
#>  Min.   : 94.69   Min.   :  30.93  
#>  1st Qu.:231.06   1st Qu.: 167.04  
#>  Median :292.31   Median : 392.57  
#>  Mean   :332.27   Mean   : 480.18  
#>  3rd Qu.:403.43   3rd Qu.: 582.54  
#>  Max.   :634.26   Max.   :1686.49
```

In this case, there is nothing too obvious that leads to the error (an
important check is to make sure that you have enough presences in a
split; too few presences will generally lead to errors; you can use
`tidysdm::check_split_balance()` to investigate split balance ).

We can now extract the workflow and refit it to the split to confirm
that we have isolated the problem:

``` r
gam_workflow <- extract_workflow(lacerta_models, id = "default_gam")
faulty_gam <- fit(gam_workflow, training(problem_split))
#> Warning in newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,
#> : Fitting terminated with step failure - check results carefully
```

The next step would be to dig deeper into the data, trying to understand
whether there are some outliers that are problematic. The specific steps
will depend on the algorithm that is giving problems. Note that some
algorithms, such as GAMs, tend to be fragile with small datasets.
