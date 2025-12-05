# Package index

## Data preparation

Functions for sampling background/pseudo-absences points and thinning
the data.

- [`sample_background()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_background.md)
  : Sample background points for SDM analysis
- [`sample_background_time()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_background_time.md)
  : Sample background points for SDM analysis for points with a time
  point.
- [`sample_pseudoabs()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_pseudoabs.md)
  : Sample pseudo-absence points for SDM analysis
- [`sample_pseudoabs_time()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_pseudoabs_time.md)
  : Sample pseudo-absence points for SDM analysis for points with a time
  point.
- [`thin_by_cell()`](https://evolecolgroup.github.io/tidysdm/dev/reference/thin_by_cell.md)
  : Thin point dataset to have 1 observation per raster cell
- [`thin_by_cell_time()`](https://evolecolgroup.github.io/tidysdm/dev/reference/thin_by_cell_time.md)
  : Thin point dataset to have 1 observation per raster cell per time
  slice
- [`thin_by_dist()`](https://evolecolgroup.github.io/tidysdm/dev/reference/thin_by_dist.md)
  : Thin points dataset based on geographic distance
- [`thin_by_dist_time()`](https://evolecolgroup.github.io/tidysdm/dev/reference/thin_by_dist_time.md)
  : Thin points dataset based on geographic and temporal distance
- [`make_mask_from_presence()`](https://evolecolgroup.github.io/tidysdm/dev/reference/make_mask_from_presence.md)
  : Make a mask from presence data

## Choice of predictor variables

Functions for removing collinearity and visualising the distribution of
predictors.

- [`filter_collinear()`](https://evolecolgroup.github.io/tidysdm/dev/reference/filter_collinear.md)
  : Filter to retain only variables that have low collinearity
- [`plot_pres_vs_bg()`](https://evolecolgroup.github.io/tidysdm/dev/reference/plot_pres_vs_bg.md)
  : Plot presences vs background
- [`geom_split_violin()`](https://evolecolgroup.github.io/tidysdm/dev/reference/geom_split_violin.md)
  : Split violin geometry for ggplots
- [`dist_pres_vs_bg()`](https://evolecolgroup.github.io/tidysdm/dev/reference/dist_pres_vs_bg.md)
  : Distance between the distribution of climate values for presences vs
  background

## Recipes

Functions for recipes with spatial SDM data (additional steps can be
added with standard `recipes` functions).

- [`recipe(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/recipe.sf.md)
  [`spatial_recipe()`](https://evolecolgroup.github.io/tidysdm/dev/reference/recipe.sf.md)
  :

  Recipe for `sf` objects

- [`check_sdm_presence()`](https://evolecolgroup.github.io/tidysdm/dev/reference/check_sdm_presence.md)
  : Check that the column with presences is correctly formatted

## Models specification

Predefined model specifications (custom models can be added with
standard `parsnip` model specifications).

- [`sdm_spec_glm()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_glm.md)
  : Model specification for a GLM for SDM
- [`sdm_spec_gam()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_gam.md)
  : Model specification for a GAM for SDM
- [`gam_formula()`](https://evolecolgroup.github.io/tidysdm/dev/reference/gam_formula.md)
  : Create a formula for gam
- [`sdm_spec_rand_forest()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_rand_forest.md)
  [`sdm_spec_rf()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_rand_forest.md)
  : Model specification for a Random Forest for SDM
- [`sdm_spec_boost_tree()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_boost_tree.md)
  : Model specification for a Boosted Trees model for SDM
- [`sdm_spec_maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_maxent.md)
  : Model specification for a MaxEnt for SDM
- [`maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/maxent.md)
  : MaxEnt model
- [`regularization_multiplier()`](https://evolecolgroup.github.io/tidysdm/dev/reference/maxent_params.md)
  [`feature_classes()`](https://evolecolgroup.github.io/tidysdm/dev/reference/maxent_params.md)
  : Parameters for maxent models

## Data splitting

Functions for splitting the data into folds (in additiona to standard
`spatialsample` functions.

- [`spatial_initial_split()`](https://evolecolgroup.github.io/tidysdm/dev/reference/spatial_initial_split.md)
  : Simple Training/Test Set Splitting for spatial data

- [`blockcv2rsample()`](https://evolecolgroup.github.io/tidysdm/dev/reference/blockcv2rsample.md)
  :

  Convert an object created with `blockCV` to an `rsample` object

- [`check_splits_balance()`](https://evolecolgroup.github.io/tidysdm/dev/reference/check_splits_balance.md)
  : Check the balance of presences vs pseudoabsences among splits

- [`autoplot(`*`<spatial_initial_split>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/autoplot.spatial_initial_split.md)
  : Create a ggplot for a spatial initial rsplit.

- [`grid_offset()`](https://evolecolgroup.github.io/tidysdm/dev/reference/grid_offset.md)
  : Get default grid cellsize for a given dataset

- [`grid_cellsize()`](https://evolecolgroup.github.io/tidysdm/dev/reference/grid_cellsize.md)
  : Get default grid cellsize for a given dataset

## Metrics

Specialised metrics for SDM, and methods of metrics from `yardstick`
adapted to work on `sf` objects

- [`sdm_metric_set()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_metric_set.md)
  : Metric set for SDM

- [`optim_thresh()`](https://evolecolgroup.github.io/tidysdm/dev/reference/optim_thresh.md)
  : Find threshold that optimises a given metric

- [`boyce_cont()`](https://evolecolgroup.github.io/tidysdm/dev/reference/boyce_cont.md)
  [`boyce_cont_vec()`](https://evolecolgroup.github.io/tidysdm/dev/reference/boyce_cont.md)
  : Boyce continuous index (BCI)

- [`kap_max()`](https://evolecolgroup.github.io/tidysdm/dev/reference/kap_max.md)
  [`kap_max_vec()`](https://evolecolgroup.github.io/tidysdm/dev/reference/kap_max.md)
  : Maximum Cohen's Kappa

- [`tss_max()`](https://evolecolgroup.github.io/tidysdm/dev/reference/tss_max.md)
  [`tss_max_vec()`](https://evolecolgroup.github.io/tidysdm/dev/reference/tss_max.md)
  : Maximum TSS - True Skill Statistics

- [`tss()`](https://evolecolgroup.github.io/tidysdm/dev/reference/tss.md)
  : TSS - True Skill Statistics

- [`average_precision(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`brier_class(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`classification_cost(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`gain_capture(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`mn_log_loss(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`pr_auc(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`roc_auc(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`roc_aunp(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  [`roc_aunu(`*`<sf>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/prob_metrics_sf.md)
  :

  Probability metrics for `sf` objects

## Ensemble

Functions for creating an ensemble selecting the best set of parameters
for each model.

- [`simple_ensemble()`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
  : Simple ensemble
- [`autoplot(`*`<simple_ensemble>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/autoplot.simple_ensemble.md)
  : Plot the results of a simple ensemble
- [`repeat_ensemble()`](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
  : Repeat ensemble
- [`add_member()`](https://evolecolgroup.github.io/tidysdm/dev/reference/add_member.md)
  : Add best member of workflow to a simple ensemble
- [`add_repeat()`](https://evolecolgroup.github.io/tidysdm/dev/reference/add_repeat.md)
  : Add repeat(s) to a repeated ensemble
- [`collect_metrics(`*`<simple_ensemble>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/collect_metrics.simple_ensemble.md)
  [`collect_metrics(`*`<repeat_ensemble>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/collect_metrics.simple_ensemble.md)
  : Obtain and format results produced by tuning functions for ensemble
  objects
- [`control_ensemble_grid()`](https://evolecolgroup.github.io/tidysdm/dev/reference/control_ensemble.md)
  [`control_ensemble_resamples()`](https://evolecolgroup.github.io/tidysdm/dev/reference/control_ensemble.md)
  [`control_ensemble_bayes()`](https://evolecolgroup.github.io/tidysdm/dev/reference/control_ensemble.md)
  : Control wrappers
- [`explain_tidysdm()`](https://evolecolgroup.github.io/tidysdm/dev/reference/explain_tidysdm.md)
  : Create explainer from your tidysdm ensembles.

## Project SDM to present, past and future

Functions for making predictions.

- [`calib_class_thresh()`](https://evolecolgroup.github.io/tidysdm/dev/reference/calib_class_thresh.md)
  : Calibrate class thresholds
- [`collect_class_thresh()`](https://evolecolgroup.github.io/tidysdm/dev/reference/collect_class_thresh.md)
  : Obtain and format the class thresholds for ensemble objects
- [`predict(`*`<repeat_ensemble>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/predict.repeat_ensemble.md)
  : Predict for a repeat ensemble set
- [`predict(`*`<simple_ensemble>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/predict.simple_ensemble.md)
  : Predict for a simple ensemble set
- [`predict_raster()`](https://evolecolgroup.github.io/tidysdm/dev/reference/predict_raster.md)
  : Make predictions for a whole raster

## Managing extrapolation

Functions for managing extrapolation.

- [`clamp_predictors()`](https://evolecolgroup.github.io/tidysdm/dev/reference/clamp_predictors.md)
  : Clamp the predictors to match values in training set
- [`extrapol_mess()`](https://evolecolgroup.github.io/tidysdm/dev/reference/extrapol_mess.md)
  : Multivariate environmental similarity surfaces (MESS)

## Other

Additional helpful functions.

- [`niche_overlap()`](https://evolecolgroup.github.io/tidysdm/dev/reference/niche_overlap.md)
  : Compute overlap metrics of the two niches
- [`km2m()`](https://evolecolgroup.github.io/tidysdm/dev/reference/km2m.md)
  : Convert a geographic distance from km to m
- [`y2d()`](https://evolecolgroup.github.io/tidysdm/dev/reference/y2d.md)
  : Convert a time interval from years to days
- [`filter_high_cor()`](https://evolecolgroup.github.io/tidysdm/dev/reference/filter_high_cor.md)
  : Deprecated: Filter to retain only variables below a given
  correlation threshold
- [`pairs(`*`<stars>`*`)`](https://evolecolgroup.github.io/tidysdm/dev/reference/pairs-stars.md)
  : Pairwise matrix of scatterplot for stars objects

## Example data

Example datasets and simple models used in the documentation

- [`horses`](https://evolecolgroup.github.io/tidysdm/dev/reference/horses.md)
  : Coordinates of radiocarbon dates for horses
- [`lacerta`](https://evolecolgroup.github.io/tidysdm/dev/reference/lacerta.md)
  : Coordinates of presences for Iberian emerald lizard
- [`lacertidae_background`](https://evolecolgroup.github.io/tidysdm/dev/reference/lacertidae_background.md)
  : Coordinates of presences for lacertidae in the Iberian peninsula
- [`lacerta_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/lacerta_ensemble.md)
  : A simple ensemble for the lacerta data
- [`lacerta_rep_ens`](https://evolecolgroup.github.io/tidysdm/dev/reference/lacerta_rep_ens.md)
  : A repeat ensemble for the lacerta data
