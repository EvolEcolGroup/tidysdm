library(tidymodels)
two_rec <- recipe(Class~.,data=two_class_dat)

mars_spec <- parsnip::mars() %>% 
  parsnip::set_engine("earth") %>%
  parsnip::set_mode("classification")

mars_tune_spec <- parsnip::mars(num_terms=tune()) %>% 
  parsnip::set_engine("earth") %>%
  parsnip::set_mode("classification")

two_tune_wkflow <-# new workflow object
  workflow() %>% # use workflow function
  add_recipe(two_rec) %>% # add the new recipe
  add_model(mars_tune_spec)

two_cv <- vfold_cv(two_class_dat, v=3)


two_bayer_res <- tune_bayes(two_tune_wkflow,
                            resamples = two_cv, initial=8)



########################################
> two_bayer_res <- tune_bayes(two_tune_wkflow,
                              +                             resamples = two_cv, initial=8)
→ A | error:   `num_terms` should be >= 1.
! No improvement for 10 iterations; returning current results.
There were issues with some computations   A: x30
There were 20 warnings (use warnings() to see them)

########
two_rec <- recipe(Class~.,data=two_class_dat)

svm_poly_tune <- tune_spec <- parsnip::svm_poly(cost=tune()) %>% 
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("classification")

two_svm_wkflow <-# new workflow object
  workflow() %>% # use workflow function
  add_recipe(two_rec) %>% # add the new recipe
  add_model(svm_poly_tune)


two_svm_bayes_res <- tune_bayes(two_svm_wkflow,
                                resamples = two_cv, initial=8)


two_svm_grid_res <- tune_grid(two_svm_wkflow,
                            resamples = two_cv, grid=5, metrics = metric_set(sensitivity))

