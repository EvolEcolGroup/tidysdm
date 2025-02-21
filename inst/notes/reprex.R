library(tidymodels)
test_dataset <- bivariate_train
test_dataset$Class <- as.character(test_dataset$Class)
test_dataset$another_factor <- sample(c("a", "b"),
                                      nrow(test_dataset),
                                      replace = TRUE)
test_rec <- recipe(test_dataset, formula = Class ~ .) %>%
  step_string2factor(Class) %>%
  step_string2factor(another_factor) %>%
  step_corr(all_numeric_predictors(), threshold = 0.65, method = "spearman")
test_glm_spec <- logistic_reg() %>%
  set_engine(engine = "glm") %>%
  set_mode("classification")

glm_wflow <-
  workflow() %>%
  add_recipe(test_rec) %>%
  add_model(test_glm_spec)

glm_res <-
  glm_wflow %>%
  fit(data = test_dataset)

predict(glm_res, new_data = test_dataset, type = "class")
head(test_dataset)


################################################################################
leopards_rec <- recipe(foo, formula = class ~ .) %>%
  update_role(geometry, new_role = "geometry")
step_string2factor(class) %>%
  step_corr(all_predictors(), threshold = 0.65, method = "spearman")

leopards_rec <- recipe(leopards_training) %>%
  update_role(geometry, new_role = "geometry")
step_string2factor(class) %>%
  step_corr(all_predictors(), threshold = 0.65, method = "spearman")

foo <- as_tibble(boston_canopy)
spatial_block_cv(foo, v = 3)
