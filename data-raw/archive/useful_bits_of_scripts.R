fit_best(two_class_res)->foo
# from best fit model

outcomes_model <- extract_mold(foo)$outcome %>%
 bind_cols(predict(foo,new_data=extract_mold(foo)$predictors, type="prob"))

library(probably)
thresholds <- seq(0, 1, by = 0.001)
# thresholds perfomances
threshold_perf(outcomes_model, Class, .pred_Class1, thresholds = thresholds,
               metrics=metric_set(j_index))

thresholds[
which.max(threshold_perf(outcomes_model, Class, .pred_Class1, thresholds = thresholds,
               metrics=metric_set(j_index))$.estimate)]


