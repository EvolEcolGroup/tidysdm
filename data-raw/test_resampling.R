test_df <- read.csv("data-raw/re-sampling_trial_data_v2.csv")
test_df$date_oldest <- - test_df$date_oldest
test_df$date_youngest <- - test_df$date_youngest
sample_time_uncertainty(data = test_df, unif_cols = c('date_oldest', 'date_youngest'),
                        lubridate_fun = pastclim::ybp2date,
                        older_col = 'id_older_than', younger_col = 'id_younger_than',
                        group_col = 'site', sample_col = 'id'
                        )
