#test sample_time_uncertainty with basic dataset
test_df1 <- read.csv("data-raw/olduvai_basic_trial.csv")
test_df1$date_oldest <- - test_df1$date_oldest
test_df1$date_youngest <- - test_df1$date_youngest
sample_time_uncertainty(data = test_df1, unif_cols = c('date_oldest', 'date_youngest'),
                        lubridate_fun = pastclim::ybp2date,
                        older_col = 'id_older_than', younger_col = 'id_younger_than',
                        group_col = 'site', sample_col = 'id'
                        )

#test sample_time_uncertainty with realistic, but simplified, dataset
test_df2 <- read.csv("data-raw/hadar_simplified_trial.csv")
test_df2$date_oldest <- - test_df2$date_oldest
test_df2$date_youngest <- - test_df2$date_youngest
sample_time_uncertainty(data = test_df2, unif_cols = c('date_oldest', 'date_youngest'),
                        lubridate_fun = pastclim::ybp2date,
                        older_col = 'id_older_than', younger_col = 'id_younger_than',
                        group_col = 'site_name', sample_col = 'id'
                        )
