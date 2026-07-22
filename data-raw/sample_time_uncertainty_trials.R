#test sample_time_uncertainty with basic dataset
test_df <- read.csv("data-raw/olduvai_basic_trial.csv")
test_df$date_oldest <- - test_df$date_oldest
test_df$date_youngest <- - test_df$date_youngest
sample_time_uncertainty(data = test_df, unif_cols = c('date_oldest', 'date_youngest'),
                        lubridate_fun = pastclim::ybp2date,
                        older_col = 'id_older_than', younger_col = 'id_younger_than',
                        group_col = 'site', sample_col = 'id'
                        )

#test sample_time_uncertainty with realistic, but simplified, dataset
test_df <- read.csv("data-raw/hadar_simplified_trial.csv")
test_df$date_oldest <- - test_df$date_oldest
test_df$date_youngest <- - test_df$date_youngest
sample_time_uncertainty(data = test_df, unif_cols = c('date_oldest', 'date_youngest'),
                        lubridate_fun = pastclim::ybp2date,
                        older_col = 'id_older_than', younger_col = 'id_younger_than',
                        group_col = 'site_name', sample_col = 'id'
                        )
#GIVES ERROR: Some younger_id values do not exist within their group: Hadar::A.L.
#doesn't say where the error is, all IDs are separated by ';'