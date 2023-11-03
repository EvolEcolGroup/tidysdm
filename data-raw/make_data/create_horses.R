horses <- read.csv("./data-raw/data_files/example_horses.csv")
horses <- horses %>% dplyr::select(-c(X, id, site))
horses <- tibble::as_tibble(horses)
usethis::use_data(horses, overwrite = TRUE)
