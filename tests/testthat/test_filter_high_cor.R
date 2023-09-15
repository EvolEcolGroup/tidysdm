test_that("filter correlated variables", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                      package="tidysdm"))
  set.seed(123)
  to_keep <- filter_high_cor(lacerta_thin)
  expect_true(all(c("bio02","bio19","altitude","bio15","bio08","bio09","bio03")
                  %in% to_keep))
  suppressMessages(filter_high_cor(lacerta_thin), verbose)
})
