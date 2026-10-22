# these tests are time consuming, so skip them on CRAN
skip_on_cran()

test_that("filter collinear variables with cor_caret", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
    package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin)
  expect_true(all(c(
    "bio02", "bio19", "altitude", "bio14", "bio08",
    "bio09", "bio03"
  )
  %in% vars_to_keep))
  expect_true(all(is.numeric(filter_collinear(lacerta_thin, names = FALSE))))
  # indices should be consistent with the names
  set.seed(123)
  vars_to_keep_ids <- filter_collinear(lacerta_thin, names = FALSE)
  var_names <- lacerta_thin %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    names()
  expect_identical(var_names[vars_to_keep_ids], as.vector(vars_to_keep))
  expect_identical(
    var_names[attr(vars_to_keep_ids, "to_remove")],
    attr(vars_to_keep, "to_remove")
  )
  verbose_test <- suppressMessages(filter_collinear(lacerta_thin,
    verbose = TRUE
  ))
  expect_error(
    filter_collinear(lacerta_thin, to_keep = "blah"),
    "to_keep includes variables that are not present in x"
  )
  # also if only some of the variables are missing
  for (this_method in c("cor_caret", "vif_step", "vif_cor")) {
    expect_error(
      filter_collinear(lacerta_thin,
        to_keep = c("bio01", "blah"),
        method = this_method
      ),
      "to_keep includes variables that are not present in x"
    )
  }
  # error if forced variables are too correlated
  expect_error(
    filter_collinear(lacerta_thin, to_keep = c("bio01", "bio06")),
    "some variables in"
  )
  # also if they are negatively correlated
  lacerta_neg <- lacerta_thin %>% dplyr::mutate(bio06 = -bio06)
  expect_error(
    filter_collinear(lacerta_neg, to_keep = c("bio01", "bio06")),
    "some variables in"
  )
  # keep variables in if they are reasonable
  set.seed(123)
  vars_kept <- filter_collinear(lacerta_thin, to_keep = c("bio01", "bio09"))
  expect_true(
    all(
      c("bio01", "bio09") %in%
        vars_kept
    )
  )
  vars_kept_cor <- cor(lacerta_thin[, vars_kept] %>% sf::st_drop_geometry())
  diag(vars_kept_cor) <- NA
  expect_true(max(abs(vars_kept_cor), na.rm = TRUE) < 0.7)
  # keep variables in if they are reasonable
  expect_true(
    all(
      c("bio01") %in%
        filter_collinear(lacerta_thin, to_keep = c("bio01"))
    )
  )
  # one dimensional dataset
  lacerta_1var <- lacerta_thin[, 1]
  expect_error(
    filter_collinear(lacerta_1var),
    "at least 2 numeric variables are needed"
  )
  # error for default object
  expect_error(
    filter_collinear("blah"),
    "no method available for this object type"
  )

  # sample from data.frame
  set.seed(123)
  expect_true(!identical(
    filter_collinear(lacerta_thin, max_cells = 100),
    vars_to_keep
  ))


  # test method on SpatRaster
  climate_present <- terra::readRDS(
    system.file("extdata/lacerta_climate_present_10m.rds",
      package = "tidysdm"
    )
  )
  cor_spatraster_ken <- filter_collinear(climate_present, cor_type = "kendall")
  cor_spatraster_ken_sub <- filter_collinear(climate_present,
    max_cells = 200,
    cor_type = "kendall"
  )
  expect_true(!identical(cor_spatraster_ken, cor_spatraster_ken_sub))

  # test method on stars
  climate_present_stars <- stars::st_as_stars(climate_present,
    as_attributes = TRUE
  )
  cor_spatraster_ken <- filter_collinear(climate_present_stars,
    cor_type = "kendall"
  )
  cor_spatraster_ken_sub <- filter_collinear(climate_present_stars,
    max_cells = 200,
    cor_type = "kendall"
  )
  expect_true(!identical(cor_spatraster_ken, cor_spatraster_ken_sub))
})


test_that("filter collinear variables with vif_step", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
    package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin, method = "vif_step")
  # we should remove two variables
  expect_true(all(!c("bio01", "bio18") %in% vars_to_keep))
  # now keep them in
  expect_true(
    all(
      c("bio01", "bio18") %in%
        filter_collinear(lacerta_thin,
          method = "vif_step",
          to_keep = c("bio01", "bio18")
        )
    )
  )
})

test_that("filter collinear variables with vif_cor", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
    package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin, method = "vif_cor")
  # we should remove two variables
  expect_true(all(!c("bio01", "bio18") %in% vars_to_keep))
  # now keep them in
  expect_true(
    all(
      c("bio01", "bio18") %in%
        filter_collinear(lacerta_thin,
          method = "vif_cor",
          to_keep = c("bio01", "bio18")
        )
    )
  )
})

test_that("filter_collinear copes with only one variable left to test", {
  set.seed(123)
  a <- stats::rnorm(100)
  x <- cbind(
    a = a,
    b = a + stats::rnorm(100, sd = 0.1),
    c = stats::rnorm(100)
  )
  # two highly collinear variables, one of which gets removed
  for (this_method in c("cor_caret", "vif_step", "vif_cor")) {
    # verbose output is printed, so we capture it
    suppressMessages(utils::capture.output(
      vars_kept <- filter_collinear(x[, c("a", "b")],
        method = this_method,
        verbose = TRUE
      )
    ))
    expect_length(vars_kept, 1)
    expect_length(attr(vars_kept, "to_remove"), 1)
  }
  # keeping one variable, removing the variable correlated with it leaves
  # a single variable to test
  for (this_method in c("cor_caret", "vif_step", "vif_cor")) {
    vars_kept <- filter_collinear(x, method = this_method, to_keep = "a")
    expect_identical(sort(as.vector(vars_kept)), c("a", "c"))
  }
  vars_kept <- filter_collinear(x[, c("a", "c")], to_keep = "a")
  expect_identical(sort(as.vector(vars_kept)), c("a", "c"))
  # keeping all variables
  for (this_method in c("cor_caret", "vif_step", "vif_cor")) {
    vars_kept <- filter_collinear(x[, c("a", "c")],
      method = this_method,
      to_keep = c("a", "c")
    )
    expect_identical(sort(as.vector(vars_kept)), c("a", "c"))
  }
})
