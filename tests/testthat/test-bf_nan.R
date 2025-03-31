test_that("bf_nan writes correct registry", {

  # run operator
  reg <- bf_nan(bf_tbl, test = "y")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check encoding
  expect_true("nan_y" %in% names(reg@flags))
  expect_equal(reg@flags$nan_y$pos, 1)
  expect_equal(reg@flags$nan_y$encoding$sign, 0)
  expect_equal(reg@flags$nan_y$encoding$exponent, 0)
  expect_equal(reg@flags$nan_y$encoding$mantissa, 1)
  expect_equal(reg@flags$nan_y$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$nan_y$provenance)))
  expect_equal(unlist(reg@flags$nan_y$provenance, use.names = F), c("y", "testValue: is.nan(y)", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$nan_y$description, c("{FALSE} the value in column 'y' is not NAN.", "{TRUE}  the value in column 'y' is NAN."))

  # test updating an existing registry
  reg <- bf_nan(bf_tbl, test = "x", registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("nan_x" %in% names(reg@flags))
  expect_equal(reg@flags$nan_x$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_nan(bf_tbl, test = "y")

  # test that the intermediate output is correct
  expect_equal(bf_env$nan_y, c(F, F, F, F, F, F, F, F, F, T))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(nrows = 3, ncols = 3, vals = c(1, 2, 3, 4, NaN, 6, 7, 8, 9))

  # run operator
  reg <- bf_nan(.rast(bf_rst), test = "lyr.1")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$nan_lyr.1, mat = FALSE), c(F, F, F, F, T, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NaN, 2))

  # run operator
  reg <- bf_na(dat, test = "col")

  expect_error(bf_nan(bf_tbl, test = "y", registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
