test_that("bf_numeric writes correct registry", {

  # run operator
  reg <- bf_numeric(bf_tbl, source = "y", na.val = 0L)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 32L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("numeric_y" %in% names(reg@flags))
  expect_equal(reg@flags$numeric_y$pos, c(1:32))
  expect_equal(reg@flags$numeric_y$encoding$sign, 1)
  expect_equal(reg@flags$numeric_y$encoding$exponent, 8)
  expect_equal(reg@flags$numeric_y$encoding$mantissa, 23)
  expect_equal(reg@flags$numeric_y$encoding$bias, 127)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$numeric_y$provenance)))
  expect_equal(unlist(reg@flags$numeric_y$provenance, use.names = F), c("y", "substituteValue: NA->0", "encodeAsBinary: 1.8.23/127"))
  expect_equal(reg@flags$numeric_y$description, c("the bits encode the numeric value in column 'y' [1.8.23]."))

  # test updating an existing registry
  reg <- bf_numeric(bf_tbl, source = "x", registry = reg)
  expect_equal(reg@width, 64L)
  expect_equal(reg@length, 10L)
  expect_true("numeric_x" %in% names(reg@flags))
  expect_equal(reg@flags$numeric_x$pos, c(33:64))
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_numeric(bf_tbl, source = "y", na.val = 0L)

  # test that the intermediate output is correct
  expect_equal(bf_env$numeric_y, c(59.5, 58.1, 57.8, 59.2, Inf, 59.1, 58.4, 59.0, 0, 0))

})

test_that("bf_na handles rast objects correctly", {

  # bf_rst <- rast(matrix(1:9, 3, 3))
  # bf_rst[5] <- NA
  #
  # # run operator
  # reg <- bf_na(.rast(bf_rst), test = "lyr.1")
  #
  # # check registry
  # expect_s4_class(reg, "registry")
  # expect_equal(reg@width, 1L)
  # expect_equal(reg@length, 9L)
  # expect_equal(reg@name, "new_registry")
  #
  # # test that the intermediate output is correct
  # expect_equal(values(bf_env$na_lyr.1, mat = FALSE), c(F, F, F, F, T, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1.5, 2.5, 3.5))

  # run operator
  reg <- bf_numeric(dat, source = "col", na.val = 0L)

  expect_error(bf_numeric(bf_tbl, source = "y", na.val = 0L, registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
