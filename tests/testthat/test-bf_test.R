test_that("bf_test writes correct registry", {

  # run operator
  reg <- bf_test(operator = "na", data = bf_tbl, x = y)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("na_y" %in% names(reg@flags))
  expect_equal(reg@flags$na_y$pos, 1)
  expect_equal(reg@flags$na_y$encoding$sign, 0)
  expect_equal(reg@flags$na_y$encoding$exponent, 0)
  expect_equal(reg@flags$na_y$encoding$mantissa, 1)
  expect_equal(reg@flags$na_y$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy", "wasAssociatedWith") %in% names(reg@flags$na_y$provenance)))
  expect_equal(unlist(reg@flags$na_y$provenance, use.names = F)[1:4], c("bf_tbl", "useTest: na_1.0.0", "withArguments: x=y", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$na_y$description, c("'y' contains NA-values."))

  # test updating an existing registry
  reg <- bf_test(operator = "na", data = bf_tbl, x = year, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("na_year" %in% names(reg@flags))
  expect_equal(reg@flags$na_year$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_test write the correct object into bf_env", {

  # run operator
  reg <- bf_test(operator = "na", data = bf_tbl, x = x)

  # test that the intermediate output is correct
  expect_equal(bf_env$na_x, c(F, F, F, F, F, F, F, F, F, F))

})

test_that("bf_test handles rast objects correctly", {

  bf_rst <- rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # run operator
  reg <- bf_test(operator = "na", data = .rast(bf_rst), x = lyr.1)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(bf_env$na_lyr.1, c(F, F, F, T, F, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run operator
  reg <- bf_test(operator = "integer", data = dat, x = col, na.val = 3)

  expect_error(bf_test(operator = "na", data = bf_tbl, x = x, registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
