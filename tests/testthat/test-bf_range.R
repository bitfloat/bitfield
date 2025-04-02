test_that("bf_range writes correct registry", {

  # run operator
  reg <- bf_range(bf_tbl, test = "x", min = 5, max = 20)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("range_x" %in% names(reg@flags))
  expect_equal(reg@flags$range_x$pos, 1)
  expect_equal(reg@flags$range_x$encoding$sign, 0)
  expect_equal(reg@flags$range_x$encoding$exponent, 0)
  expect_equal(reg@flags$range_x$encoding$mantissa, 1)
  expect_equal(reg@flags$range_x$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$range_x$provenance)))
  expect_equal(unlist(reg@flags$range_x$provenance, use.names = F), c("x", "testValue: 5<=x<=20", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$range_x$description, c("{FALSE} the value in column 'x' is outside the range [5,20].", "{TRUE}  the value in column 'x' ranges between [5,20]."))

  # test updating an existing registry
  reg <- bf_range(bf_tbl, test = "yield", min = 10, max = NULL, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("range_yield" %in% names(reg@flags))
  expect_equal(reg@flags$range_yield$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_range(bf_tbl, test = "x", min = 5, max = 20)

  # test that the intermediate output is correct
  expect_equal(bf_env$range_x, c(F, F, F, F, F, F, F, F, F, F))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(matrix(1:9, 3, 3))

  # run operator
  reg <- bf_range(.rast(bf_rst), test = "lyr.1", min = 5, max = 9)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$range_lyr.1, mat = FALSE), c(F, F, T, F, T, T, F, T, T))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, 2, 3))

  # run operator
  reg <- bf_range(dat, test = "col", min = 5, max = 20)

  expect_error(bf_range(bf_tbl, test = "x", min = 5, max = 20, registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
