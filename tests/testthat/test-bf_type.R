test_that("bf_type writes correct registry", {

  # run operator
  reg <- bf_type(bf_tbl, test = "x", type = "integer")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("type_x_integer" %in% names(reg@flags))
  expect_equal(reg@flags$type_x_integer$pos, 1)
  expect_equal(reg@flags$type_x_integer$encoding$sign, 0)
  expect_equal(reg@flags$type_x_integer$encoding$exponent, 0)
  expect_equal(reg@flags$type_x_integer$encoding$mantissa, 1)
  expect_equal(reg@flags$type_x_integer$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$type_x_integer$provenance)))
  expect_equal(unlist(reg@flags$type_x_integer$provenance, use.names = F), c("x", "testValue: is.integer(x)", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$type_x_integer$description, c("{FALSE} the value in column 'x' does not have type 'integer'.", "{TRUE}  the value in column 'x' has type 'integer'."))

  # test updating an existing registry
  reg <- bf_type(bf_tbl, test = "yield", type = "character", coerce = FALSE, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("type_yield_character" %in% names(reg@flags))
  expect_equal(reg@flags$type_yield_character$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_type(bf_tbl, test = "x", type = "integer")

  # test that the intermediate output is correct
  expect_equal(bf_env$type_x_integer, c(F, F, F, F, F, F, F, F, F, F))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9)))

  # run operator
  reg <- bf_type(.rast(bf_rst), test = "lyr.1", type = "integer")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$type_lyr.1_integer, mat = FALSE), c(T, T, T, T, T, T, T, T, T))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run operator
  reg <- bf_inf(dat, test = "col")

  expect_error(bf_type(bf_tbl, test = "x", type = "integer", registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})

test_that("bf_type correctly identifies integers", {

  # # Test with coerce=FALSE on integer column
  # dat <- data.frame(col = c(1L, 2L, 3L))
  # reg <- bf_type(x = dat, test = "col", type = "integer", coerce = FALSE)
  # expect_identical(object = bf_env$type_col_integer, expected = rep(TRUE, 3))
  #
  # # Test with coerce=FALSE on non-integer columns
  # dat <- data.frame(col = c(1.0, 2.0, 3.0))
  # reg <- bf_type(x = dat, test = "col", type = "integer", coerce = FALSE)
  # expect_identical(object = bf_env$type_col_integer, expected = rep(FALSE, 3))
  #
  # # Test with coerce=TRUE on whole number numeric column
  # dat <- data.frame(col = c(1.0, 2.0, 3.0))
  # reg <- bf_type(x = dat, test = "col", type = "integer", coerce = TRUE)
  # expect_identical(object = bf_env$type_col_integer, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on decimal number column
  # dat <- data.frame(col = c(1.1, 2.2, 3.0))
  # reg <- bf_type(x = dat, test = "col", type = "integer", coerce = TRUE)
  # expect_equal(bf_env$type_col_integer, c(FALSE, FALSE, TRUE))
})

test_that("bf_type correctly identifies numerics", {

  # # Test with coerce=FALSE on numeric column
  # dat <- data.frame(col = c(1.0, 2.5, 3.0))
  # reg <- bf_type(x = dat, test = "col", type = "numeric", coerce = FALSE)
  # expect_identical(object = bf_env$type_col_numeric, expected = rep(TRUE, 3))
  #
  # # Test integers are also identified as numeric
  # dat <- data.frame(col = c(1L, 2L, 3L))
  # reg <- bf_type(x = dat, test = "col", type = "numeric", coerce = FALSE)
  # expect_identical(object = bf_env$type_col_numeric, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on convertible character column
  # dat <- data.frame(col = c("1", "2", "3"))
  # reg <- bf_type(x = dat, test = "col", type = "numeric", coerce = TRUE)
  # expect_identical(object = bf_env$type_col_numeric, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on non-convertible character column
  # dat <- data.frame(col = c("a", "b", "3"))
  # reg <- bf_type(x = dat, test = "col", type = "numeric", coerce = TRUE)
  #
  # expect_equal(bf_env$type_col_numeric, c(FALSE, FALSE, TRUE))
})

test_that("bf_type correctly identifies characters", {

  # # Test with coerce=FALSE on character column
  # dat <- data.frame(col = c("1", "2", "3"))
  # reg <- bf_type(x = dat, test = "col", type = "character", coerce = FALSE)
  # expect_identical(object = bf_env$type_col_character, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on numeric column
  # dat <- data.frame(col = c(1.0, 2.5, 3.0))
  # reg <- bf_type(x = dat, test = "col", type = "character", coerce = TRUE)
  # expect_identical(object = bf_env$type_col_character, expected = rep(TRUE, 3))
  #
  # # Test with Infinity values
  # dat <- data.frame(col = c("text", Inf, "more"))
  # reg <- bf_type(x = dat, test = "col", type = "character")
  # expect_identical(object = bf_env$type_col_character, expected = rep(TRUE, 3))
})

test_that("bf_type correctly identifies logicals", {

  # # Test with coerce=FALSE on logical column
  # dat <- data.frame(col = c(TRUE, FALSE, TRUE))
  # reg <- bf_type(x = dat, test = "col", type = "logical")
  # expect_identical(object = bf_env$type_col_logical, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on binary integer column
  # dat <- data.frame(col = c(0L, 1L, 0L))
  # reg <- bf_type(x = dat, test = "col", type = "logical", coerce = TRUE)
  # expect_identical(object = bf_env$type_col_logical, expected = rep(TRUE, 3))
  #
  # # Test with coerce=TRUE on non-binary integer column
  # dat <- data.frame(col = c(0L, 1L, 2L))
  # reg <- bf_type(x = dat, test = "col", type = "logical", coerce = TRUE)
  # expect_identical(object = bf_env$type_col_logical, expected = rep(FALSE, 3))
})

test_that("bf_type correctly handles NA values", {

  # # Test with NA values and na.val provided
  # dat <- data.frame(col = c(1L, NA_integer_, 3L))
  # reg <- bf_type(x = dat, test = "col", type = "integer", coerce = FALSE, na.val = FALSE)
  # expect_equal(bf_env$type_col_integer, c(TRUE, FALSE, TRUE))
  #
  # # Test with NA values and no na.val (should error)
  # dat <- data.frame(col = c(1.0, NA_real_, 3.0))
  # expect_error(
  #   bf_type(x = dat, test = "col", type = "numeric", coerce = FALSE),
  #   "there are NA values in the bit representation, please define 'na.val'."
  # )
  #
  # # Test coercion with NA values
  # dat <- data.frame(col = c("1", NA_character_, "3"))
  # reg <- bf_type(x = dat, test = "col", type = "numeric", coerce = TRUE, na.val = 0)
  # expect_equal(bf_env$type_col, c(TRUE, FALSE, TRUE))
})
