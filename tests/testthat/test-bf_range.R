test_that("bf_range writes correct registry", {

  # run operator
  reg <- bf_range(bf_tbl, test = "x", min = 5, max = 20)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)

  # check flags
  expect_true("range_x" %in% names(reg@flags))
  expect_equal(reg@flags$range_x$pos, 1)
  expect_equal(reg@flags$range_x$encoding$sign, 0)
  expect_equal(reg@flags$range_x$encoding$exponent, 0)
  expect_equal(reg@flags$range_x$encoding$mantissa, 1)
  expect_equal(reg@flags$range_x$encoding$bias, 0)

  # test updating an existing registry
  reg <- bf_range(bf_tbl, test = "yield", min = 10, max = NULL, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("range_yield" %in% names(reg@flags))
  expect_equal(reg@flags$range_yield$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_range correctly identifies values within range", {

  # # Test data with numeric values
  # dat <- data.frame(col = c(1, 5, 10, 15, 20))
  #
  # # Test with explicit min and max values
  # reg <- bf_range(x = dat, test = "col", min = 5, max = 15)
  # expect_equal(bf_env$range_col, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  #
  # # Test with only min specified
  # reg <- bf_range(x = dat, test = "col", min = 10, max = NULL)
  # expect_equal(bf_env$range_col, c(FALSE, FALSE, TRUE, TRUE, TRUE))
  #
  # # Test with only max specified
  # reg <- bf_range(x = dat, test = "col", min = NULL, max = 10)
  # expect_equal(bf_env$range_col, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  #
  # # Test with both min and max NULL (should use data min/max)
  # reg <- bf_range(x = dat, test = "col", min = NULL, max = NULL)
  # expect_equal(bf_env$range_col, c(TRUE, TRUE, TRUE, TRUE, TRUE))

})

test_that("bf_range correctly handles NA values", {

  # # test with NA values and na.val provided
  # dat <- data.frame(col = c(1, 5, NA, 15, NA))
  # reg <- bf_range(x = dat, test = "col", min = 5, max = 15, na.val = FALSE)
  # expect_equal(bf_env$range_col, c(FALSE, TRUE, FALSE, TRUE, FALSE))
  #
  # # test with NA values and no na.val (should error)
  # expect_error(
  #   bf_range(x = dat, test = "col", min = 5, max = 15),
  #   "there are NA values in the bit representation, please define 'na.val'."
  # )

})

test_that("bf_range handles edge cases correctly", {

  # # test the same min and max
  # dat <- data.frame(col = c(1, 5, 10, 15, 20))
  # reg <- bf_range(x = dat, test = "col", min = 10, max = 10)
  # expect_equal(bf_env$range_col, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  #
  # # test with min > max (invalid range)
  # reg <- bf_range(x = dat, test = "col", min = 15, max = 5)
  # expect_equal(bf_env$range_col, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  #
  # # test with extreme values
  # dat <- data.frame(col = c(-1e9, 0, 1e9))
  # reg <- bf_range(x = dat, test = "col", min = -1e9, max = 1e9)
  # expect_equal(bf_env$range_col, c(TRUE, TRUE, TRUE))

})

test_that("bf_range works with different data types", {

  # # Test with integer column
  # test_data_int <- data.frame(
  #   int_col = c(1L, 5L, 10L, 15L, 20L)
  # )
  # reg1 <- bf_range(x = test_data_int, test = "int_col", min = 5, max = 15)
  # expect_equal(bf_env$range_int_col, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  #
  # # Test with character column that can be coerced to numeric
  # test_data_char <- data.frame(
  #   char_col = c("1", "5", "10", "15", "20")
  # )
  # expect_error(
  #   bf_range(x = test_data_char, test = "char_col", min = 5, max = 15),
  #   regexp = NULL  # Any error is acceptable
  # )
  #
  # # Test with date column
  # test_data_date <- data.frame(
  #   date_col = as.Date(c("2023-01-01", "2023-03-15", "2023-06-30", "2023-09-01", "2023-12-31"))
  # )
  # reg2 <- bf_range(x = test_data_date, test = "date_col",
  #                  min = as.Date("2023-03-01"),
  #                  max = as.Date("2023-10-01"))
  # expect_equal(bf_env$range_date_col, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("errors", {

  # dat <- data.frame(col = c(1, 5, 10, 15, 20))
  #
  # # test with non-existent column
  # expect_error(
  #   bf_range(x = dat, test = "non_existent_col", min = 5, max = 15),
  #   regexp = NULL  # Any error is acceptable
  # )
  #
  # # test with non-numeric min/max
  # expect_error(
  #   bf_range(x = dat, test = "col", min = "five", max = 15),
  #   regexp = NULL  # Any error is acceptable
  # )
  #
  # # test with infinite min/max
  # expect_error(
  #   bf_range(x = dat, test = "col", min = -Inf, max = Inf),
  #   regexp = NULL  # Any error is acceptable
  # )
  #
  # # test with negative position
  # expect_error(
  #   bf_range(x = dat, test = "col", min = 5, max = 15, pos = -1),
  #   regexp = NULL  # Any error is acceptable
  # )

})
