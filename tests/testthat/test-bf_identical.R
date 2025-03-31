test_that("bf_identical writes correct registry", {

  # run operator
  reg <- bf_identical(bf_tbl, test = "y")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)

  # check flags
  expect_true("nan_y" %in% names(reg@flags))
  expect_equal(reg@flags$nan_y$pos, 1)
  expect_equal(reg@flags$nan_y$encoding$sign, 0)
  expect_equal(reg@flags$nan_y$encoding$exponent, 0)
  expect_equal(reg@flags$nan_y$encoding$mantissa, 1)
  expect_equal(reg@flags$nan_y$encoding$bias, 0)

  # test updating an existing registry
  reg <- bf_identical(bf_tbl, test = "x", registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("nan_x" %in% names(reg@flags))
  expect_equal(reg@flags$nan_x$pos, 2)
  expect_true(length(reg@flags) == 2L)

})
