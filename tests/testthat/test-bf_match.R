test_that("bf_match writes correct registry", {

  # run operator
  reg <- bf_match(bf_tbl, test = "commodity", set = c("soybean"))

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("match_commodity" %in% names(reg@flags))
  expect_equal(reg@flags$match_commodity$pos, 1)
  expect_equal(reg@flags$match_commodity$encoding$sign, 0)
  expect_equal(reg@flags$match_commodity$encoding$exponent, 0)
  expect_equal(reg@flags$match_commodity$encoding$mantissa, 1)
  expect_equal(reg@flags$match_commodity$encoding$bias, 0)

  # test updating an existing registry
  reg <- bf_match(bf_tbl, test = "year", set = c("2021r"), registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("match_year" %in% names(reg@flags))
  expect_equal(reg@flags$match_year$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_match(bf_tbl, test = "commodity", set = c("soybean"))

  # test that the intermediate output is correct
  expect_equal(bf_env$match_commodity, c(T, F, F, F, F, F, T, F, T, F))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(nrows = 3, ncols = 3, vals = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

  # run operator
  reg <- bf_match(.rast(bf_rst), test = "lyr.1", set = c(5))

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$match_lyr.1, mat = FALSE), c(F, F, F, F, T, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run operator
  reg <- bf_match(dat, test = "col", set = c("soybean"))

  expect_error(bf_match(bf_tbl, test = "commodity", set = c("soybean"), registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
