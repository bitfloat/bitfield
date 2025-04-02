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
