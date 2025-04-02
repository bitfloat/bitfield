test_that("bf_length writes correct registry", {

  # run operator
  reg <- bf_length(bf_tbl, test = "y")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 3L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("length_y" %in% names(reg@flags))
  expect_equal(reg@flags$length_y$pos, c(1, 2, 3))
  expect_equal(reg@flags$length_y$encoding$sign, 0)
  expect_equal(reg@flags$length_y$encoding$exponent, 0)
  expect_equal(reg@flags$length_y$encoding$mantissa, 3)
  expect_equal(reg@flags$length_y$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$length_y$provenance)))
  expect_equal(unlist(reg@flags$length_y$provenance, use.names = F), c("y", "encodeAsBinary: 0.0.3/0"))
  expect_equal(reg@flags$length_y$description, c("the bits encode the value length in column 'y'."))

  # test updating an existing registry
  reg <- bf_length(bf_tbl, test = "x", registry = reg)
  expect_equal(reg@width, 6L)
  expect_equal(reg@length, 10L)
  expect_true("length_x" %in% names(reg@flags))
  expect_equal(reg@flags$length_x$pos, c(4, 5, 6))
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_length(bf_tbl, test = "y")

  # test that the intermediate output is correct
  expect_equal(bf_env$length_y, c(4, 4, 4, 4, 0, 4, 4, 2, 1, 0))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(matrix(1:9, 3, 3))
  bf_rst[5] <- NA

  # run operator
  reg <- bf_length(.rast(bf_rst), test = "lyr.1")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$length_lyr.1, mat = FALSE), c(1, 1, 1, 1, 0, 1, 1, 1, 1))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run operator
  reg <- bf_length(dat, test = "col")

  expect_error(bf_length(bf_tbl, test = "y", registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})

