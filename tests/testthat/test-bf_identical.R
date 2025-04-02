test_that("bf_identical writes correct registry", {

  # run operator
  reg <- bf_identical(bf_tbl, test = "y", against = "x", na.val = FALSE)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("identical_y_x" %in% names(reg@flags))
  expect_equal(reg@flags$identical_y_x$pos, 1)
  expect_equal(reg@flags$identical_y_x$encoding$sign, 0)
  expect_equal(reg@flags$identical_y_x$encoding$exponent, 0)
  expect_equal(reg@flags$identical_y_x$encoding$mantissa, 1)
  expect_equal(reg@flags$identical_y_x$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$identical_y_x$provenance)))
  expect_equal(unlist(reg@flags$identical_y_x$provenance, use.names = F), c("y, x", "testValue: y==x", "substituteValue: NA->FALSE", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$identical_y_x$description, c("{FALSE} the value in column 'y' is distinct from the value in column 'x'.", "{TRUE}  the value in column 'y' is identical to the value in column 'x'."))

  # test updating an existing registry
  reg <- bf_identical(bf_tbl, test = "y", against = "year", na.val = FALSE, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("identical_y_year" %in% names(reg@flags))
  expect_equal(reg@flags$identical_y_year$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_identical(bf_tbl, test = "y", against = "x", na.val = FALSE)

  # test that the intermediate output is correct
  expect_equal(bf_env$identical_y_x, c(F, F, F, F, F, F, F, F, T, F))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- c(rast(matrix(1:9, 3, 3)), rast(matrix(9:1, 3, 3)))
  names(bf_rst) <- c("lyr.1", "lyr.2")
  bf_rst[3] <- NA

  # run operator
  reg <- bf_identical(.rast(bf_rst), test = "lyr.1", against = "lyr.2", na.val = FALSE)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$identical_lyr.1_lyr.2, mat = FALSE), c(F, F, F, F, T, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(x = c(1, NA, 2),
                    y = c(9, 8, 7))

  # run operator
  reg <- bf_identical(dat, test = "y", against = "x", na.val = FALSE)

  expect_error(bf_identical(bf_tbl, test = "y", against = "x", na.val = FALSE, registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})

