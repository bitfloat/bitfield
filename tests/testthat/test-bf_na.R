test_that("bf_na writes correct registry", {

  # run operator
  reg <- bf_na(bf_tbl, test = "year")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check encoding
  expect_true("na_year" %in% names(reg@flags))
  expect_equal(reg@flags$na_year$pos, 1)
  expect_equal(reg@flags$na_year$encoding$sign, 0)
  expect_equal(reg@flags$na_year$encoding$exponent, 0)
  expect_equal(reg@flags$na_year$encoding$mantissa, 1)
  expect_equal(reg@flags$na_year$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$na_year$provenance)))
  expect_equal(unlist(reg@flags$na_year$provenance, use.names = F), c("year", "testValue: is.na(year)", "encodeAsBinary: 0.0.1/0"))
  expect_equal(reg@flags$na_year$description, c("{FALSE} the value in column 'year' is not NA.", "{TRUE}  the value in column 'year' is NA."))

  # test updating an existing registry
  reg <- bf_na(bf_tbl, test = "x", registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("na_x" %in% names(reg@flags))
  expect_equal(reg@flags$na_x$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_na(bf_tbl, test = "year")

  # test that the intermediate output is correct
  expect_equal(bf_env$na_year, c(F, T, F, F, F, F, F, F, F, F))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- rast(matrix(1:9, 3, 3))
  bf_rst[5] <- NA

  # run operator
  reg <- bf_na(.rast(bf_rst), test = "lyr.1")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$na_lyr.1, mat = FALSE), c(F, F, F, F, T, F, F, F, F))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run operator
  reg <- bf_na(dat, test = "col")

  expect_error(bf_na(bf_tbl, test = "yield", registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
