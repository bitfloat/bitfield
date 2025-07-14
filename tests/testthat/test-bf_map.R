test_that("bf_map writes correct registry", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "testBF")

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
  reg <- bf_map(protocol = "na", data = bf_tbl, x = year, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_true("na_year" %in% names(reg@flags))
  expect_equal(reg@flags$na_year$pos, 2)
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_map write the correct object into bf_env", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = x)

  # test that the intermediate output is correct
  expect_equal(bf_env$na_x, c(F, F, F, F, F, F, F, F, F, F))

})

test_that("bf_map handles rast objects correctly", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  bf_rst <- rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # run protocol
  reg <- bf_map(protocol = "na", data = .rast(bf_rst), registry = reg, x = lyr.1)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "testBF")

  # test that the intermediate output is correct
  expect_equal(bf_env$na_lyr.1, c(F, F, F, T, F, F, F, F, F))

})

test_that("errors", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, NA, 2))

  # run protocol
  reg <- bf_map(protocol = "integer", data = dat, registry = reg, x = col, na.val = 3)

  expect_error(bf_map(protocol = "na", data = bf_tbl, x = x, registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
