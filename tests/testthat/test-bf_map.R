test_that("test encoding type 'bool'", {


})

test_that("test encoding type 'enum'", {


})

test_that("test encoding type 'int'", {


})

test_that("test encoding type 'float'", {


})

test_that("test protocol 'case'", {


})


test_that("bf_map writes correct registry", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "testBF")

  # check flags
  expect_true("na_y" %in% names(reg@flags))
  expect_equal(reg@flags$na_y$wasGeneratedBy$assignPosition, c(1, 1))
  expect_equal(reg@flags$na_y$wasGeneratedBy$encodeAsBinary$sign, 0)
  expect_equal(reg@flags$na_y$wasGeneratedBy$encodeAsBinary$exponent, 0)
  expect_equal(reg@flags$na_y$wasGeneratedBy$encodeAsBinary$significand, 1)
  expect_equal(reg@flags$na_y$wasGeneratedBy$encodeAsBinary$bias, 0)

  # test other metadata
  expect_true(all(c("comment", "wasDerivedFrom", "wasGeneratedBy", "wasAssociatedWith") %in% names(reg@flags$na_y)))
  expect_equal(reg@flags$na_y$comment, c("'y' contains NA-values."))

  # test updating an existing registry
  reg <- bf_map(protocol = "na", data = bf_tbl, x = year, registry = reg)
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 9L)
  expect_true("na_year" %in% names(reg@flags))
  expect_equal(reg@flags$na_year$wasGeneratedBy$assignPosition, c(2, 2))
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_map write the correct object into bf_env", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = x)

})

test_that("bf_map handles rast objects correctly", {

  reg <- bf_registry(name = "testBF", description = "test bitfield")

  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 1L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "testBF")

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
