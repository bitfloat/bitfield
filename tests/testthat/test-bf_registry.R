test_that("registry creation works with data.frame template", {

  # Create a registry with data.frame template
  test_reg <- bf_registry(name = "test_registry",
                          description = "Test registry for unit tests",
                          template = bf_tbl)

  # Check registry
  expect_true(is(test_reg, "registry"))
  expect_equal(test_reg@template$width, 0L)
  expect_equal(test_reg@template$length, nrow(bf_tbl))
  expect_equal(test_reg@template$type, "data.frame")
  expect_equal(test_reg@name, "test_registry")
  expect_equal(test_reg@md5, NA_character_)
  expect_equal(test_reg@description, "Test registry for unit tests")
  expect_equal(test_reg@flags, list())

})

test_that("registry creation works with raster template", {

  # Create a raster template
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = 1:9)

  # Create a registry with raster template
  test_reg <- bf_registry(name = "raster_registry",
                          description = "Raster registry for unit tests",
                          template = bf_rst)

  # Check registry
  expect_true(is(test_reg, "registry"))
  expect_equal(test_reg@template$width, 0L)
  expect_equal(test_reg@template$length, 9L)
  expect_equal(test_reg@template$type, "SpatRaster")
  expect_equal(test_reg@template$nrows, 3L)
  expect_equal(test_reg@template$ncols, 3L)
  expect_equal(test_reg@name, "raster_registry")
  expect_equal(test_reg@flags, list())

})

test_that("registry creation fails without template", {

  # Should error when no template provided
 expect_error(bf_registry(name = "test", description = "test"),
               "template")

})
