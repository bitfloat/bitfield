test_that("bf_encode works with basic NA flag", {

  # Create registry and add NA flag
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # Encode the bitfield
  field <- bf_encode(registry = reg)

  # Check output structure
  expect_s3_class(field, "data.frame")
  expect_equal(nrow(field), nrow(bf_tbl))
  expect_true(ncol(field) >= 1)
  expect_true(all(sapply(field, is.integer)))

  # Check that bitfield has correct length
  expect_equal(nrow(field), reg@length)
})

test_that("bf_encode works with multiple flags", {

  # Create registry with multiple flags
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode the bitfield
  field <- bf_encode(registry = reg)

  # Check output structure
  expect_s3_class(field, "data.frame")
  expect_equal(nrow(field), nrow(bf_tbl))
  expect_true(all(sapply(field, is.integer)))

  # Check registry width matches encoding
  expect_equal(nrow(field), reg@length)
})

test_that("bf_encode works with numeric protocol", {

  # Create registry with numeric flag
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "numeric", data = bf_tbl, registry = reg, x = yield)

  # Encode the bitfield
  field <- bf_encode(registry = reg)

  # Check output structure
  expect_s3_class(field, "data.frame")
  expect_equal(nrow(field), nrow(bf_tbl))
  expect_true(all(sapply(field, is.integer)))
})

test_that("bf_encode works with categorical protocol", {

  # Create registry with categorical flag
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg, x = commodity, na.val = 0)

  # Encode the bitfield
  field <- bf_encode(registry = reg)

  # Check output structure
  expect_s3_class(field, "data.frame")
  expect_equal(nrow(field), nrow(bf_tbl))
  expect_true(all(sapply(field, is.integer)))
})

test_that("bf_encode works with raster data", {

  # Create test raster
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # Create registry with raster
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  # Encode the bitfield
  field <- bf_encode(registry = reg)

  # Check output structure
  expect_s3_class(field, "data.frame")
  expect_equal(nrow(field), 9) # 3x3 raster = 9 cells
  expect_true(all(sapply(field, is.integer)))
})

test_that("bf_encode handles empty registry", {

  # Create empty registry
  reg <- bf_registry(name = "testBF", description = "test bitfield")

  # Should handle empty registry gracefully
  expect_error(bf_encode(registry = reg))
})

test_that("bf_encode input validation", {

  # Test with non-registry input
  expect_error(bf_encode(registry = "not_a_registry"),
               "Assertion on 'registry' failed")

  # Test with NULL input
  expect_error(bf_encode(registry = NULL),
               "Assertion on 'registry' failed")
})
