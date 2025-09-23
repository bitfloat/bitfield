test_that("bf_decode basic roundtrip works", {

  # Create registry, encode, then decode
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # Encode
  field <- bf_encode(registry = reg)

  # Decode
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
  expect_true(ncol(decoded) >= 1)
})

test_that("bf_decode works with multiple flags", {

  # Create registry with multiple flags
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
  expect_equal(ncol(decoded), 2) # Two flags
})

test_that("bf_decode works with specific flags", {

  # Create registry with multiple flags
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode then decode specific flag
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, flags = "na_y", verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
  expect_equal(ncol(decoded), 1) # Only one flag requested
})

test_that("bf_decode works with separator", {

  # Create registry
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode then decode with separator
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, sep = "-", verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
  expect_equal(ncol(decoded), 1) # Should be unified column with separator
  expect_true(all(grepl("-", decoded[[1]]))) # Should contain separator
})

test_that("bf_decode works with categorical data", {

  # Create registry with categorical flag
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg, x = commodity, na.val = 0)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
})

test_that("bf_decode works with numeric data", {

  # Create registry with numeric flag
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "numeric", data = bf_tbl, registry = reg, x = yield)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), nrow(bf_tbl))
})

test_that("bf_decode works with raster data", {

  # Create test raster
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # Create registry, encode, decode
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_s3_class(decoded, "data.frame")
  expect_equal(nrow(decoded), 9) # 3x3 raster = 9 cells
})

test_that("bf_decode verbose option works", {

  # Create registry
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  field <- bf_encode(registry = reg)

  # Test verbose output (should print legend)
  expect_output(bf_decode(x = field, registry = reg, verbose = TRUE), "pos")

  # Test non-verbose (should not print)
  expect_silent(bf_decode(x = field, registry = reg, verbose = FALSE))
})

test_that("bf_decode input validation", {

  # Create valid registry and field for error testing
  reg <- bf_registry(name = "testBF", description = "test bitfield")
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  field <- bf_encode(registry = reg)

  # Test with invalid field input
  expect_error(bf_decode(x = "not_a_dataframe", registry = reg),
               "Assertion on 'x' failed")

  # Test with invalid registry input
  expect_error(bf_decode(x = field, registry = "not_a_registry"),
               "Assertion on 'registry' failed")

  # Test with invalid flags
  expect_error(bf_decode(x = field, registry = reg, flags = "nonexistent_flag"),
               "Assertion on 'flags' failed")

  # Test with invalid separator
  expect_error(bf_decode(x = field, registry = reg, sep = c("a", "b")),
               "Assertion on 'sep' failed")
})
