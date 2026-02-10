test_that("bf_decode basic roundtrip works", {

  # Create registry, encode, then decode
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # Encode
  field <- bf_encode(registry = reg)

  # Decode
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure - should be a named list
  expect_type(decoded, "list")
  expect_equal(length(decoded), 1)
  expect_equal(length(decoded[[1]]), nrow(bf_tbl))
})

test_that("bf_decode works with multiple flags", {

  # Create registry with multiple flags
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_type(decoded, "list")
  expect_equal(length(decoded), 2) # Two flags
  expect_equal(length(decoded[[1]]), nrow(bf_tbl))
  expect_equal(length(decoded[[2]]), nrow(bf_tbl))
})

test_that("bf_decode works with specific flags", {

  # Create registry with multiple flags
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  # Encode then decode specific flag
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, flags = "na_y", verbose = FALSE)

  # Check output structure
  expect_type(decoded, "list")
  expect_equal(length(decoded), 1) # Only one flag requested
  expect_equal(names(decoded), "na_y")
})

test_that("bf_decode works with categorical data", {

  # Create registry with categorical flag
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg, x = commodity, na.val = 0)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_type(decoded, "list")
  expect_equal(length(decoded[[1]]), nrow(bf_tbl))
})

test_that("bf_decode works with numeric data", {

  # Create registry with numeric flag
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "numeric", data = bf_tbl, registry = reg, x = yield)

  # Encode then decode
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure
  expect_type(decoded, "list")
  expect_equal(length(decoded[[1]]), nrow(bf_tbl))
})

test_that("bf_decode works with raster data", {

  # Create test raster
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  # Create registry with raster template, encode, decode
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_rst)
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check output structure - should be a raster
  expect_s4_class(decoded, "SpatRaster")
  expect_equal(terra::ncell(decoded), 9) # 3x3 raster = 9 cells
  expect_equal(terra::nrow(decoded), 3)
  expect_equal(terra::ncol(decoded), 3)
})

test_that("bf_decode verbose option works", {

  # Create registry
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  field <- bf_encode(registry = reg)

  # Test verbose output (should print legend)
  expect_output(bf_decode(x = field, registry = reg, verbose = TRUE), "pos")

  # Test non-verbose (should not print)
  expect_silent(bf_decode(x = field, registry = reg, verbose = FALSE))
})

test_that("bf_decode envir option works", {

  # Create registry
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = year)

  field <- bf_encode(registry = reg)

  # Create test environment
  test_env <- new.env()

  # Decode into environment
  result <- bf_decode(x = field, registry = reg, envir = test_env, verbose = FALSE)

  # Should return invisible NULL

  expect_null(result)

  # Check that flags were stored in environment
  expect_true("na_y" %in% names(test_env))
  expect_true("na_year" %in% names(test_env))
  expect_true("bf_legend" %in% names(test_env))
})

test_that("bf_decode returns legend as attribute", {

  # Create registry
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # Check legend attribute
  legend <- attr(decoded, "legend")
  expect_s3_class(legend, "data.frame")
  expect_true("pos" %in% names(legend))
  expect_true("name" %in% names(legend))
})

test_that("bf_decode input validation", {

  # Create valid registry and field for error testing
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  field <- bf_encode(registry = reg)

  # Test with invalid field input (passing raster when template is data.frame)
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = 1:9)
  expect_error(bf_decode(x = bf_rst, registry = reg),
               "registry template is 'data.frame' but 'x' is a SpatRaster")

  # Test with invalid registry input
  expect_error(bf_decode(x = field, registry = "not_a_registry"),
               "Assertion on 'registry' failed")

  # Test with invalid flags
  expect_error(bf_decode(x = field, registry = reg, flags = "nonexistent_flag"),
               "Assertion on 'flags' failed")

  # Test with invalid envir
  expect_error(bf_decode(x = field, registry = reg, envir = "not_an_env"),
               "Assertion on 'envir' failed")
})

test_that("bf_decode validates raster vs dataframe mismatch", {

  # Create raster registry
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(1:9))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_rst)
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  # Encode (returns raster)
  field <- bf_encode(registry = reg)

  # Passing a data.frame when expecting raster should fail
  df_input <- data.frame(bf_int1 = as.integer(c(0, 1, 0, 1, 0, 1, 0, 1, 0)))
  expect_error(bf_decode(x = df_input, registry = reg),
               "registry template is 'SpatRaster' but 'x' is not a SpatRaster")
})
