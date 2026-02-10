test_that("bf_map writes correct registry", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_equal(reg@template$length, 9L)
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
  expect_equal(reg@template$width, 2L)
  expect_equal(reg@template$length, 9L)
  expect_true("na_year" %in% names(reg@flags))
  expect_equal(reg@flags$na_year$wasGeneratedBy$assignPosition, c(2, 2))
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_map write the correct object into bf_env", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = x)

})

test_that("bf_map handles rast objects correctly", {

  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(c(1, 2, 3, NA, 5, 6, 7, 8, 9)))

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_rst)

  # run protocol
  reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = lyr.1)

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_equal(reg@template$length, 9L)
  expect_equal(reg@template$type, "SpatRaster")
  expect_equal(reg@name, "testBF")

})

test_that("errors for data length mismatch", {

  dat <- data.frame(col = c(1, NA, 2))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)

  # run protocol
  reg <- bf_map(protocol = "integer", data = dat, registry = reg, x = col, na.val = 3)

  # try to add flag with different length data
  expect_error(bf_map(protocol = "na", data = bf_tbl, x = x, registry = reg),
               regexp = "data length.*does not match registry template length")

})

test_that("errors for data type mismatch", {

  # Create registry with data.frame template
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)

  # Try to map raster data to data.frame registry
  bf_rst <- terra::rast(nrows = 3, ncols = 3, vals = as.integer(1:9))
  expect_error(bf_map(protocol = "na", data = bf_rst, x = lyr.1, registry = reg),
               regexp = "data type does not match registry template")

  # Create registry with raster template
  reg_rst <- bf_registry(name = "testBF", description = "test bitfield",
                         template = bf_rst)

  # Try to map data.frame to raster registry
  expect_error(bf_map(protocol = "na", data = bf_tbl, x = x, registry = reg_rst),
               regexp = "data type does not match registry template")

})

# ==============================================================================
# Boolean protocols
# ==============================================================================

test_that("bf_map handles nan protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "nan", data = bf_tbl, registry = reg, x = y)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("nan_y" %in% names(reg@flags))

})

test_that("bf_map handles inf protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "inf", data = bf_tbl, registry = reg, x = y)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("inf_y" %in% names(reg@flags))

})

test_that("bf_map handles identical protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "identical", data = bf_tbl, registry = reg,
                x = x, y = y, na.val = FALSE)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("identical_x-y" %in% names(reg@flags))

})

test_that("bf_map handles range protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
                x = yield, min = 5, max = 11)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("range_yield" %in% names(reg@flags))

})

test_that("bf_map handles matches protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "matches", data = bf_tbl, registry = reg,
                x = commodity, set = c("soybean", "honey"), na.val = FALSE)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("matches_commodity" %in% names(reg@flags))

})

test_that("bf_map handles grepl protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "grepl", data = bf_tbl, registry = reg,
                x = year, pattern = ".*r", na.val = FALSE)

  expect_s4_class(reg, "registry")
  expect_equal(reg@template$width, 1L)
  expect_true("grepl_year" %in% names(reg@flags))

})

# ==============================================================================
# Enumeration protocols
# ==============================================================================

test_that("bf_map handles category protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg,
                x = commodity, na.val = 0)

  expect_s4_class(reg, "registry")
  expect_true("category_commodity" %in% names(reg@flags))
  # 3 levels + "no category" → needs 2+ bits
  expect_true(reg@template$width >= 2L)

})

test_that("bf_map handles case protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "case", data = bf_tbl, registry = reg, na.val = 4,
                yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize")

  expect_s4_class(reg, "registry")
  # case protocol creates flag named "case<iter>_<inputNames>"
  caseFlags <- grep("^case", names(reg@flags), value = TRUE)
  expect_true(length(caseFlags) >= 1)

})

# ==============================================================================
# Integer protocols
# ==============================================================================

test_that("bf_map handles nChar protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "nChar", data = bf_tbl, registry = reg,
                x = commodity, na.val = 0)

  expect_s4_class(reg, "registry")
  expect_true("nChar_commodity" %in% names(reg@flags))

})

test_that("bf_map handles nInt protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "nInt", data = bf_tbl, registry = reg, x = yield)

  expect_s4_class(reg, "registry")
  expect_true("nInt_yield" %in% names(reg@flags))

})

test_that("bf_map handles nDec protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "nDec", data = bf_tbl, registry = reg, x = yield)

  expect_s4_class(reg, "registry")
  expect_true("nDec_yield" %in% names(reg@flags))

})

test_that("bf_map handles integer protocol", {

  dat <- data.frame(col = as.integer(c(1, 5, 10, NA, 3)))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = col, na.val = 0L)

  expect_s4_class(reg, "registry")
  expect_true("integer_col" %in% names(reg@flags))
  # max value 10 needs 4 bits
  expect_true(reg@template$width >= 4L)

})

# ==============================================================================
# Float protocol
# ==============================================================================

test_that("bf_map handles numeric protocol", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "numeric", data = bf_tbl, registry = reg,
                x = yield, decimals = 2)

  expect_s4_class(reg, "registry")
  expect_true("numeric_yield" %in% names(reg@flags))
  # float encoding needs sign + exponent + significand
  expect_true(reg@template$width >= 3L)

})

# ==============================================================================
# Validation
# ==============================================================================

test_that("bf_map errors on protocol with underscore", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  expect_error(bf_map(protocol = "bad_name", data = bf_tbl, registry = reg, x = x),
               regexp = "must not contain")

})

test_that("bf_map errors on bool protocol with NAs and no na.val", {

  # Create data where x has NAs, so the identical protocol (bool, not na/nan/inf) triggers the check
  dat <- data.frame(a = c(1, NA, 3), b = c(1, 2, 3))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  expect_error(bf_map(protocol = "identical", data = dat, registry = reg,
                      x = a, y = b),
               regexp = "na.val")

})

test_that("bf_map errors on enum protocol with NAs and no na.val", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  # commodity has NA → needs na.val
  expect_error(bf_map(protocol = "category", data = bf_tbl, registry = reg,
                      x = commodity),
               regexp = "na.val")

})

test_that("bf_map errors on integer protocol with non-integer data", {

  dat <- data.frame(col = c(1.5, 2.7, 3.14))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  expect_error(bf_map(protocol = "integer", data = dat, registry = reg,
                      x = col),
               regexp = "non-integer")

})

test_that("bf_map errors on integer protocol with NAs and no na.val", {

  dat <- data.frame(col = c(1L, NA, 3L))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  expect_error(bf_map(protocol = "integer", data = dat, registry = reg,
                      x = col),
               regexp = "na.val")

})

test_that("bf_map handles custom name parameter", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg,
                x = y, name = "custom_flag_name")

  expect_true("custom_flag_name" %in% names(reg@flags))

})

test_that("bf_map iterator works for repeated protocols", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
                x = yield, min = 5, max = 11)
  reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
                x = yield, min = 8, max = 14)

  rangeFlags <- grep("^range", names(reg@flags), value = TRUE)
  expect_equal(length(rangeFlags), 2L)

})

test_that("bf_map with category protocol validates factor/character input", {

  dat <- data.frame(col = c(1.0, 2.0, 3.0))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  expect_error(bf_map(protocol = "category", data = dat, registry = reg,
                      x = col, na.val = 0),
               regexp = "factor or character")

})

test_that("bf_map with case protocol validates na.val", {

  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = bf_tbl)
  # na.val=1 conflicts with case value 1
  expect_error(bf_map(protocol = "case", data = bf_tbl, registry = reg, na.val = 1,
                      yield >= 11, yield < 11),
               regexp = "conflicts")

})

# ---- auto-scaling integer protocol ----

test_that("integer protocol with range and fields does full round-trip", {

  dat <- data.frame(density = c(0.5, 1.2, 2.8, 0.0, 3.1))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = density, range = c(0, 3.1), fields = list(significand = 5),
                na.val = 0L)

  # check scale is stored in provenance
  scale <- reg@flags[[1]]$wasGeneratedBy$encodeAsBinary$scale
  expect_false(is.null(scale))
  expect_equal(scale$min, 0)
  expect_equal(scale$max, 3.1)

  # encode and decode round-trip
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # values should be within resolution (3.1/31 = 0.1) of originals
  expect_true(all(abs(decoded$`integer_density` - dat$density) <= 0.1))

})

test_that("integer protocol with fields only on numeric data uses data range", {

  dat <- data.frame(val = c(1.0, 2.5, 4.0))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = val, fields = list(significand = 4), na.val = 0L)

  scale <- reg@flags[[1]]$wasGeneratedBy$encodeAsBinary$scale
  expect_false(is.null(scale))
  expect_equal(scale$min, 1.0)
  expect_equal(scale$max, 4.0)

  # round-trip
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)
  expect_true(all(abs(decoded[[1]] - dat$val) <= 0.3))

})

test_that("integer protocol with decimals on numeric data auto-scales", {

  dat <- data.frame(pct = c(0.15, 0.50, 0.85))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = pct, range = c(0, 1), decimals = 1, na.val = 0L)

  scale <- reg@flags[[1]]$wasGeneratedBy$encodeAsBinary$scale
  expect_false(is.null(scale))
  expect_equal(scale$min, 0)
  expect_equal(scale$max, 1)

  # round-trip
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)
  expect_true(all(abs(decoded[[1]] - dat$pct) <= 0.15))

})

test_that("integer protocol still works with actual integer data", {

  dat <- data.frame(col = c(1L, 3L, 5L))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = col, na.val = 0L)

  # no scale should be stored
  scale <- reg@flags[[1]]$wasGeneratedBy$encodeAsBinary$scale
  expect_null(scale)

  # round-trip should be exact
  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)
  expect_equal(decoded[[1]], dat$col)

})

test_that("integer auto-scaling still rejects non-integer data without scaling args", {

  dat <- data.frame(col = c(1.5, 2.7, 3.14))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  expect_error(bf_map(protocol = "integer", data = dat, registry = reg,
                      x = col),
               regexp = "non-integer")

})

test_that("integer auto-scaling handles NA values", {

  dat <- data.frame(density = c(0.5, NA, 2.8))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = density, range = c(0, 3.1), fields = list(significand = 5),
                na.val = 0L)

  field <- bf_encode(registry = reg)
  decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

  # non-NA values should round-trip
  expect_true(abs(decoded[[1]][1] - 0.5) <= 0.1)
  expect_true(abs(decoded[[1]][3] - 2.8) <= 0.1)

})

test_that("integer auto-scaling quality assessment reports correct metrics", {

  dat <- data.frame(density = c(0.5, 1.2, 2.8, 0.0, 3.1))
  reg <- bf_registry(name = "testBF", description = "test bitfield",
                     template = dat)
  reg <- bf_map(protocol = "integer", data = dat, registry = reg,
                x = density, range = c(0, 3.1), fields = list(significand = 5),
                na.val = 0L)

  quality <- reg@flags[[1]]$wasGeneratedBy$assessQuality
  expect_equal(quality$type, "int")
  expect_equal(quality$underflow, 0L)
  expect_equal(quality$overflow, 0L)
  # resolution should be uniform: 3.1/31 = 0.1
  expect_equal(quality$min_resolution, quality$max_resolution)
  expect_true(abs(quality$min_resolution - 0.1) < 0.01)

})
