test_that("bf_analyze works with basic numeric vector", {

  set.seed(42)
  x <- runif(100, 1, 10)

  result <- bf_analyze(x)

  # Check output structure
  expect_s3_class(result, "bf_analysis")
  expect_true("data" %in% names(result))
  expect_true("results" %in% names(result))

  # Check data summary
  expect_equal(result$data$n_valid, 100)
  expect_false(result$data$needs_sign)
  expect_true(result$data$data_range["min"] > 0)
  expect_true(result$data$data_range["max"] < 11)

  # Check results table
  expect_s3_class(result$results, "data.frame")
  expect_true(nrow(result$results) > 0)
  expect_true(all(c("exp", "sig", "total", "underflow", "overflow", "changed",
                    "min_res", "max_res", "rmse", "max_err") %in% names(result$results)))

  # Check pareto indices exist and are valid
  expect_true(length(result$data$pareto) > 0)
  expect_true(all(result$data$pareto <= nrow(result$results)))
})

test_that("bf_analyze respects range parameter", {

  set.seed(42)
  x <- runif(100, 5, 10)

  # Without range - uses data range
  result1 <- bf_analyze(x)

  # With expanded range
  result2 <- bf_analyze(x, range = c(0, 100))

  # Target range should differ

  expect_equal(result1$data$target_range["min"], result1$data$data_range["min"])
  expect_true(result2$data$target_range["max"] > result1$data$target_range["max"])
})

test_that("bf_analyze respects decimals parameter", {

  set.seed(42)
  x <- runif(100, 1, 10)

  result <- bf_analyze(x, decimals = 1)

  expect_equal(result$data$decimals, 1)
})

test_that("bf_analyze respects max_bits parameter", {

  set.seed(42)
  x <- runif(100, 1, 10)

  result8 <- bf_analyze(x, max_bits = 8)
  result16 <- bf_analyze(x, max_bits = 16)

  # max_bits=8 should have fewer combinations
  expect_true(max(result8$results$total) <= 8)
  expect_true(max(result16$results$total) <= 16)
  expect_true(nrow(result8$results) < nrow(result16$results))
})

test_that("bf_analyze handles negative values (sign bit)", {

  set.seed(42)
  x <- runif(100, -10, 10)

  result <- bf_analyze(x)

  expect_true(result$data$needs_sign)
  # Total bits should account for sign bit
  expect_true(min(result$results$total) >= 3)  # at least sign + exp + sig
})

test_that("bf_analyze handles NAs in input", {

  set.seed(42)
  x <- c(runif(90, 1, 10), rep(NA, 10))

  result <- bf_analyze(x)

  expect_equal(result$data$n, 100)  # total including NAs

  expect_equal(result$data$n_valid, 90)  # valid only
})

test_that("bf_analyze fields parameter - paired combinations", {

  set.seed(42)
  x <- runif(100, 1, 50)

  result <- bf_analyze(x, fields = list(exponent = c(2, 4), significand = c(5, 3)))

  # Should have exactly 2 rows
  expect_equal(nrow(result$results), 2)

  # Check the combinations
  expect_true(all(result$results$exp %in% c(2, 4)))
  expect_true(all(result$results$sig %in% c(3, 5)))

  # Verify pairing (exp=2 with sig=5, exp=4 with sig=3)
  row1 <- result$results[result$results$exp == 2, ]
  row2 <- result$results[result$results$exp == 4, ]
  expect_equal(row1$sig, 5)
  expect_equal(row2$sig, 3)

  # fields_mode should be TRUE
  expect_true(result$data$fields_mode)
})

test_that("bf_analyze fields parameter - exponent only", {

  set.seed(42)
  x <- runif(100, 1, 50)

  result <- bf_analyze(x, max_bits = 10, fields = list(exponent = 4))

  # All rows should have exp=4
  expect_true(all(result$results$exp == 4))

  # Should have multiple sig values
  expect_true(length(unique(result$results$sig)) > 1)
})

test_that("bf_analyze fields parameter - significand only", {

  set.seed(42)
  x <- runif(100, 1, 50)

  result <- bf_analyze(x, max_bits = 10, fields = list(significand = 5))

  # All rows should have sig=5
  expect_true(all(result$results$sig == 5))

  # Should have multiple exp values
  expect_true(length(unique(result$results$exp)) > 1)
})

test_that("bf_analyze fields parameter - multiple exponents", {

  set.seed(42)
  x <- runif(100, 1, 50)

  result <- bf_analyze(x, max_bits = 12, fields = list(exponent = c(3, 4)))

  # All rows should have exp in {3, 4}
  expect_true(all(result$results$exp %in% c(3, 4)))

  # Should have rows for both exponent values
  expect_true(3 %in% result$results$exp)
  expect_true(4 %in% result$results$exp)
})

test_that("bf_analyze fields validation - mismatched lengths", {

  set.seed(42)
  x <- runif(100, 1, 10)

  # Different length vectors should error
  expect_error(
    bf_analyze(x, fields = list(exponent = c(2, 3, 4), significand = c(5, 6))),
    "same length"
  )
})

test_that("bf_analyze fields validation - invalid values", {

  set.seed(42)
  x <- runif(100, 1, 10)

  # Non-positive exponent
  expect_error(
    bf_analyze(x, fields = list(exponent = 0)),
    "positive integers"
  )

  # Non-positive significand
  expect_error(
    bf_analyze(x, fields = list(significand = -1)),
    "positive integers"
  )
})

test_that("bf_analyze min_res reflects actual representable range", {

  set.seed(42)
  x <- runif(100, 0.05, 10)  # some values below 0.125

  result <- bf_analyze(x, fields = list(exponent = 3, significand = 5))

  # With exp=3, bias=3, min representable = 2^(-3) = 0.125
  # Data goes down to ~0.05, so there's underflow
  expect_true(result$results$underflow[1] > 0)

  # min_res should be based on effective min (0.125), not data min (0.05)
  # 2^floor(log2(0.125)) / 2^5 = 0.125 / 32 = 0.00390625
  expected_min_res <- 2^floor(log2(0.125)) / 2^5
  expect_equal(result$results$min_res[1], expected_min_res)
})

test_that("bf_analyze works with SpatRaster input", {

  # Create test raster
  r <- terra::rast(nrows = 10, ncols = 10, vals = runif(100, 1, 10))

  result <- bf_analyze(r)

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$n_valid, 100)
})

test_that("bf_analyze errors on multi-layer SpatRaster", {

  # Create multi-layer raster
  r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 2, vals = runif(200, 1, 10))

  expect_error(bf_analyze(r), "single layer")
})

test_that("bf_analyze errors on all-NA input", {

  x <- rep(NA_real_, 10)

  expect_error(bf_analyze(x), "No non-NA values")
})

test_that("bf_analyze handles all-zero input as integer", {

  x <- rep(0, 10)

  # all-zeros numeric is now detected as integer (needs 1 bit)
  result <- bf_analyze(x)
  expect_equal(result$data$type, "int")
  expect_equal(result$data$bits_required, 1L)
})

test_that("bf_analyze print method works", {

  set.seed(42)
  x <- runif(100, 1, 10)

  result <- bf_analyze(x)

  # Should print without error
  expect_output(print(result), "Observations")
  expect_output(print(result), "Range")
  expect_output(print(result), "Exp")
  expect_output(print(result), "Usage")
})

test_that("bf_analyze Pareto front is correct", {

  set.seed(42)
  x <- runif(1000, 0.1, 100)

  result <- bf_analyze(x, max_bits = 12)

  # Get Pareto front rows
  pareto <- result$results[result$data$pareto, ]

  # Each row should be unique in total bits
  expect_equal(length(unique(pareto$total)), nrow(pareto))

  # For each bit count, the Pareto row should have minimum issues
  for (i in seq_len(nrow(pareto))) {
    bit_count <- pareto$total[i]
    all_at_bitcount <- result$results[result$results$total == bit_count, ]
    pareto_issues <- pareto$underflow[i] + pareto$overflow[i] + pareto$changed[i]
    all_issues <- all_at_bitcount$underflow + all_at_bitcount$overflow + all_at_bitcount$changed
    expect_true(pareto_issues <= min(all_issues) + 1e-10)  # tolerance for floating point
  }
})

test_that("bf_analyze range with zero uses data minimum", {

  set.seed(42)
  x <- runif(100, 0.5, 10)

  result <- bf_analyze(x, range = c(0, 20))

  # Target min should be data min (not 0)
  expect_equal(result$data$target_range["min"], result$data$data_range["min"])
  # Target max should be the specified 20
  expect_equal(unname(result$data$target_range["max"]), 20)
})

# ==============================================================================
# Type detection tests
# ==============================================================================

test_that("bf_analyze detects logical/boolean type", {

  result <- bf_analyze(c(TRUE, FALSE, TRUE, NA))

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$type, "bool")
  expect_equal(result$data$n_true, 2)
  expect_equal(result$data$n_false, 1)
  expect_equal(result$data$n_na, 1)

})

test_that("bf_analyze detects character type as enum", {

  result <- bf_analyze(c("apple", "banana", "cherry", "apple"))

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$type, "enum")
  expect_equal(result$data$n_levels, 3)

})

test_that("bf_analyze detects factor type as enum", {

  result <- bf_analyze(factor(c("low", "medium", "high", "low")))

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$type, "enum")
  expect_equal(result$data$n_levels, 3)

})

test_that("bf_analyze detects integer type", {

  result <- bf_analyze(as.integer(c(0, 5, 10, 100)))

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$type, "int")
  expect_equal(result$data$min, 0)
  expect_equal(result$data$max, 100)

})

test_that("bf_analyze handles raster with factor levels as enum", {

  r <- terra::rast(nrows = 3, ncols = 3, vals = c(0, 1, 2, 0, 1, 2, 0, 1, 2))
  levels(r) <- data.frame(id = 0:2, label = c("low", "medium", "high"))

  result <- bf_analyze(r)

  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$type, "enum")

})

# ==============================================================================
# Print method for all types
# ==============================================================================

test_that("bf_analyze print method works for boolean", {

  result <- bf_analyze(c(TRUE, FALSE, TRUE, NA))

  expect_output(print(result), "Boolean Analysis")
  expect_output(print(result), "TRUE")
  expect_output(print(result), "FALSE")
  expect_output(print(result), "Usage")

})

test_that("bf_analyze print method works for integer", {

  result <- bf_analyze(as.integer(c(0, 5, 10, NA, 100)))

  expect_output(print(result), "Integer Analysis")
  expect_output(print(result), "Sign required")
  expect_output(print(result), "Usage")

})

test_that("bf_analyze print method works for enum", {

  result <- bf_analyze(c("apple", "banana", "cherry", "apple"))

  expect_output(print(result), "Category/Enum Analysis")
  expect_output(print(result), "Levels")
  expect_output(print(result), "apple")
  expect_output(print(result), "Usage")

})

test_that("bf_analyze print method works for float with target range", {

  set.seed(42)
  x <- runif(100, 1, 10)
  result <- bf_analyze(x, range = c(0, 20), decimals = 2)

  expect_output(print(result), "Float Analysis")
  expect_output(print(result), "Target range")
  expect_output(print(result), "Decimals")

})

test_that("bf_analyze plot runs without error", {

  set.seed(42)
  x <- runif(100, 1, 10)

  # plot=TRUE should not error
  pdf(NULL)  # suppress graphical output
  on.exit(dev.off())
  result <- bf_analyze(x, plot = TRUE)
  expect_s3_class(result, "bf_analysis")

})

test_that("bf_analyze plot with fields mode runs without error", {

  set.seed(42)
  x <- runif(100, 1, 10)

  pdf(NULL)
  on.exit(dev.off())
  result <- bf_analyze(x, fields = list(exponent = c(3, 4, 5), significand = c(5, 6, 7)), plot = TRUE)
  expect_s3_class(result, "bf_analysis")

})
