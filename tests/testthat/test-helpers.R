# Tests for internal helper functions in helpers.R

# ==============================================================================
# .toBin tests
# ==============================================================================

test_that(".toBin converts integers to binary strings", {

  # single value
  expect_equal(bitfield:::.toBin(0, len = 4), "0000")
  expect_equal(bitfield:::.toBin(1, len = 4), "0001")
  expect_equal(bitfield:::.toBin(5, len = 4), "0101")
  expect_equal(bitfield:::.toBin(15, len = 4), "1111")
  expect_equal(bitfield:::.toBin(255, len = 8), "11111111")

})

test_that(".toBin handles vectors", {

  result <- bitfield:::.toBin(c(0, 1, 2, 3), len = 2)
  expect_equal(result, c("00", "01", "10", "11"))

})

test_that(".toBin auto-computes length from max value", {

  # len=NULL should auto-detect
  result <- bitfield:::.toBin(c(0, 7))
  expect_equal(nchar(result[1]), nchar(result[2]))
  expect_equal(result[2], "111")

})

test_that(".toBin pads to equal width", {

  result <- bitfield:::.toBin(c(1, 255), len = 8)
  expect_equal(nchar(result[1]), 8)
  expect_equal(nchar(result[2]), 8)
  expect_equal(result[1], "00000001")

})

test_that(".toBin handles large values (32-bit)", {

  # 2^31 = 2147483648
  result <- bitfield:::.toBin(2^31, len = 32)
  expect_equal(nchar(result), 32)
  expect_equal(substr(result, 1, 1), "1")

})

# ==============================================================================
# .toDec tests
# ==============================================================================

test_that(".toDec converts binary strings to integers", {

  expect_equal(bitfield:::.toDec("0000"), 0L)
  expect_equal(bitfield:::.toDec("0001"), 1L)
  expect_equal(bitfield:::.toDec("0101"), 5L)
  expect_equal(bitfield:::.toDec("1111"), 15L)
  expect_equal(bitfield:::.toDec("11111111"), 255L)

})

test_that(".toDec handles vectors", {

  result <- bitfield:::.toDec(c("00", "01", "10", "11"))
  expect_equal(result, c(0L, 1L, 2L, 3L))

})

test_that(".toDec handles radix point (fractional binary)", {

  # 1.1 in binary = 1 + 0.5 = 1.5
  result <- bitfield:::.toDec("1.1")
  expect_equal(result, 1.5)

  # 1.101 in binary = 1 + 0.5 + 0.125 = 1.625
  result <- bitfield:::.toDec("1.101")
  expect_equal(result, 1.625)

})

test_that(".toBin and .toDec are inverse operations for integers", {

  # roundtrip
  for (val in c(0, 1, 42, 127, 255)) {
    bin <- bitfield:::.toBin(val, len = 8)
    dec <- bitfield:::.toDec(bin)
    expect_equal(dec, as.integer(val))
  }

})

# ==============================================================================
# .makeEncoding tests
# ==============================================================================

test_that(".makeEncoding returns correct structure for bool type", {

  enc <- bitfield:::.makeEncoding(var = c(TRUE, FALSE, TRUE), type = "bool")
  expect_type(enc, "list")
  expect_equal(enc$sign, 0L)
  expect_equal(enc$exponent, 0L)
  expect_equal(enc$significand, 1L)

})

test_that(".makeEncoding returns correct structure for enum type", {

  # .makeEncoding receives integer-coded enum values (as produced by bf_map)
  enc <- bitfield:::.makeEncoding(var = c(0L, 1L, 2L), type = "enum")
  expect_type(enc, "list")
  expect_equal(enc$sign, 0L)
  expect_equal(enc$exponent, 0L)
  # 3 values → needs 2 bits
  expect_true(enc$significand >= 2L)

})

test_that(".makeEncoding handles named format for float type", {

  enc <- bitfield:::.makeEncoding(var = c(1.0, 2.0), type = "float", format = "half")
  expect_equal(enc$sign, 1L)
  expect_equal(enc$exponent, 5L)
  expect_equal(enc$significand, 10L)

})

test_that(".makeEncoding handles custom fields for float type", {

  enc <- bitfield:::.makeEncoding(
    var = c(1.0, 2.0), type = "float",
    fields = list(sign = 0, exponent = 3, significand = 7)
  )
  expect_equal(enc$sign, 0L)
  expect_equal(enc$exponent, 3L)
  expect_equal(enc$significand, 7L)
  expect_equal(enc$bias, 3L)  # 2^(3-1) - 1 = 3

})

test_that(".makeEncoding detects sign requirement", {

  # positive only → sign=0
  enc_pos <- bitfield:::.makeEncoding(var = c(1, 2, 3), type = "int")
  expect_equal(enc_pos$sign, 0L)

  # negative values → sign=1
  enc_neg <- bitfield:::.makeEncoding(var = c(-1, 2, 3), type = "int")
  expect_equal(enc_neg$sign, 1L)

})

# ==============================================================================
# .assessEncodingQuality tests
# ==============================================================================

test_that(".assessEncodingQuality returns correct structure for bool", {

  result <- bitfield:::.assessEncodingQuality(
    values = c(TRUE, FALSE, TRUE, NA),
    type = "bool"
  )
  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$n, 4)
  expect_equal(result$data$n_na, 1)
  expect_equal(result$data$n_true, 2)
  expect_equal(result$data$n_false, 1)
  expect_equal(result$data$bits_required, 2L)  # needs NA encoding

})

test_that(".assessEncodingQuality returns correct structure for enum", {

  result <- bitfield:::.assessEncodingQuality(
    values = factor(c("a", "b", "c", "a")),
    type = "enum"
  )
  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$n_levels, 3)

})

test_that(".assessEncodingQuality returns correct structure for int", {

  result <- bitfield:::.assessEncodingQuality(
    values = c(0L, 5L, 10L, 15L),
    type = "int"
  )
  expect_s3_class(result, "bf_analysis")
  expect_equal(result$data$min, 0)
  expect_equal(result$data$max, 15)
  expect_false(result$data$needs_sign)

})

test_that(".assessEncodingQuality computes float metrics for multiple configs", {

  set.seed(42)
  vals <- runif(100, 0.1, 10)
  result <- bitfield:::.assessEncodingQuality(
    values = vals,
    type = "float",
    max_bits = 12L
  )
  expect_s3_class(result, "bf_analysis")
  expect_true("results" %in% names(result))
  expect_true(nrow(result$results) > 0)
  expect_true(all(c("exp", "sig", "total", "underflow", "overflow",
                     "changed", "min_res", "max_res", "rmse", "max_err") %in%
                     names(result$results)))

})

test_that(".assessEncodingQuality float with specific enc computes quality", {

  vals <- c(0.5, 1.0, 2.5, 5.0, 8.0)
  enc <- list(sign = 0L, exponent = 3L, significand = 7L, bias = 3L)

  result <- bitfield:::.assessEncodingQuality(
    values = vals,
    type = "float",
    enc = enc
  )
  expect_s3_class(result, "bf_analysis")
  expect_true(!is.null(result$data$underflow))
  expect_true(!is.null(result$data$overflow))
  expect_true(!is.null(result$data$rmse))
  expect_true(!is.null(result$data$max_error))

})

test_that(".assessEncodingQuality handles NA values", {

  result <- bitfield:::.assessEncodingQuality(
    values = c(1L, NA, 3L, NA, 5L),
    type = "int"
  )
  expect_equal(result$data$n, 5)
  expect_equal(result$data$n_na, 2)
  expect_equal(result$data$n_valid, 3)

})

# ==============================================================================
# .updateMD5 tests
# ==============================================================================

test_that(".updateMD5 produces a valid MD5 checksum", {

  reg <- bf_registry(name = "md5_test",
                     description = "Testing MD5",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  updated <- bitfield:::.updateMD5(reg)
  expect_true(nchar(updated@md5) == 32)  # MD5 is 32 hex chars
  expect_true(grepl("^[0-9a-f]+$", updated@md5))

})

test_that(".updateMD5 produces consistent checksums", {

  reg <- bf_registry(name = "md5_test",
                     description = "Testing MD5",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  md5_1 <- bitfield:::.updateMD5(reg)@md5
  md5_2 <- bitfield:::.updateMD5(reg)@md5
  expect_equal(md5_1, md5_2)

})

# ==============================================================================
# .getDependencies tests
# ==============================================================================

test_that(".getDependencies finds base functions only", {

  simple_fun <- function(x) is.na(x)
  deps <- bitfield:::.getDependencies(simple_fun)
  expect_null(deps)  # is.na is base, no extra deps

})

# ==============================================================================
# project() tests
# ==============================================================================

test_that("project() creates a valid project object", {

  proj <- project(
    title = "Test Project",
    type = "Dataset",
    publisher = "Test Publisher"
  )
  expect_s3_class(proj, "project")
  expect_equal(proj$title, "Test Project")
  expect_equal(proj$type, "Dataset")

})

test_that("project() with full metadata", {

  auth <- person("Jane", "Smith", email = "jane@example.com", role = "aut",
                 comment = c(ORCID = "0000-0001-2345-6789",
                             affiliation = "University of Example"))

  proj <- project(
    title = "Full Project",
    type = "Dataset",
    author = auth,
    publisher = "Example Publisher",
    identifier = "10.5281/zenodo.1234567",
    description = "A test project",
    subject = c("testing", "bitfields"),
    license = "CC-BY-4.0"
  )
  expect_s3_class(proj, "project")
  expect_equal(proj$license, "CC-BY-4.0")
  expect_equal(length(proj$subject), 2)

})

test_that("project() validates inputs", {

  expect_error(project(title = "Test", type = "InvalidType"))

})
