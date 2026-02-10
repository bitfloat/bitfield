# Tests for bf_protocol.R

test_that("bf_protocol creates a valid bool protocol", {

  pcl <- bf_protocol(
    name = "test_na",
    description = "{x} contains NA-values{result}.",
    test = "function(x) is.na(x = x)",
    example = list(x = c(1, NA, 3)),
    type = "bool"
  )

  expect_type(pcl, "list")
  expect_equal(pcl$name, "test_na")
  expect_equal(pcl$encoding_type, "bool")
  expect_equal(pcl$bits, 1)
  expect_equal(pcl$version, "1.0.0")
  expect_true(is.function(pcl$test))

})

test_that("bf_protocol creates a valid enum protocol", {

  pcl <- bf_protocol(
    name = "test_category",
    description = "categorizes {x}.",
    test = "function(x) as.integer(factor(x)) - 1L",
    example = list(x = c("a", "b", "c", "a")),
    type = "enum"
  )

  expect_type(pcl, "list")
  expect_equal(pcl$encoding_type, "enum")
  expect_true(pcl$bits >= 2)  # 3 levels needs at least 2 bits

})

test_that("bf_protocol creates a valid int protocol", {

  pcl <- bf_protocol(
    name = "test_int",
    description = "stores integer {x}.",
    test = "function(x) as.integer(x)",
    example = list(x = c(0, 5, 10)),
    type = "int"
  )

  expect_type(pcl, "list")
  expect_equal(pcl$encoding_type, "int")
  expect_true(pcl$bits >= 4)  # max 10 needs 4 bits

})

test_that("bf_protocol respects explicit bits parameter", {

  pcl <- bf_protocol(
    name = "test_bits",
    description = "{x} test.",
    test = "function(x) is.na(x = x)",
    example = list(x = c(1, NA, 3)),
    type = "bool",
    bits = 2L
  )

  expect_equal(pcl$bits, 2L)

})

test_that("bf_protocol respects custom version", {

  pcl <- bf_protocol(
    name = "test_ver",
    description = "{x} test.",
    test = "function(x) is.na(x = x)",
    example = list(x = c(1, NA, 3)),
    type = "bool",
    version = "2.1.0"
  )

  expect_equal(pcl$version, "2.1.0")

})

test_that("bf_protocol creates reference with author", {

  auth <- person("Jane", "Smith", role = "aut")
  pcl <- bf_protocol(
    name = "test_auth",
    description = "{x} test.",
    test = "function(x) is.na(x = x)",
    example = list(x = c(1, NA, 3)),
    type = "bool",
    author = auth
  )

  expect_true(inherits(pcl$reference, "bibentry"))

})

test_that("bf_protocol validates inputs", {

  # invalid type
  expect_error(bf_protocol(
    name = "bad", description = "test", test = "function(x) x",
    example = list(x = 1), type = "invalid"
  ))

  # missing name
  expect_error(bf_protocol(
    description = "test", test = "function(x) x",
    example = list(x = 1), type = "bool"
  ))

})

test_that("bf_protocol errors when test result doesn't match type", {

  # test returns numeric but type says bool
  expect_error(bf_protocol(
    name = "mismatch",
    description = "test {x}.",
    test = "function(x) x * 2",
    example = list(x = c(1, 2, 3)),
    type = "bool"
  ))

})
