# Tests for built-in bit-flag protocol checklist (bf_pcl)
# Each protocol should have correct structure and produce valid output

test_that("bf_pcl$na works correctly", {

  pcl <- bf_pcl$na
  expect_equal(pcl$name, "na")
  expect_equal(pcl$encoding_type, "bool")
  expect_equal(pcl$bits, 1)

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, NA, 3))
  expect_type(result, "logical")
  expect_equal(result, c(FALSE, TRUE, FALSE))

})

test_that("bf_pcl$nan works correctly", {

  pcl <- bf_pcl$nan
  expect_equal(pcl$name, "nan")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, NaN, 3))
  expect_type(result, "logical")
  expect_true(result[2])
  expect_false(result[1])

})

test_that("bf_pcl$inf works correctly", {

  pcl <- bf_pcl$inf
  expect_equal(pcl$name, "inf")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, Inf, -Inf, 3))
  expect_type(result, "logical")
  expect_true(result[2])
  expect_true(result[3])
  expect_false(result[1])

})

test_that("bf_pcl$identical works correctly", {

  pcl <- bf_pcl$identical
  expect_equal(pcl$name, "identical")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, 2, 1), y = c(1, 3, 1))
  expect_type(result, "logical")
  expect_true(result[1])
  expect_false(result[2])
  expect_true(result[3])

})

test_that("bf_pcl$range works correctly", {

  pcl <- bf_pcl$range
  expect_equal(pcl$name, "range")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, 5, 10, 15), min = 3, max = 12)
  expect_type(result, "logical")
  # values in range should be TRUE, out of range FALSE
  expect_false(result[1])  # 1 < 3
  expect_true(result[2])   # 5 in [3,12]
  expect_true(result[3])   # 10 in [3,12]
  expect_false(result[4])  # 15 > 12

})

test_that("bf_pcl$matches works correctly", {

  pcl <- bf_pcl$matches
  expect_equal(pcl$name, "matches")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c("apple", "banana", "apple"), set = "apple")
  expect_type(result, "logical")
  expect_true(result[1])
  expect_false(result[2])

})

test_that("bf_pcl$grepl works correctly", {

  pcl <- bf_pcl$grepl
  expect_equal(pcl$name, "grepl")
  expect_equal(pcl$encoding_type, "bool")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c("apple", "application", "banana"), pattern = "appl")
  expect_type(result, "logical")
  expect_true(result[1])
  expect_true(result[2])
  expect_false(result[3])

})

test_that("bf_pcl$category works correctly", {

  pcl <- bf_pcl$category
  expect_equal(pcl$name, "category")
  expect_equal(pcl$encoding_type, "enum")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c("a", "b", "c", "a"))
  expect_true(is.numeric(result))

})

test_that("bf_pcl$case works correctly", {

  pcl <- bf_pcl$case
  expect_equal(pcl$name, "case")
  expect_equal(pcl$encoding_type, "enum")

  test_fn <- eval(parse(text = pcl$test))
  # case protocol expects logical conditions as ...
  result <- test_fn(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, FALSE))
  expect_true(is.integer(result) || is.factor(result) || is.numeric(result))

})

test_that("bf_pcl$nChar works correctly", {

  pcl <- bf_pcl$nChar
  expect_equal(pcl$name, "nChar")
  expect_equal(pcl$encoding_type, "int")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c("a", "ab", "abc"))
  expect_true(is.numeric(result))
  expect_equal(result, c(1, 2, 3))

})

test_that("bf_pcl$nInt works correctly", {

  pcl <- bf_pcl$nInt
  expect_equal(pcl$name, "nInt")
  expect_equal(pcl$encoding_type, "int")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1.23, 45.6, 789.01))
  expect_true(is.numeric(result))

})

test_that("bf_pcl$nDec works correctly", {

  pcl <- bf_pcl$nDec
  expect_equal(pcl$name, "nDec")
  expect_equal(pcl$encoding_type, "int")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1.23, 45.6, 789))
  expect_true(is.numeric(result))

})

test_that("bf_pcl$integer works correctly", {

  pcl <- bf_pcl$integer
  expect_equal(pcl$name, "integer")
  expect_equal(pcl$encoding_type, "int")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1, 5, 10))
  expect_true(is.numeric(result))

})

test_that("bf_pcl$numeric works correctly", {

  pcl <- bf_pcl$numeric
  expect_equal(pcl$name, "numeric")
  expect_equal(pcl$encoding_type, "float")

  test_fn <- eval(parse(text = pcl$test))
  result <- test_fn(x = c(1.5, 2.7, 3.14))
  expect_true(is.numeric(result))

})
