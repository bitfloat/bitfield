test_that("registry creation works", {

  # Create an empty registry
  test_reg <- bf_registry(name = "test_registry",
                          description = "Test registry for unit tests")

  # Check registry
  expect_true(is(test_reg, "registry"))
  expect_equal(test_reg@width, 0)
  expect_equal(test_reg@length, 0)
  expect_equal(test_reg@name, "test_registry")
  expect_equal(test_reg@md5, NA_character_)
  expect_equal(test_reg@description, "Test registry for unit tests")
  expect_equal(test_reg@flags, list())

})
