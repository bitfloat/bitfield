# Tests for 1registry.R - S4 class definition, validity, and show method

test_that("registry S4 class has all required slots", {

  reg <- bf_registry(name = "slot_test",
                     description = "Testing slots",
                     template = bf_tbl)

  expect_true(.hasSlot(reg, "name"))
  expect_true(.hasSlot(reg, "version"))
  expect_true(.hasSlot(reg, "md5"))
  expect_true(.hasSlot(reg, "description"))
  expect_true(.hasSlot(reg, "template"))
  expect_true(.hasSlot(reg, "flags"))
  expect_true(.hasSlot(reg, "attribution"))

})

test_that("registry validity method accepts valid registry", {

  reg <- bf_registry(name = "valid_test",
                     description = "Testing validity",
                     template = bf_tbl)
  expect_true(validObject(reg))

})

test_that("show method works for empty registry", {

  reg <- bf_registry(name = "empty_show",
                     description = "Testing show",
                     template = bf_tbl)

  expect_output(show(reg), "empty registry")

})

test_that("show method works for populated registry", {

  reg <- bf_registry(name = "show_test",
                     description = "Testing show",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
                x = yield, min = 5, max = 11)

  # should print type, width, flags count, and flag details
  expect_output(show(reg), "type")
  expect_output(show(reg), "width")
  expect_output(show(reg), "flags")

})

test_that("show method displays barcode for multi-flag registry", {

  reg <- bf_registry(name = "barcode_test",
                     description = "Testing barcode",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = yield)

  output <- capture.output(show(reg))
  # should contain the barcode separator "|"
  barcode_line <- output[grepl("flags", output)]
  expect_true(length(barcode_line) > 0)

})
