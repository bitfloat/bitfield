test_that("bf_case writes correct registry", {

  # run operator
  reg <- bf_case(bf_tbl, exclusive = FALSE,
                 yield >= 11,
                 yield < 11 & yield > 9,
                 yield < 9 & commodity == "maize")

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 10L)
  expect_equal(reg@name, "new_registry")

  # check flags
  expect_true("cases_1" %in% names(reg@flags))
  expect_equal(reg@flags$cases_1$pos, c(1, 2))
  expect_equal(reg@flags$cases_1$encoding$sign, 0)
  expect_equal(reg@flags$cases_1$encoding$exponent, 0)
  expect_equal(reg@flags$cases_1$encoding$mantissa, 2)
  expect_equal(reg@flags$cases_1$encoding$bias, 0)

  # test other metadata
  expect_true(all(c("wasDerivedFrom", "wasGeneratedBy") %in% names(reg@flags$cases_1$provenance)))
  expect_equal(unlist(reg@flags$cases_1$provenance, use.names = F), c("yield, commodity", "encodingAsBinary: 0.0.2/0"))
  expect_equal(reg@flags$cases_1$description, c("the observation has case 1 [yield >= 11].", "the observation has case 2 [yield < 11 & yield > 9].", "the observation has case 3 [yield < 9 & commodity == \"maize\"]."))

  # test updating an existing registry
  reg <- bf_case(bf_tbl, commodity == "maize", commodity == "soybean", commodity == "honey", registry = reg)
  expect_equal(reg@width, 4L)
  expect_equal(reg@length, 10L)
  expect_true("cases_2" %in% names(reg@flags))
  expect_equal(reg@flags$cases_2$pos, c(3, 4))
  expect_true(length(reg@flags) == 2L)

})

test_that("bf_na write the correct object into bf_env", {

  # run operator
  reg <- bf_case(bf_tbl, exclusive = FALSE,
                 yield >= 11,
                 yield < 11 & yield > 9,
                 yield < 9 & commodity == "maize")

  # test that the intermediate output is correct
  expect_equal(bf_env$cases_1, c(0, 0, 0, 2, 0, 2, 0, 1, 1, 0))

})

test_that("bf_na handles rast objects correctly", {

  bf_rst <- c(rast(matrix(1:9, 3, 3)), rast(matrix(9:1, 3, 3)))
  names(bf_rst) <- c("lyr.1", "lyr.2")
  bf_rst[3] <- NA

  # run operator
  reg <- bf_case(.rast(bf_rst), lyr.1 < 5, lyr.1 > 5, is.na(lyr.2))

  # check registry
  expect_s4_class(reg, "registry")
  expect_equal(reg@width, 2L)
  expect_equal(reg@length, 9L)
  expect_equal(reg@name, "new_registry")

  # test that the intermediate output is correct
  expect_equal(values(bf_env$cases_1, mat = FALSE), c(1, 1, 3, 1, 0, 2, 1, 2, 2))

})

test_that("errors", {

  # mismatch in flags with previous registry
  dat <- data.frame(col = c(1, 2, 3))

  # run operator
  reg <- bf_case(dat, col < 5, col > 5)

  expect_error(
    reg <- bf_case(bf_tbl, exclusive = FALSE,
                   yield >= 11,
                   yield < 11 & yield > 9,
                   yield < 9 & commodity == "maize",
                   registry = reg),
               regexp = "this flag doesn't have as many items, as there are observations in the bitfield")

})
