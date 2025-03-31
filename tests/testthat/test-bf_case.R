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

  # check flags
  expect_true("cases_1" %in% names(reg@flags))
  expect_equal(reg@flags$cases_1$pos, c(1, 2))
  expect_equal(reg@flags$cases_1$encoding$sign, 0)
  expect_equal(reg@flags$cases_1$encoding$exponent, 0)
  expect_equal(reg@flags$cases_1$encoding$mantissa, 2)
  expect_equal(reg@flags$cases_1$encoding$bias, 0)

  # test updating an existing registry
  reg <- bf_case(bf_tbl, commodity == "maize", commodity == "soybean", commodity == "honey", registry = reg)
  expect_equal(reg@width, 4L)
  expect_equal(reg@length, 10L)
  expect_true("cases_2" %in% names(reg@flags))
  expect_equal(reg@flags$cases_2$pos, c(3, 4))
  expect_true(length(reg@flags) == 2L)

})
