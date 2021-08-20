# within my_rf_cv.R

test_that("check average mean squared error is a valid value)", {
  expect_true(my_rf_cv(5) >= 0)
})
