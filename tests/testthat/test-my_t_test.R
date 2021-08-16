# within test-my_t_test.R

# create input data for tests
x <- rnorm(100, mean = 0, sd = 1)

test_that("error message gets thrown for invalid input", {
  expect_error(my_t.test(x, "twosided", 0))
  expect_error(my_t.test(x, "less than", 0 ))
  expect_error(my_t.test(x, "greater than", 0 ))
})
