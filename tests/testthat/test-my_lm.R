# within test-my_lm.R

# load data for test
data(mtcars)

test_that("estimated coefficients correct for given input data and formula", {
  expect_equal(my_lm(mpg ~ hp + wt, data = mtcars)[, 1],
               lm(mpg ~ hp + wt, data = mtcars)$coefficients)
})
