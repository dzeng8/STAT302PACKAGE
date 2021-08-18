# within test-my_t_test.R

# create input data for tests
x <- rnorm(100, mean = 0, sd = 1)

# create correct outputs
output_less_0 <- list("test statistic" = 0.9190272,
                      "degrees of freedom" = 99,
                      "alternative" = "less",
                      "p-value" = 0.8198425)
test_that("error message gets thrown for invalid input", {
  expect_error(my_t.test(x, "twosided", 0))
  expect_error(my_t.test(x, "less than", 0 ))
  expect_error(my_t.test(x, "greater than", 0 ))
})

test_that("output is correct for given test cases", {
  expect_equal(my_t.test(x, "less", 0), output_less_0)
})
