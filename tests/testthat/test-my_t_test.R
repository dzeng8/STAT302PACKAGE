# within test-my_t_test.R

# create input data for tests
x <- rnorm(100, mean = 0, sd = 1)

test_that("error message gets thrown for invalid input", {
  expect_error(my_t.test(x, "twosided", 0))
  expect_error(my_t.test(x, "less than", 0 ))
  expect_error(my_t.test(x, "greater than", 0 ))
})

test_that("output is correct for given test cases", {
  expect_equal(my_t.test(x, "two.sided", 0)$'p-value', t.test(x, mu = 0, alternative="two.sided")$'p.value')
  expect_equal(my_t.test(x, "less", 0)$'p-value', t.test(x, mu = 0, alternative="less")$'p.value')
  expect_equal(my_t.test(x, "greater", 0)$'p-value', t.test(x, mu = 0, alternative="greater")$'p.value')
})
