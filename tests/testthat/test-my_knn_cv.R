# within my_knn_cv.R

# load data to be tested
data("my_penguins")
data <- my_penguins[c("bill_length_mm",
                   "bill_depth_mm",
                   "flipper_length_mm",
                   "body_mass_g",
                   "species")]
data <- as.data.frame(data)
# remove rows containing NA values
data_noNA <- na.omit(data)
# split data into predictors and outcome
train <- data_noNA[, 1:4]
cl <- data_noNA[, 5]
# train model
results_1nn_5cv <- my_knn_cv(train, cl, 1, 5)

test_that("average misclassification rate is a valid ", {
  expect_true(results_1nn_5cv[[2]] >= 0)
  expect_true(results_1nn_5cv[[2]] <= 1)
})
