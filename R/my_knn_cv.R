#' knn_cv
#'
#' This function takes input data, the desired number of nearest neighbors,
#' the number of folds to be used for k-fold cross validation and performs
#' k-fold cross validation from the given input data
#'
#' @param train a numeric input data frame
#' @param cl the true class value of the training data
#' @param k_nn a numeric representing the desired number of nearest neighbors
#' @param k_cv a numeric representing the number of folds to be used for k-fold
#' cross validation
#' @keywords k-nearest-neighbors cross-validation k-fold prediction
#'
#' @return A list of two objects where the first object are the predicted outcomes
#' based on the input data and the second object is the average
#' cross-validation misclassification error rate
#'
#' @examples
#' data("my_penguins")
#' penguin_data <- my_penguins[c("bill_length_mm",
#'                   "bill_depth_mm",
#'                   "flipper_length_mm",
#'                   "body_mass_g",
#'                   "species")]
#' penguin_data <- as.data.frame(penguin_data)
#' # remove rows containing NA values
#' data_noNA <- na.omit(penguin_data)
#' # split data into predictors and outcome
#' train <- data_noNA[, 1:4]
#' cl <- data_noNA[, 5]
#' # test function with 1-nearest neighbor and 5-fold cv
#' results_1nn_5cv <- my_knn_cv(train, cl, 1, 5)
#' # test function with 5-nearest neighbor and 5-fold cv
#' results_5nn_5cv <- my_knn_cv(train, cl, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Split data in k_cv folds, randomly
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # cumulative misclassification rate
  cum_misclass_rate <- 0
  # perform k-fold cv
  for (i in (1:k_cv)) {
    # split data and then train a model
    train_split <- train[fold != i,]
    test_split <- train[fold == i,]
    train_outcome <- cl[fold != i]
    pred <- class::knn(train_split, test_split, train_outcome, k_nn)
    # compute misclassification rate and aggregate it
    test_outcome <- cl[fold == i]
    misclass_rate <- sum(pred != test_outcome) / length(pred)
    cum_misclass_rate <- cum_misclass_rate + misclass_rate
  }
  # compute average misclassification rate
  cv_err <- cum_misclass_rate / k_cv
  # train final model with training set as the test set
  class <- class::knn(train, train, cl, k_nn)
  # store results in a list and return
  results <- list()
  results[[1]] <- class
  results[[2]] <- cv_err
  return(results)
}
