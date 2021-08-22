#' random forest
#'
#' This function performs k-fold cross validation on a random forest model
#' generated from internal data on penguins. Model trained is
#' body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm
#' and number of trees is 100. User can specify the number of folds to be used.
#'
#' @param k A numeric indicating the number of folds
#' @keywords random-forest prediction
#'
#' @importFrom stats na.omit predict
#'
#' @return Returns average mean squared error of the random forest model across
#' all k folds from k-fold cross-validation
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {
  # load penguin data
  data("my_penguins")
  data <- my_penguins[c("bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g")]
  # remove NAs from data
  data <- na.omit(data)
  fold <- sample(rep(1:k, length = nrow(data)))
  cum_mse <- 0
  for (i in (1:k)) {
    # split data and then train a model with 100 trees
    train_split <- data[fold != i,]
    test_split <- data[fold == i,]
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = train_split,
                          ntree = 100)
    # generate predictions of body_mass_g based on test split
    predictions <- predict(model, test_split[, -4])
    true <- test_split[, 4]
    # compute mse and aggregate it
    cum_mse <- sum((true - predictions)^2) / length(predictions)
  }
  # compute mean mse and return it
  avg_mse <- cum_mse / k
  return(avg_mse)
}
utils::globalVariables("my_penguins")
