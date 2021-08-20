#' linear model
#'
#' This function takes a formula and input data and
#' fits a linear regression model of the data to the formula,
#' providing summary statistics.
#'
#' @param formula formula to fit the input data to
#' @param data input data frame
#' @keywords linear model, regression, inference, prediction
#'
#' @importFrom stats model.frame model.matrix model.response
#'
#' @return A table of the parameters of the model and their respective
#' estimated values, standard errors, t values, and p values
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extract x and y matrices
  x <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  y <- model.response(frame)
  # compute coefficients
  beta <- solve(t(x)%*%x)%*%t(x)%*%y
  # compute degrees of freedom
  df <- nrow(x) - ncol(x)
  # compute variance
  var <- 0
  for (i in 1:nrow(x)) {
    var <- var + ((y[i] - x[i,] %*% beta) ** 2 / df)
  }
  # coerce matrix value into numeric value
  var <- c(var)
  # compute various statistics
  std_err <- sqrt(diag(var * solve(t(x) %*% x)))
  t_stat <- beta / std_err
  p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  # put results in table and return results
  results <- as.table(cbind(beta, std_err, t_stat, p_val))
  colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(results)
}
