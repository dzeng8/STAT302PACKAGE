#' lm
#'
#' This function takes a formula and input data and
#' fits a linear regression model of the data to the formula,
#' providing summary statistics.
#'
#' @param formula formula to fit the input data to
#' @param data data frame

# my_lm is a function which takes a formula and input data and
# fits a linear regression model of the data to the formula,
# providing summary statistics.
# Input: a formula indicating the linear model, and data
#        representing the input data
# Output: A table of the parameters and their estimated values,
#         Standard errors, t values, and p values
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
