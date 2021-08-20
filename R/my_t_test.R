#' t-test
#'
#' This function takes input data, a hypothesis test, a null hypothesis
#' value, and returns a list of the computed t statistic, the degrees
#' of freedom, the hypothesis test used, and the computed p value
#'
#' @param x Numeric vector of the input data
#' @param alternative A character representing the hypothesis
#'        test we want to compute: "two.sided", "less", or "greater
#' @param mu A number indicating the null hypothesis value of the mean
#' @keywords t-test, statistical test, inference
#'
#' @importFrom stats pt sd
#'
#' @return A list containing the test statistic, the degrees of freedom, the
#'         hypothesis test, and the computed p value
#'
#' @examples
#' x <- rnorm(100, mean = 0, sd = 1)
#' my_t.test(x, "less", 0)
#' my_t.test(x, "greater", 0)
#' my_t.test(x, "two.sided", 0)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # If hypothesis test is not recognized, throw error message
  if (alternative != "two.sided" & alternative != "less" & alternative != "greater") {
    stop("alternative parameter must be one of the following: two.sided, less, greater")
  }
  # Compute test statistic, df, and p-value
  test_statistic = (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  deg_free = length(x) - 1
  if (alternative == "two.sided") {
    p_val = 2 * pt(abs(test_statistic), df = deg_free, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val = pt(test_statistic, df = deg_free)
  } else if (alternative == "greater") {
    p_val = pt(test_statistic, df = deg_free, lower.tail = FALSE)
  }
  # return results
  summary <- list("test statistic" = test_statistic,
                  "degrees of freedom" = deg_free,
                  "alternative" = alternative,
                  "p-value" = p_val)
  return(summary)
}
