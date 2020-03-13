#' Fitting a linear model
#'
#' my_lm fits a linear model
#'
#' @param formula a formula class object, similar to lm()
#' @param data input data frame
#'
#' @return a table with rows for each coefficient and columns
#'  for the Estimate, Std. Error, t value, and Pr(>|t|)
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @import magrittr stats
#' @export

my_lm <- function(formula, data){
  X <- model.matrix(formula, data)
  my_model <- model.frame(formula, data)
  Y <- model.response(my_model)
  df <- nrow(data) - ncol(X)
  #Calculate intercept
  beta <- solve(t(X) %*% X) %*% t(X) %*%
    # calculates variance
  sigma_2 <- sum((Y - X %*% beta)^2/df)
  #Calculate standard error
  se <- diag(sqrt(sigma_2 * solve(t(X) %*% X)))
  #Calculate t value and p value
  t <- (beta - 0)/se
  p_val <- 2*pt(abs(t), df, lower=FALSE)
  # make table
  result <- cbind(beta, se, t_value, pr)
  colnames(result) <- c("Estimate", "Std.Error", "t.value", "Pr(>|t|)")
  result <-as.table(result)
  return(result)
}