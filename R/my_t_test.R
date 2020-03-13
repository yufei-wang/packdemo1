#' Sample t-test Function
#'
#' This function performs a one sample t-test.
#' @param x a numeric vector of data
#' @param alternative a character string specifying the alternative hypothesis
#' @param mu a number indicating the null hypothesis value of the mean
#' @keywords inference
#'
#' @return A list with the following elements:
#'   test_stat: the numeric test statistic
#'   df: the degrees of freedom.
#'   alternative: the value of the parameter `alternative`
#'   p_val: the numeric p-value.
#'
#' @examples
#' my_t.test(1:5, 2, alt = "greater")
#'
#' @import stat
#' @export

my_t.test <- function(x, alternative, mu){
  str <- c("two.sided", "less", "greater")
  if (!(alternative %in% str)) {
    return("alternative should be \"two.sided\", \"less\", or \"greater\" only.")
  } else {

    #Calculate degree of freedom, mean, se and t value
    df <- length(x) - 1
    se <- sd(x) / sqrt(length(x))
    mean <- mean(x)
    t <- (mean - mu)/se
    if (alternative == "less"){
      p_val <- pt(t, df, lower=TRUE)
    } else if (alternative == "greater") {
      p_val <- pt(t, df, lower=FALSE)
    } else {
      p_val <- 2*pt(-abs(t), df)
    }
    output_list <- c("test_stat" = t, "df" = df,
                     "alternative" = alternative, "p-value" = p_val)
    return(output_list)
  }
}
