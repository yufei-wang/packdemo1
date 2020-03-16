#' Random Forest Cross Validation Function
#'
#' This function is to predict output using covariates.
#' @param k number of folds
#' @keywords prediction
#'
#' @return a list with a numeric with the cross-validation error
#'
#' @examples
#' my_rf_cv(5)
#'
#' @import class randomForest dplyr
#'
#' @export

my_rf_cv <- function(k){
  n <- nrow(my_gapminder)
  # randomly assigns observations to folds 1,…,k with equal probability.
  fold <- sample(rep(1:k, length = n))
  data <- data.frame(my_gapminder, "split" = fold)
  cv_err_vec <- rep(NA, k)
  for (i in 1:k) {
    # Within each iteration, define the training data as all the data not in the ith fold.
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)

    #train a random forest model with 100 trees to make prediction
    model <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 100)
    prediction <- predict(model, data_test[, -1])

    # evaluate the MSE
    cv_err_vec[i] = mean((data_test$lifeExp - prediction)^2)
  }

  #Calculate the average MSE across all k folds.
  cv_err <- mean(cv_err_vec)
  return(cv_err)
}

