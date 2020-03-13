#' k-Nearest Neighbors Cross-Validation
#'
#' This function is to to predict the output class using covariates.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords prediction
#'
#' @return A list with a vector of the predicted class for all observations and
#'  a numeric with the cross-validation misclassification error
#'
#' @examples
#' my_knn_cv(train = iris[, -5], cl = iris$Species, k_nn = 1, k_cv = 5)
#' my_knn_cv(train = iris[, -5], cl = iris$Species, k_nn = 5, k_cv = 5)
#'
#' @import class dplyr
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv){

  if(!is.numeric(k_nn) || !is.numeric(k_cv)) {
    stop("k_nn and k_cv must be numeric !")
  }

  set.seed(302)
  n <- nrow(train)
  # Split data in k_cv parts randomly
  fold <- sample(rep(1:k_cv, length = n))
  data <- data.frame(train, "split" = fold)
  cl1 <- data.frame(cl,  "split" = fold)
  cv_err_vec <- rep(NA, k_cv)

  for (i in 1:k_cv) {
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    cl_train <- cl1 %>% dplyr::filter(split != i)
    cl_test <- cl1 %>% dplyr::filter(split == i)
    cl_train$split <- NULL
    cl_test$split <- NULL

    # predicts output class
    yhat_star <- knn(train = data_train, test = data_test,
                     cl = cl_train[,1, drop = TRUE], k = k_nn)
    #convert to data frame
    # yhat_star.df <- data.frame(yhat_star)
    cv_err_vec[i] = sum(yhat_star.df != cl_test) / nrow(cl_test)
  }
  class <- knn(train = train, test = train, cl = cl, k = k_nn)
  cv_err <- mean(cv_err_vec)
  output <- list("class" = my_class, "cv_err" = cv_err)
  return(output)
}


