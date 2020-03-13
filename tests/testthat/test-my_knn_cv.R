data("iris")

test_that("the output is a list", {
  expect_is(my_knn_cv(train = iris[, -5], cl = iris$Species,
                      k_nn=1, k_cv = 5), "list")
})

test_that("non numeric input for k_nn and k_cv throws error", {
  expect_error(my_knn_cv(train, cl, 302, "stat"))
  expect_error(my_knn_cv(train, cl, "stat", 302))
})


