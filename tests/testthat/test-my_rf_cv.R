data("iris")

test_that("the output is numeric", {
  expect_is(my_rf_cv(k = 5), "numeric")
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("stat302"))
})
