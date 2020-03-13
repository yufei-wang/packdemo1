test_that("the output is table", {
  expect_is(my_lm(formula = mpg ~ gear, data = mtcars), "table")
})

test_that("incorrect input throws error", {
  expect_error(my_lm("string"))
})
