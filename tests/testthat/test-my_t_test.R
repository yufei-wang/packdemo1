test_that("the output is list", {
  expect_is(my_t_test(rnorm(10, 0, 1),"less", 5), "list")
})

test_that("my_t_test throws an error when input is incorrect", {
  expect_error(my_t_test("string"))
})
