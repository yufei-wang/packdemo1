test_that("the output is table", {
  expect_is(my_lm(formula = lifeExp ~ gdpPercap + continent,
                        data = my_gapminder), "table")
})

test_that("incorrect input throws error", {
  expect_error(my_lm("string"))
})
