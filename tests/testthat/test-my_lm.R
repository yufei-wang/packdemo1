data("my_gapminder")
test_that("my_lm works correctly", {
  expect_is(my_lm(formula = lifeExp ~ gdpPercap + continent,
                  data = my_gapminder)$Estimate, "numeric")
  expect_is(my_lm(formula = lifeExp ~ gdpPercap + continent,
                  data = my_gapminder)$Std.Error, "numeric")
  expect_is(my_lm(formula = lifeExp ~ gdpPercap + continent,
                  data = my_gapminder)$t.value, "numeric")
  expect_is(my_lm(formula = lifeExp ~ gdpPercap + continent,
                  data = my_gapminder), "data.frame")
})

test_that("incorrect input throws error", {
  expect_error(my_lm(formula = stat302, data = my_gapminder))
})
