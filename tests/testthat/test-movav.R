context("test-movav")

test_that("movav works", {
  nirdata <- data("NIRsoil")

  X_movav <- movav(NIRsoil$spc, 5)

  expect_is(X_movav, "matrix")
  expect_true(round(max(X_movav[1, ]), 5) == 0.37237)
})
