context("test-standardNormalVariate")

test_that("standardNormalVariate works", {
  nirdata <- data("NIRsoil")

  X_standardNormalVariate <- standardNormalVariate(NIRsoil$spc)

  expect_is(X_standardNormalVariate, "matrix")
  expect_true(round(max(X_standardNormalVariate[1, ]), 5) == 2.63444)
})
