context("test-binning")

test_that("binning works", {
  nirdata <- data("NIRsoil")

  X_binning <- binning(NIRsoil$spc)

  expect_is(X_binning, "matrix")
  expect_true(round(max(X_binning[1, ]), 5) == 0.37257)
})
