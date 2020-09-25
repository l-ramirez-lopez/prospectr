context("test-gapDer")

test_that("gapDer works", {
  nirdata <- data("NIRsoil")

  X_gapDer <- gapDer(NIRsoil$spc, m = 1, w = 3)

  expect_is(X_gapDer, "matrix")
  expect_true(round(max(abs(X_gapDer[1, ])), 5) == 0.00517)
})
