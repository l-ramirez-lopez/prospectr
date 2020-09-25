context("test-resample")

test_that("resample works", {
  nirdata <- data("NIRsoil")

  X_resample <- resample(
    NIRsoil$spc, as.numeric(colnames(NIRsoil$spc)),
    seq(1100, 2500, 10)
  )

  expect_is(X_resample, "matrix")

  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.37288)
})
