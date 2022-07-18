context("test-detrend")

test_that("detrend works", {
  nirdata <- data("NIRsoil")

  X_detrend <- detrend(NIRsoil$spc, wav = as.numeric(colnames(NIRsoil$spc)))

  expect_is(X_detrend, "matrix")
  expect_true(round(max(X_detrend[1, ]), 5) == 2.57863)
})
