context("test-resample2")

test_that("resample2 works", {
  nirdata <- data("NIRsoil")

  new_wav <- c(1650, 2165, 2205, 2260, 2330, 2395)
  fwhm <- c(100, 40, 40, 50, 70, 70)
  X_resample <- resample2(
    NIRsoil$spc, as.numeric(colnames(NIRsoil$spc)),
    new_wav, fwhm
  )

  expect_is(X_resample, "matrix")

  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.34966)
})
