context("test-gapDer")

test_that("gapDer works with m=1, w=3 (Norris-Gap)", {
  data("NIRsoil")

  X_gapDer <- gapDer(NIRsoil$spc, m = 1, w = 3)

  expect_is(X_gapDer, "matrix")
  expect_true(round(max(abs(X_gapDer[1, ])), 5) == 0.00517)
  # columns are reduced by gap effect
  expect_true(ncol(X_gapDer) < ncol(NIRsoil$spc))
  expect_equal(nrow(X_gapDer), nrow(NIRsoil$spc))
})

test_that("gapDer works with second order derivative (m=2)", {
  data("NIRsoil")

  X_gapDer2 <- gapDer(NIRsoil$spc, m = 2, w = 3)

  expect_is(X_gapDer2, "matrix")
  expect_equal(nrow(X_gapDer2), nrow(NIRsoil$spc))
  expect_true(ncol(X_gapDer2) < ncol(NIRsoil$spc))
  expect_true(round(max(abs(X_gapDer2[1, ])), 5) == 0.00089)
})

test_that("gapDer works with segment smoothing (s > 1)", {
  data("NIRsoil")

  X_gapDer_s3 <- gapDer(NIRsoil$spc, m = 1, w = 3, s = 3)

  expect_is(X_gapDer_s3, "matrix")
  expect_equal(nrow(X_gapDer_s3), nrow(NIRsoil$spc))
  expect_true(round(max(abs(X_gapDer_s3[1, ])), 5) == 0.00497)
  # smoothed version has fewer columns than w=3, s=1
  X_gapDer_s1 <- gapDer(NIRsoil$spc, m = 1, w = 3, s = 1)
  expect_true(ncol(X_gapDer_s3) < ncol(X_gapDer_s1))
})

test_that("gapDer scales output correctly when delta.wav is provided", {
  data("NIRsoil")
  delta <- 2

  X_scaled   <- gapDer(NIRsoil$spc, m = 1, w = 3, delta.wav = delta)
  X_unscaled <- gapDer(NIRsoil$spc, m = 1, w = 3)

  expect_equal(X_scaled, X_unscaled / delta, tolerance = 1e-10)
})

test_that("gapDer works on data.frame input", {
  data("NIRsoil")

  X_gd <- gapDer(as.data.frame(NIRsoil$spc[1:10, ]), m = 1, w = 3)

  expect_is(X_gd, "matrix")
  expect_equal(nrow(X_gd), 10)
})

test_that("gapDer errors when w is even", {
  data("NIRsoil")
  expect_error(gapDer(NIRsoil$spc, m = 1, w = 4))
})

test_that("gapDer errors when w < 1", {
  data("NIRsoil")
  expect_error(gapDer(NIRsoil$spc, m = 1, w = 0))
})

test_that("gapDer errors when s is even", {
  data("NIRsoil")
  expect_error(gapDer(NIRsoil$spc, m = 1, w = 3, s = 2))
})

test_that("gapDer errors when filter length exceeds ncol(X)", {
  data("NIRsoil")
  expect_error(gapDer(NIRsoil$spc, m = 1, w = 999))
})
