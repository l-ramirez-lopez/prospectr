context("test-savitzkyGolay")

test_that("savitzkyGolay works with m=1, p=1, w=3", {
  data("NIRsoil")

  X_sg <- savitzkyGolay(NIRsoil$spc, m = 1, p = 1, w = 3)

  expect_is(X_sg, "matrix")
  expect_true(round(max(abs(X_sg[1, ])), 5) == 0.00528)
  expect_equal(nrow(X_sg), nrow(NIRsoil$spc))
  expect_true(ncol(X_sg) < ncol(NIRsoil$spc))
})

test_that("savitzkyGolay smoothing (m=0) preserves approximate values", {
  data("NIRsoil")

  X_smooth <- savitzkyGolay(NIRsoil$spc, m = 0, p = 2, w = 5)

  expect_is(X_smooth, "matrix")
  expect_equal(nrow(X_smooth), nrow(NIRsoil$spc))
  # smoothed values should be in a similar range as input
  expect_true(max(X_smooth) < max(NIRsoil$spc) * 2)
  expect_true(min(X_smooth) > min(NIRsoil$spc) - 0.1)
})

test_that("savitzkyGolay higher order derivative (m=2) works", {
  data("NIRsoil")

  X_sg2 <- savitzkyGolay(NIRsoil$spc, m = 2, p = 3, w = 11)

  expect_is(X_sg2, "matrix")
  expect_equal(nrow(X_sg2), nrow(NIRsoil$spc))
  expect_true(ncol(X_sg2) < ncol(NIRsoil$spc))
})

test_that("savitzkyGolay scales output when delta.wav is provided", {
  data("NIRsoil")
  delta <- 2

  X_scaled   <- savitzkyGolay(NIRsoil$spc, m = 1, p = 2, w = 5, delta.wav = delta)
  X_unscaled <- savitzkyGolay(NIRsoil$spc, m = 1, p = 2, w = 5)

  expect_equal(X_scaled, X_unscaled / delta, tolerance = 1e-10)
})

test_that("savitzkyGolay works on data.frame input", {
  data("NIRsoil")

  X_sg <- savitzkyGolay(as.data.frame(NIRsoil$spc[1:10, ]), m = 1, p = 2, w = 5)

  expect_is(X_sg, "matrix")
  expect_equal(nrow(X_sg), 10)
})

test_that("savitzkyGolay errors when w is even", {
  data("NIRsoil")
  expect_error(savitzkyGolay(NIRsoil$spc, m = 1, p = 2, w = 4))
})

test_that("savitzkyGolay errors when p >= w", {
  data("NIRsoil")
  expect_error(savitzkyGolay(NIRsoil$spc, m = 1, p = 5, w = 5))
})

test_that("savitzkyGolay errors when p < m", {
  data("NIRsoil")
  expect_error(savitzkyGolay(NIRsoil$spc, m = 3, p = 2, w = 5))
})

test_that("savitzkyGolay errors when w >= ncol(X)", {
  data("NIRsoil")
  expect_error(savitzkyGolay(NIRsoil$spc, m = 1, p = 2, w = ncol(NIRsoil$spc) + 1))
})
