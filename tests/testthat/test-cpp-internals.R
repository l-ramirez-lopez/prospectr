context("test-cpp-internals")

# ── bitAND ────────────────────────────────────────────────────────────────────
test_that("bitAND performs bitwise AND correctly", {
  expect_equal(bitAND(5L,  3L),  1L)   # 101 & 011 = 001
  expect_equal(bitAND(12L, 10L), 8L)   # 1100 & 1010 = 1000
  expect_equal(bitAND(0L,  5L),  0L)   # anything & 0 = 0
  expect_equal(bitAND(7L,  7L),  7L)   # idempotent
  expect_equal(bitAND(15L, 9L),  9L)   # 1111 & 1001 = 1001
  expect_equal(bitAND(6L,  3L),  2L)   # 110 & 011 = 010
})

# ── bitSR ─────────────────────────────────────────────────────────────────────
test_that("bitSR performs unsigned right shift correctly", {
  expect_equal(bitSR(8L,   2L), 2L)    # 1000 >> 2 = 0010
  expect_equal(bitSR(16L,  1L), 8L)
  expect_equal(bitSR(1L,   0L), 1L)    # shift by 0 is identity
  expect_equal(bitSR(255L, 4L), 15L)   # 11111111 >> 4 = 00001111
  expect_equal(bitSR(32L,  5L), 1L)    # 100000 >> 5 = 1
})

# ── convCppM ──────────────────────────────────────────────────────────────────
test_that("convCppM output dimensions are nrow x (ncol - length(f) + 1)", {
  X <- matrix(1:20, nrow = 4, ncol = 5)
  f <- rep(1/3, 3)
  out <- convCppM(X, f)
  expect_equal(dim(out), c(4L, 3L))
})

test_that("convCppM uniform filter gives running averages", {
  X <- matrix(c(1, 2, 3, 4, 5), nrow = 1)
  f <- rep(1/3, 3)
  out <- convCppM(X, f)
  expect_equal(out[1, 1], 2, tolerance = 1e-10)   # (1+2+3)/3
  expect_equal(out[1, 2], 3, tolerance = 1e-10)   # (2+3+4)/3
  expect_equal(out[1, 3], 4, tolerance = 1e-10)   # (3+4+5)/3
})

test_that("convCppM filter [1, 0, -1] computes signed differences", {
  X <- matrix(c(1, 4, 9, 16, 25), nrow = 1)   # perfect squares
  f <- c(1, 0, -1)
  out <- convCppM(X, f)
  expect_equal(out[1, ], c(1-9, 4-16, 9-25), tolerance = 1e-10)
})

test_that("convCppM works identically on every row", {
  X <- matrix(c(1:5, 6:10), nrow = 2, ncol = 5, byrow = TRUE)
  f <- c(1, -1)    # discrete first difference
  out <- convCppM(X, f)
  # row 1: differences of 1:5 → all -1
  expect_equal(out[1, ], rep(-1, 4), tolerance = 1e-10)
  # row 2: differences of 6:10 → all -1
  expect_equal(out[2, ], rep(-1, 4), tolerance = 1e-10)
})

# ── convCppV ──────────────────────────────────────────────────────────────────
test_that("convCppV output length is length(X) - length(f) + 1", {
  expect_equal(length(convCppV(1:10, rep(1/3, 3))), 8L)
})

test_that("convCppV uniform filter gives running averages", {
  out <- convCppV(c(1, 2, 3, 4, 5), rep(1/3, 3))
  expect_equal(out, c(2, 3, 4), tolerance = 1e-10)
})

test_that("convCppV result matches first row of convCppM", {
  x <- as.numeric(1:10)
  f <- c(0.5, 0.5)
  expect_equal(convCppV(x, f),
               as.numeric(convCppM(matrix(x, nrow = 1), f)),
               tolerance = 1e-10)
})

# ── fastDist ──────────────────────────────────────────────────────────────────
test_that("fastDist output dimensions are nY x nX", {
  X <- matrix(rnorm(15), nrow = 3, ncol = 5)
  Y <- matrix(rnorm(10), nrow = 2, ncol = 5)
  expect_equal(dim(fastDist(X, Y, "euclid")), c(2L, 3L))
})

test_that("fastDist euclid returns squared Euclidean distances", {
  X <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  Y <- matrix(c(2, 0, 0, 2), nrow = 2, byrow = TRUE)
  D <- fastDist(X, Y, "euclid")
  # D[i,j] = ||Y[i,] - X[j,]||^2
  expect_equal(D[1, 1], 1, tolerance = 1e-10)   # (2-1)^2 + (0-0)^2
  expect_equal(D[1, 2], 5, tolerance = 1e-10)   # (2-0)^2 + (0-1)^2
  expect_equal(D[2, 1], 5, tolerance = 1e-10)   # (0-1)^2 + (2-0)^2
  expect_equal(D[2, 2], 1, tolerance = 1e-10)   # (0-0)^2 + (2-1)^2
})

test_that("fastDist euclid diagonal is 0 for identical matrices", {
  set.seed(1)
  X <- matrix(rnorm(12), nrow = 3, ncol = 4)
  expect_equal(diag(as.matrix(fastDist(X, X, "euclid"))),
               rep(0, 3), tolerance = 1e-10)
})

test_that("fastDist euclid matches manual R computation", {
  set.seed(1)
  X <- matrix(rnorm(12), nrow = 3, ncol = 4)
  Y <- matrix(rnorm(8),  nrow = 2, ncol = 4)
  D_cpp <- as.matrix(fastDist(X, Y, "euclid"))
  D_r   <- outer(1:2, 1:3, Vectorize(function(i, j) sum((Y[i, ] - X[j, ])^2)))
  expect_equal(D_cpp, D_r, tolerance = 1e-10)
})

test_that("fastDist cor diagonal is 0 (perfect self-correlation)", {
  set.seed(42)
  X <- matrix(rnorm(20), nrow = 4, ncol = 5)
  expect_equal(diag(as.matrix(fastDist(X, X, "cor"))),
               rep(0, 4), tolerance = 1e-10)
})

test_that("fastDist cor values are in [0, 1]", {
  set.seed(42)
  X <- matrix(rnorm(20), nrow = 4, ncol = 5)
  D <- fastDist(X, X, "cor")
  expect_true(all(D >= -1e-10) && all(D <= 1 + 1e-10))
})

# ── fastDistV ─────────────────────────────────────────────────────────────────
test_that("fastDistV output length equals nrow(X)", {
  X <- matrix(rnorm(15), nrow = 3, ncol = 5)
  Y <- rnorm(5)
  expect_equal(length(fastDistV(X, Y, "euclid")), 3L)
})

test_that("fastDistV euclid returns squared distances to a reference vector", {
  X <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  Y <- c(2, 0)
  d <- fastDistV(X, Y, "euclid")
  expect_equal(d[1], 1, tolerance = 1e-10)   # ||c(1,0) - c(2,0)||^2
  expect_equal(d[2], 5, tolerance = 1e-10)   # ||c(0,1) - c(2,0)||^2
})

test_that("fastDistV euclid distance to own row is 0", {
  set.seed(2)
  X <- matrix(rnorm(15), nrow = 3, ncol = 5)
  for (i in 1:3) {
    d <- fastDistV(X, as.numeric(X[i, ]), "euclid")
    expect_equal(d[i], 0, tolerance = 1e-10)
  }
})

test_that("fastDistV euclid matches manual R computation", {
  set.seed(7)
  X <- matrix(rnorm(15), nrow = 3, ncol = 5)
  Y <- rnorm(5)
  d_cpp <- as.numeric(fastDistV(X, Y, "euclid"))
  d_r   <- apply(X, 1, function(x) sum((x - Y)^2))
  expect_equal(d_cpp, d_r, tolerance = 1e-10)
})

test_that("fastDistV cor distance to own row is 0", {
  set.seed(3)
  X <- matrix(rnorm(20), nrow = 4, ncol = 5)
  for (i in 1:4) {
    d <- fastDistV(X, as.numeric(X[i, ]), "cor")
    expect_equal(d[i], 0, tolerance = 1e-10)
  }
})

# ── get_msc_coeff ─────────────────────────────────────────────────────────────
test_that("get_msc_coeff returns a 2 x nrow(X) matrix", {
  set.seed(1)
  X   <- matrix(rnorm(20), nrow = 4, ncol = 5)
  ref <- rnorm(5)
  coef <- get_msc_coeff(X, ref)
  expect_equal(dim(coef), c(2L, 4L))
})

test_that("get_msc_coeff recovers exact offset and slope for linear spectra", {
  ref <- as.numeric(1:5)
  # row 1: exactly matches ref → offset=0, slope=1
  # row 2: 2 × ref → offset=0, slope=2
  # row 3: 1 + 3 × ref → offset=1, slope=3
  X <- rbind(ref, 2 * ref, 1 + 3 * ref)
  coef <- get_msc_coeff(X, ref)
  expect_equal(coef[1, 1], 0, tolerance = 1e-10)
  expect_equal(coef[2, 1], 1, tolerance = 1e-10)
  expect_equal(coef[1, 2], 0, tolerance = 1e-10)
  expect_equal(coef[2, 2], 2, tolerance = 1e-10)
  expect_equal(coef[1, 3], 1, tolerance = 1e-10)
  expect_equal(coef[2, 3], 3, tolerance = 1e-10)
})

test_that("get_msc_coeff matches lm() with intercept", {
  set.seed(5)
  ref <- rnorm(10)
  X   <- matrix(rnorm(30), nrow = 3, ncol = 10)
  coef_cpp <- get_msc_coeff(X, ref)
  for (i in 1:3) {
    lm_coef <- unname(coef(lm(X[i, ] ~ ref)))
    expect_equal(coef_cpp[1, i], lm_coef[1], tolerance = 1e-8)  # intercept
    expect_equal(coef_cpp[2, i], lm_coef[2], tolerance = 1e-8)  # slope
  }
})

# ── resample_fwhm ─────────────────────────────────────────────────────────────
test_that("resample_fwhm returns nrow(X) x length(new_wav) matrix", {
  wav     <- seq(400, 2400, by = 10)
  new_wav <- c(800, 1200, 1600, 2000)
  fwhm    <- rep(50, 4)
  set.seed(1)
  X   <- matrix(runif(5 * length(wav)), nrow = 5)
  out <- resample_fwhm(X, wav, new_wav, fwhm)
  expect_equal(dim(out), c(5L, 4L))
})

test_that("resample_fwhm returns 0 for bands outside ±3*sigma of wav range", {
  wav  <- seq(1000, 2000, by = 10)
  X    <- matrix(rep(1, length(wav)), nrow = 1)
  # 500 nm is far below min(wav) = 1000; condition new_wav - 3*sdx >= min(wav) fails
  out  <- resample_fwhm(X, wav, 500, 50)
  expect_equal(out[1, 1], 0)
})

test_that("resample_fwhm on a constant spectrum returns the constant value", {
  wav     <- seq(400, 2400, by = 2)
  new_wav <- c(800, 1200, 1600, 2000)
  fwhm    <- rep(50, 4)
  X   <- matrix(rep(0.5, length(wav)), nrow = 1)
  out <- resample_fwhm(X, wav, new_wav, fwhm)
  expect_equal(out[1, ], rep(0.5, 4), tolerance = 1e-6)
})

test_that("resample_fwhm result is consistent across rows for identical spectra", {
  wav     <- seq(400, 2400, by = 2)
  new_wav <- c(1000, 1500, 2000)
  fwhm    <- rep(60, 3)
  set.seed(9)
  x   <- runif(length(wav))
  X   <- matrix(rep(x, 3), nrow = 3, byrow = TRUE)
  out <- resample_fwhm(X, wav, new_wav, fwhm)
  expect_equal(out[1, ], out[2, ], tolerance = 1e-12)
  expect_equal(out[1, ], out[3, ], tolerance = 1e-12)
})

# ── resample_fwhm_vec ─────────────────────────────────────────────────────────
test_that("resample_fwhm_vec returns vector of length(new_wav)", {
  wav     <- seq(400, 2400, by = 10)
  new_wav <- c(800, 1200, 1600, 2000)
  fwhm    <- rep(50, 4)
  set.seed(1)
  x   <- runif(length(wav))
  out <- resample_fwhm_vec(x, wav, new_wav, fwhm)
  expect_equal(length(out), 4L)
})

test_that("resample_fwhm_vec matches first row of resample_fwhm", {
  wav     <- seq(400, 2400, by = 2)
  new_wav <- c(800, 1200, 1600, 2000)
  fwhm    <- rep(50, 4)
  set.seed(1)
  x <- runif(length(wav))
  expect_equal(
    as.numeric(resample_fwhm_vec(x, wav, new_wav, fwhm)),
    as.numeric(resample_fwhm(matrix(x, nrow = 1), wav, new_wav, fwhm)[1, ]),
    tolerance = 1e-12
  )
})

test_that("resample_fwhm_vec on a constant spectrum returns the constant", {
  wav     <- seq(400, 2400, by = 2)
  new_wav <- c(800, 1200, 1600, 2000)
  fwhm    <- rep(50, 4)
  out <- resample_fwhm_vec(rep(0.3, length(wav)), wav, new_wav, fwhm)
  expect_equal(out, rep(0.3, 4), tolerance = 1e-6)
})

# ── residLm ───────────────────────────────────────────────────────────────────
test_that("residLm returns matrix with same dimensions as Yr", {
  set.seed(1)
  Xr <- matrix(rnorm(20), nrow = 10, ncol = 2)  # 10 obs, 2 predictors
  Yr <- matrix(rnorm(30), nrow = 3, ncol = 10)  # 3 responses, 10 obs each
  out <- residLm(Yr, Xr)
  expect_equal(dim(out), dim(Yr))
})

test_that("residLm returns zero residuals when Yr lies in column space of Xr", {
  Xr <- matrix(c(1, 2, 3, 4, 5), ncol = 1)         # 5 x 1
  # Row of Yr is exactly 2 × Xr → OLS fit is perfect
  Yr <- matrix(c(2, 4, 6, 8, 10), nrow = 1)        # 1 x 5
  resids <- residLm(Yr, Xr)
  expect_equal(resids[1, ], rep(0, 5), tolerance = 1e-10)
})

test_that("residLm residuals are orthogonal to predictor columns", {
  set.seed(42)
  Xr    <- poly(1:10, 2)                           # 10 x 2 orthogonal basis
  Yr    <- matrix(rnorm(20), nrow = 2, ncol = 10)
  resids <- residLm(Yr, Xr)
  for (i in 1:2) {
    # t(resid) %*% Xr should be ~0 (orthogonality condition)
    expect_equal(as.numeric(resids[i, ] %*% Xr),
                 rep(0, 2), tolerance = 1e-8)
  }
})

test_that("residLm matches lm() residuals (no intercept)", {
  set.seed(99)
  Xr     <- poly(seq(400, 2400, length.out = 20), 2)  # 20 x 2
  Yr     <- matrix(rnorm(40), nrow = 2, ncol = 20)
  resids_cpp <- residLm(Yr, Xr)
  for (i in 1:2) {
    resids_r <- as.numeric(residuals(lm(Yr[i, ] ~ Xr - 1)))
    expect_equal(resids_cpp[i, ], resids_r, tolerance = 1e-8)
  }
})
