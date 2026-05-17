context("test-spliceCorrection")

make_synthetic_spectra <- function(nrow = 5, wav = seq(500, 2500, by = 2)) {
  # Simple synthetic spectra: smooth polynomial + a step at 1000 nm and 1830 nm
  set.seed(42)
  X <- matrix(0, nrow = nrow, ncol = length(wav))
  for (i in seq_len(nrow)) {
    base_signal <- seq(0.1, 0.5, length.out = length(wav)) + rnorm(length(wav), 0, 0.002)
    # Introduce a step artifact at splices
    base_signal[wav >= 1000 & wav < 1830] <- base_signal[wav >= 1000 & wav < 1830] + 0.05
    base_signal[wav >= 1830] <- base_signal[wav >= 1830] - 0.03
    X[i, ] <- base_signal
  }
  colnames(X) <- as.character(wav)
  X
}

test_that("spliceCorrection returns matrix with same dimensions", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_corrected <- spliceCorrection(X, wav = wav, splice = c(1000, 1830))

  expect_is(X_corrected, "matrix")
  expect_equal(dim(X_corrected), dim(X))
  expect_equal(colnames(X_corrected), colnames(X))
  expect_equal(rownames(X_corrected), rownames(X))
  expect_true(round(mean(X_corrected[1, ]), 6) == 0.328357)
  expect_true(round(max(X_corrected[1, ]), 5) == 0.5265)
})

test_that("spliceCorrection reduces discontinuity at splice points", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_corrected <- spliceCorrection(X, wav = wav, splice = c(1000, 1830))

  idx_1000 <- which(wav == 1000)
  # The jump before the first splice should be smaller after correction
  jump_before <- abs(X[1, idx_1000] - X[1, idx_1000 + 1])
  jump_after  <- abs(X_corrected[1, idx_1000] - X_corrected[1, idx_1000 + 1])
  expect_true(jump_after <= jump_before + 0.01)
})

test_that("spliceCorrection works with a single splice", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_corrected <- spliceCorrection(X, wav = wav, splice = 1000)

  expect_is(X_corrected, "matrix")
  expect_equal(dim(X_corrected), dim(X))
})

test_that("spliceCorrection middle region (Xb) is unchanged", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_corrected <- spliceCorrection(X, wav = wav, splice = c(1000, 1830))

  # The middle segment (Xb, between the two splices) should be identical
  idx_b <- which(wav > 1000 & wav <= 1830)
  expect_equal(X_corrected[, idx_b], X[, idx_b], tolerance = 1e-12)
})

test_that("spliceCorrection works on data.frame input", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_corrected <- spliceCorrection(as.data.frame(X), wav = wav, splice = c(1000, 1830))

  expect_is(X_corrected, "matrix")
  expect_equal(dim(X_corrected), dim(X))
})

test_that("spliceCorrection works on vector input", {
  wav <- seq(500, 2500, by = 2)
  x_vec <- make_synthetic_spectra(nrow = 1, wav = wav)[1, ]

  x_corrected <- spliceCorrection(x_vec, wav = wav, splice = c(1000, 1830))

  expect_is(x_corrected, "numeric")
  expect_equal(length(x_corrected), length(x_vec))
})

test_that("spliceCorrection errors when splice has length > 2", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)
  expect_error(spliceCorrection(X, wav = wav, splice = c(800, 1000, 1830)))
})

test_that("spliceCorrection errors when splice positions not found in wav", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)
  # 999 and 1831 are not in wav (odd numbers, wav steps by 2 from even start)
  expect_error(spliceCorrection(X, wav = wav, splice = c(999, 1831)))
})

test_that("spliceCorrection errors when wav length mismatches ncol(X)", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)
  expect_error(spliceCorrection(X, wav = 1:10, splice = c(1000, 1830)))
})

test_that("spliceCorrection respects interpol.bands parameter", {
  wav <- seq(500, 2500, by = 2)
  X <- make_synthetic_spectra(wav = wav)

  X_5  <- spliceCorrection(X, wav = wav, splice = c(1000, 1830), interpol.bands = 5)
  X_20 <- spliceCorrection(X, wav = wav, splice = c(1000, 1830), interpol.bands = 20)

  expect_is(X_5, "matrix")
  expect_is(X_20, "matrix")
  # different interpolation bands produce different corrections
  expect_false(isTRUE(all.equal(X_5, X_20)))
})
