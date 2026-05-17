context("test-resample")

test_that("resample works with spline interpolation (default)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- seq(1100, 2500, 10)

  X_resample <- resample(NIRsoil$spc, wav, new_wav)

  expect_is(X_resample, "matrix")
  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.37288)
  expect_equal(nrow(X_resample), nrow(NIRsoil$spc))
  expect_equal(ncol(X_resample), length(new_wav))
  expect_equal(as.numeric(colnames(X_resample)), new_wav)
})

test_that("resample works with linear interpolation", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- seq(1100, 2500, 10)

  X_linear <- resample(NIRsoil$spc, wav, new_wav, interpol = "linear")

  expect_is(X_linear, "matrix")
  expect_equal(nrow(X_linear), nrow(NIRsoil$spc))
  expect_equal(ncol(X_linear), length(new_wav))
})

test_that("resample spline and linear produce different results", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- seq(1100, 2500, 10)

  X_spline <- resample(NIRsoil$spc, wav, new_wav, interpol = "spline")
  X_linear <- resample(NIRsoil$spc, wav, new_wav, interpol = "linear")

  expect_false(isTRUE(all.equal(X_spline, X_linear)))
})

test_that("resample preserves values at original wavelengths (linear)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_resampled <- resample(NIRsoil$spc[1:5, ], wav, wav, interpol = "linear")

  expect_equal(X_resampled, NIRsoil$spc[1:5, ], tolerance = 1e-10,
               check.attributes = FALSE)
})

test_that("resample works on data.frame input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- seq(1100, 2500, 20)

  X_res <- resample(as.data.frame(NIRsoil$spc[1:10, ]), wav, new_wav)

  expect_is(X_res, "matrix")
  expect_equal(nrow(X_res), 10)
  expect_equal(ncol(X_res), length(new_wav))
})

test_that("resample errors when wav argument is missing", {
  data("NIRsoil")
  expect_error(resample(NIRsoil$spc, new.wav = seq(1100, 2500, 10)))
})

test_that("resample errors when wav length mismatches ncol(X)", {
  data("NIRsoil")
  expect_error(resample(NIRsoil$spc, wav = 1:10, new.wav = seq(1100, 2500, 10)))
})

test_that("resample errors on invalid interpol argument", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  expect_error(resample(NIRsoil$spc, wav, wav, interpol = "cubic"))
})
