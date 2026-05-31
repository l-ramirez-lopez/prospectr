context("test-resample2")

test_that("resample2 works with explicit fwhm vector", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- c(1650, 2165, 2205, 2260, 2330, 2395)
  fwhm <- c(100, 40, 40, 50, 70, 70)

  X_resample <- resample2(NIRsoil$spc, wav, new_wav, fwhm)

  expect_is(X_resample, "matrix")
  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.34966)
  expect_equal(nrow(X_resample), nrow(NIRsoil$spc))
  expect_equal(ncol(X_resample), length(new_wav))
  expect_equal(as.numeric(colnames(X_resample)), new_wav)
})

test_that("resample2 works with scalar fwhm (constant bandwidth)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- c(1200, 1400, 1600, 1800, 2000)

  X_resample <- resample2(NIRsoil$spc, wav, new_wav, fwhm = 50)

  expect_is(X_resample, "matrix")
  expect_equal(nrow(X_resample), nrow(NIRsoil$spc))
  expect_equal(ncol(X_resample), length(new_wav))
  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.32769)
})

test_that("resample2 works without fwhm (defaults to band spacing)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- c(1200, 1400, 1600, 1800, 2000)

  X_resample <- resample2(NIRsoil$spc, wav, new_wav)

  expect_is(X_resample, "matrix")
  expect_equal(nrow(X_resample), nrow(NIRsoil$spc))
  expect_equal(ncol(X_resample), length(new_wav))
  expect_true(round(max(abs(X_resample[1, ])), 5) == 0.32143)
})

test_that("resample2 works on data.frame input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- c(1200, 1400, 1600)
  fwhm <- c(50, 50, 50)

  X_res <- resample2(as.data.frame(NIRsoil$spc[1:10, ]), wav, new_wav, fwhm)

  expect_is(X_res, "matrix")
  expect_equal(nrow(X_res), 10)
  expect_equal(ncol(X_res), 3)
})

test_that("resample2 errors when wav is missing", {
  data("NIRsoil")
  expect_error(resample2(NIRsoil$spc, new.wav = c(1200, 1400), fwhm = c(50, 50)))
})

test_that("resample2 errors when new.wav is missing", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  expect_error(resample2(NIRsoil$spc, wav = wav, fwhm = 50))
})

test_that("resample2 errors when fwhm length mismatches new.wav", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  new_wav <- c(1200, 1400, 1600)
  expect_error(resample2(NIRsoil$spc, wav, new_wav, fwhm = c(50, 50)))
})

test_that("resample2 errors when wav length mismatches ncol(X)", {
  data("NIRsoil")
  new_wav <- c(1200, 1400)
  expect_error(resample2(NIRsoil$spc, wav = 1:10, new.wav = new_wav, fwhm = c(50, 50)))
})
