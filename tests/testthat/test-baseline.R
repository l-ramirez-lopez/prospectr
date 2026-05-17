context("test-baseline")

test_that("baseline works on matrix input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_baselined <- baseline(round(NIRsoil$spc, 6), wav = wav)

  expect_is(X_baselined, "matrix")
  expect_is(attr(X_baselined, "baselines"), "matrix")
  expect_true(round(mean(X_baselined), 6) == 0.005746)

  # dimensions preserved
  expect_equal(dim(X_baselined), dim(NIRsoil$spc))
  expect_equal(colnames(X_baselined), colnames(NIRsoil$spc))
  expect_equal(rownames(X_baselined), rownames(NIRsoil$spc))

  # baseline attribute has same shape
  expect_equal(dim(attr(X_baselined, "baselines")), dim(NIRsoil$spc))
})

test_that("baseline returns non-negative residuals on reflectance-like data", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  X_baselined <- baseline(NIRsoil$spc, wav = wav)
  # baselined spectra should be >= 0 (baseline removes the convex hull)
  expect_true(all(X_baselined >= -1e-10))
})

test_that("baseline works without wav argument", {
  data("NIRsoil")
  X_baselined <- baseline(NIRsoil$spc)

  expect_is(X_baselined, "matrix")
  expect_equal(nrow(X_baselined), nrow(NIRsoil$spc))
  expect_equal(ncol(X_baselined), ncol(NIRsoil$spc))
})

test_that("baseline works on data.frame input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  X_df <- as.data.frame(NIRsoil$spc)
  X_baselined <- baseline(X_df, wav = wav)

  expect_is(X_baselined, "matrix")
  expect_equal(dim(X_baselined), dim(NIRsoil$spc))
})

test_that("baseline result equals original minus baselines", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  X_baselined <- baseline(NIRsoil$spc, wav = wav)
  baselines <- attr(X_baselined, "baselines")

  expect_equal(unclass(X_baselined), NIRsoil$spc - baselines, tolerance = 1e-10)
})
