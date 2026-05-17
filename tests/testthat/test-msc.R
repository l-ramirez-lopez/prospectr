context("test-msc")

test_that("msc works with default (mean spectrum) reference", {
  data("NIRsoil")

  X_msc <- msc(NIRsoil$spc)

  expect_is(X_msc, "matrix")
  expect_true(round(max(X_msc[1, ]), 5) == 0.37394)
  expect_true(round(min(X_msc[1, ]), 5) == 0.29474)
  expect_equal(dim(X_msc), dim(NIRsoil$spc))
})

test_that("msc works with custom reference spectrum (median)", {
  data("NIRsoil")

  X_mscb <- msc(NIRsoil$spc, apply(NIRsoil$spc, 2, median))

  expect_is(X_mscb, "matrix")
  expect_true(round(max(X_mscb[1, ]), 5) == 0.34816)
  expect_true(round(min(X_mscb[1, ]), 5) == 0.26749)
})

test_that("msc attaches reference spectrum as attribute", {
  data("NIRsoil")

  X_msc <- msc(NIRsoil$spc)
  ref <- attr(X_msc, "Reference spectrum:")

  expect_is(ref, "numeric")
  expect_equal(length(ref), ncol(NIRsoil$spc))
  expect_equal(ref, colMeans(NIRsoil$spc), tolerance = 1e-12)
})

test_that("msc reference attribute is preserved for transfer correction", {
  data("NIRsoil")

  spectra_a <- NIRsoil$spc[1:40, ]
  spectra_b <- NIRsoil$spc[-(1:40), ]

  spectra_a_msc <- msc(spectra_a)
  ref_attr <- attr(spectra_a_msc, "Reference spectrum:")

  spectra_b_msc <- msc(spectra_b, ref_spectrum = ref_attr)

  expect_is(spectra_b_msc, "matrix")
  expect_equal(nrow(spectra_b_msc), nrow(spectra_b))
})

test_that("msc accepts data.frame input", {
  data("NIRsoil")

  X_msc <- msc(as.data.frame(NIRsoil$spc))

  expect_is(X_msc, "matrix")
  expect_equal(dim(X_msc), dim(NIRsoil$spc))
})

test_that("msc errors when ref_spectrum is not a vector", {
  data("NIRsoil")
  expect_error(msc(NIRsoil$spc, ref_spectrum = NIRsoil$spc[1:2, ]))
})

test_that("msc errors when ref_spectrum length mismatches ncol(X)", {
  data("NIRsoil")
  expect_error(msc(NIRsoil$spc, ref_spectrum = 1:10))
})
