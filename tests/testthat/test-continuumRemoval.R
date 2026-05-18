context("test-continuumRemoval")

test_that("continuumRemoval works with default settings (type=R, division)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr <- continuumRemoval(NIRsoil$spc, wav = wav)

  expect_is(X_cr, "matrix")
  expect_true(round(min(X_cr[1, ]), 5) == 0.80512)
  expect_equal(dim(X_cr), dim(NIRsoil$spc))
})

test_that("continuumRemoval values are in (0, 1] for reflectance division", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr <- continuumRemoval(NIRsoil$spc, wav = wav, type = "R", method = "division")

  expect_true(all(X_cr >= 0 - 1e-10))
  expect_true(all(X_cr <= 1 + 1e-10))
})

test_that("continuumRemoval works with type = 'A' (absorbance)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr_A <- continuumRemoval(NIRsoil$spc, wav = wav, type = "A")

  expect_is(X_cr_A, "matrix")
  expect_equal(dim(X_cr_A), dim(NIRsoil$spc))
  expect_true(round(min(X_cr_A[1, ]), 5) == 0)
})

test_that("continuumRemoval works with method = 'subtraction'", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr_sub <- continuumRemoval(NIRsoil$spc, wav = wav, method = "subtraction")

  expect_is(X_cr_sub, "matrix")
  expect_equal(dim(X_cr_sub), dim(NIRsoil$spc))
  expect_true(round(min(X_cr_sub[1, ]), 5) == 0.92942)
})

test_that("continuumRemoval works with interpol = 'spline'", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr_sp <- continuumRemoval(NIRsoil$spc[1:10, ], wav = wav, interpol = "spline")

  expect_is(X_cr_sp, "matrix")
  expect_equal(nrow(X_cr_sp), 10)
  expect_equal(ncol(X_cr_sp), ncol(NIRsoil$spc))
  expect_true(round(min(X_cr_sp[1, ]), 5) == 0.80512)
})

test_that("continuumRemoval works without wav argument", {
  data("NIRsoil")

  X_cr <- continuumRemoval(NIRsoil$spc[1:5, ])

  expect_is(X_cr, "matrix")
  expect_equal(dim(X_cr), dim(NIRsoil$spc[1:5, ]))
})

test_that("continuumRemoval works on data.frame input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_cr <- continuumRemoval(as.data.frame(NIRsoil$spc[1:5, ]), wav = wav)

  expect_is(X_cr, "matrix")
})

test_that("continuumRemoval errors when wav length mismatches ncol(X)", {
  data("NIRsoil")
  expect_error(continuumRemoval(NIRsoil$spc, wav = 1:10))
})
