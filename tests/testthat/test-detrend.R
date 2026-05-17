context("test-detrend")

test_that("detrend works with default settings (snv = TRUE, p = 2)", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_detrend <- detrend(NIRsoil$spc, wav = wav)

  expect_is(X_detrend, "matrix")
  expect_true(round(max(X_detrend[1, ]), 5) == 2.57863)
  expect_equal(dim(X_detrend), dim(NIRsoil$spc))
})

test_that("detrend works with snv = FALSE", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_det_nosnv <- detrend(NIRsoil$spc, wav = wav, snv = FALSE)

  expect_is(X_det_nosnv, "matrix")
  expect_equal(dim(X_det_nosnv), dim(NIRsoil$spc))
  expect_true(round(max(X_det_nosnv[1, ]), 5) == 0.37141)
  # result should differ from SNV+detrend
  X_det_snv <- detrend(NIRsoil$spc, wav = wav, snv = TRUE)
  expect_false(isTRUE(all.equal(X_det_nosnv, X_det_snv)))
})

test_that("detrend works with higher polynomial order p", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_det_p3 <- detrend(NIRsoil$spc, wav = wav, p = 3)

  expect_is(X_det_p3, "matrix")
  expect_equal(dim(X_det_p3), dim(NIRsoil$spc))
  expect_true(round(max(X_det_p3[1, ]), 5) == 2.72346)
})

test_that("detrend works on data.frame input", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))

  X_det <- detrend(as.data.frame(NIRsoil$spc[1:10, ]), wav = wav)

  expect_is(X_det, "matrix")
  expect_equal(nrow(X_det), 10)
})

test_that("detrend errors when wav is missing", {
  data("NIRsoil")
  expect_error(detrend(NIRsoil$spc))
})

test_that("detrend errors when p < 1", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  expect_error(detrend(NIRsoil$spc, wav = wav, p = 0))
})

test_that("detrend errors when p is non-integer", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  expect_error(detrend(NIRsoil$spc, wav = wav, p = 1.5))
})

test_that("detrend errors when snv is not logical", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  expect_error(detrend(NIRsoil$spc, wav = wav, snv = "yes"))
})

test_that("detrend with snv=TRUE produces near-zero row means before detrending", {
  data("NIRsoil")
  wav <- as.numeric(colnames(NIRsoil$spc))
  X_det <- detrend(NIRsoil$spc[1:20, ], wav = wav, snv = TRUE)
  # detrended output row means should be close to zero
  expect_true(all(abs(rowMeans(X_det)) < 1))
})
