context("test-binning")

test_that("binning works with default bins", {
  data("NIRsoil")

  X_binning <- binning(NIRsoil$spc)

  expect_is(X_binning, "matrix")
  expect_true(round(max(X_binning[1, ]), 5) == 0.37257)
  # when no bins or bin.size supplied, X is returned as-is
  expect_equal(dim(X_binning), dim(NIRsoil$spc))
})

test_that("binning works with bin.size argument", {
  data("NIRsoil")

  X_binned <- binning(NIRsoil$spc, bin.size = 10)

  expect_is(X_binned, "matrix")
  expect_equal(nrow(X_binned), nrow(NIRsoil$spc))
  # fewer columns than original
  expect_true(ncol(X_binned) < ncol(NIRsoil$spc))
})

test_that("binning works with bins argument", {
  data("NIRsoil")

  n_bins <- 20
  X_binned <- binning(NIRsoil$spc, bins = n_bins)

  expect_is(X_binned, "matrix")
  expect_equal(nrow(X_binned), nrow(NIRsoil$spc))
  expect_equal(ncol(X_binned), n_bins)
})

test_that("binning errors when both bins and bin.size are given", {
  data("NIRsoil")
  expect_error(binning(NIRsoil$spc, bins = 10, bin.size = 5))
})

test_that("binning works on a data.frame input", {
  data("NIRsoil")
  X_binned <- binning(as.data.frame(NIRsoil$spc), bins = 10)
  expect_is(X_binned, "matrix")
  expect_equal(ncol(X_binned), 10)
})

test_that("binning preserves row count", {
  data("NIRsoil")
  X_binned <- binning(NIRsoil$spc, bins = 30)
  expect_equal(nrow(X_binned), nrow(NIRsoil$spc))
})
