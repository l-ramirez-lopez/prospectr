context("test-movav")

test_that("movav works with w=5", {
  data("NIRsoil")

  X_movav <- movav(NIRsoil$spc, 5)

  expect_is(X_movav, "matrix")
  expect_true(round(max(X_movav[1, ]), 5) == 0.37237)
  expect_equal(nrow(X_movav), nrow(NIRsoil$spc))
  expect_true(ncol(X_movav) < ncol(NIRsoil$spc))
})

test_that("movav with w=1 returns input unchanged", {
  data("NIRsoil")

  X_movav1 <- movav(NIRsoil$spc, w = 1)

  expect_equal(X_movav1, NIRsoil$spc)
})

test_that("movav output is smoothed (less variable than input)", {
  data("NIRsoil")

  X_movav <- movav(NIRsoil$spc, w = 11)

  # smoothed version should have smaller column-wise variance
  expect_true(mean(apply(X_movav, 2, var)) <= mean(apply(NIRsoil$spc, 2, var)))
})

test_that("movav works on data.frame input", {
  data("NIRsoil")

  X_movav <- movav(as.data.frame(NIRsoil$spc[1:10, ]), w = 5)

  expect_is(X_movav, "matrix")
  expect_equal(nrow(X_movav), 10)
  expect_true(round(max(X_movav[1, ]), 5) == 0.37237)
})

test_that("movav errors when w is missing", {
  data("NIRsoil")
  expect_error(movav(NIRsoil$spc))
})

test_that("movav errors when w < 1", {
  data("NIRsoil")
  expect_error(movav(NIRsoil$spc, w = 0))
})

test_that("movav errors when w >= ncol(X)", {
  data("NIRsoil")
  expect_error(movav(NIRsoil$spc, w = ncol(NIRsoil$spc)))
})
