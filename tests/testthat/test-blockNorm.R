context("test-blockNorm")

test_that("blockNorm works with default targetnorm = 1", {
  data("NIRsoil")

  X_blockNorm <- blockNorm(NIRsoil$spc)

  expect_is(X_blockNorm, "list")
  expect_true(round(max(X_blockNorm$Xscaled[1, ]), 5) == 0.00146)
  expect_true(all(c("Xscaled", "f") %in% names(X_blockNorm)))
})

test_that("blockNorm sum of squares equals targetnorm", {
  data("NIRsoil")

  X_blockNorm <- blockNorm(NIRsoil$spc, targetnorm = 1)
  ss <- sum(X_blockNorm$Xscaled^2)
  expect_equal(ss, 1, tolerance = 1e-8)
})

test_that("blockNorm works with targetnorm != 1", {
  data("NIRsoil")

  X_blockNorm <- blockNorm(NIRsoil$spc, targetnorm = 0.5)

  expect_is(X_blockNorm, "list")
  ss <- sum(X_blockNorm$Xscaled^2)
  expect_equal(ss, 0.5, tolerance = 1e-6)
  expect_true(round(max(X_blockNorm$Xscaled[1, ]), 5) == 0.00103)
})

test_that("blockNorm works with data.frame input", {
  data("NIRsoil")

  X_blockNorm <- blockNorm(as.data.frame(NIRsoil$spc))

  expect_is(X_blockNorm, "list")
  expect_is(X_blockNorm$Xscaled, "matrix")
})

test_that("blockNorm errors on non-matrix non-data.frame input", {
  expect_error(blockNorm(1:10))
  expect_error(blockNorm(list(a = 1, b = 2)))
})

test_that("blockNorm scaling factor is consistent with scaled matrix", {
  data("NIRsoil")
  X <- NIRsoil$spc
  res <- blockNorm(X, targetnorm = 1)
  expect_equal(res$Xscaled, X / res$f, tolerance = 1e-12)
})
