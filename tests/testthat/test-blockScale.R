context("test-blockScale")

test_that("blockScale hard scaling works", {
  data("NIRsoil")

  X_blockScale <- blockScale(NIRsoil$spc, type = "hard", sigma2 = 1)

  expect_is(X_blockScale, "list")
  expect_true(round(max(X_blockScale$Xscaled[1, ]), 5) == 0.15734)
  expect_true(all(c("Xscaled", "f") %in% names(X_blockScale)))
})

test_that("blockScale soft scaling works", {
  data("NIRsoil")

  X_blockScale_soft <- blockScale(NIRsoil$spc, type = "soft", sigma2 = 1)

  expect_is(X_blockScale_soft, "list")
  expect_true(round(max(X_blockScale_soft$Xscaled[1, ]), 5) == 0.80929)
})

test_that("blockScale hard: sum of column variances equals sigma2", {
  data("NIRsoil")
  sigma2 <- 1
  res <- blockScale(NIRsoil$spc, type = "hard", sigma2 = sigma2)
  total_var <- sum(apply(res$Xscaled, 2, var))
  expect_equal(total_var, sigma2, tolerance = 1e-8)
})

test_that("blockScale works on data.frame input", {
  data("NIRsoil")
  res <- blockScale(as.data.frame(NIRsoil$spc), type = "hard", sigma2 = 1)
  expect_is(res, "list")
  expect_is(res$Xscaled, "matrix")
})

test_that("blockScale errors on non-matrix non-data.frame input", {
  expect_error(blockScale(1:10))
  expect_error(blockScale(list(a = 1)))
})

test_that("blockScale scaling factor is consistent with scaled matrix", {
  data("NIRsoil")
  X <- NIRsoil$spc
  res <- blockScale(X, type = "hard", sigma2 = 1)
  expected <- t(t(X) / res$f)
  expect_equal(res$Xscaled, expected, tolerance = 1e-12)
})
