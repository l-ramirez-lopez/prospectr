context("test-blockScale")

test_that("blockScale works", {
  nirdata <- data("NIRsoil")

  X_blockScale <- blockScale(NIRsoil$spc, type = "hard", sigma2 = 1)
  X_blockScale_soft <- blockScale(NIRsoil$spc, type = "soft", sigma2 = 1)

  expect_is(X_blockScale, "list")
  expect_is(X_blockScale_soft, "list")
  expect_true(round(max(X_blockScale$Xscaled[1, ]), 5) == 0.15734)
  expect_true(round(max(X_blockScale_soft$Xscaled[1, ]), 5) == 0.80929)
})
