context("test-blockNorm")

test_that("blockNorm works", {
  nirdata <- data("NIRsoil")

  X_blockNorm <- blockNorm(NIRsoil$spc)

  expect_is(X_blockNorm, "list")
  expect_true(round(max(X_blockNorm$Xscaled[1, ]), 5) == 0.00146)
})
