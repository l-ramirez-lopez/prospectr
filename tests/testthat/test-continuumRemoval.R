context("test-continuumRemoval")

test_that("continuumRemoval works", {
  nirdata <- data("NIRsoil")

  X_continuumRemoval <- continuumRemoval(NIRsoil$spc,
    wav = as.numeric(colnames(NIRsoil$spc))
  )

  expect_is(X_continuumRemoval, "matrix")
  expect_true(round(min(X_continuumRemoval[1, ]), 5) == 0.80512)
})
