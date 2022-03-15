context("test-baseline")

test_that("baseline works", {
  nirdata <- data("NIRsoil")

  X_baselined <- baseline(round(NIRsoil$spc, 6),
    wav = as.numeric(colnames(NIRsoil$spc))
  )

  expect_is(X_baselined, "matrix")
  expect_is(attr(X_baselined, "baselines"), "matrix")
  expect_true(round(mean(X_baselined), 6) == 0.005746)
})
