context("test-msc")

test_that("msc", {
  nirdata <- data("NIRsoil")

  X_msc <- msc(NIRsoil$spc)

  expect_is(X_msc, "matrix")
  expect_true(round(max(X_msc[1, ]), 5) == 0.37394)
  expect_true(round(min(X_msc[1, ]), 5) == 0.29474)

  X_mscb <- msc(NIRsoil$spc, apply(NIRsoil$spc, 2, median))
  expect_true(round(max(X_mscb[1, ]), 5) == 0.34816)
  expect_true(round(min(X_mscb[1, ]), 5) == 0.26749)
})
