context("test-savitzkyGolay")

test_that("savitzkyGolay works", {
  nirdata <- data("NIRsoil")

  X_savitzkyGolay <- savitzkyGolay(NIRsoil$spc, m = 1, p = 1, w = 3)

  expect_is(X_savitzkyGolay, "matrix")
  expect_true(round(max(abs(X_savitzkyGolay[1, ])), 5) == 0.00528)
})
