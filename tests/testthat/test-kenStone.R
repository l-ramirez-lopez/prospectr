context("test-kenStone")

test_that("kenStone works", {
  nirdata <- data("NIRsoil")

  X_kenStone <- kenStone(NIRsoil$spc, k = 5, metric = "mahal", pc = 3)
  X_kenStone$model

  expect_is(X_kenStone, "list")

  sel_samples <- c(687, 377, 410, 619, 87)
  expect_true(!any(!sel_samples == X_kenStone$model))
})
