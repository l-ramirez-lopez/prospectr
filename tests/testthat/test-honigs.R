context("test-honigs")

test_that("honigs works", {
  nirdata <- data("NIRsoil")

  X_honigs <- honigs(NIRsoil$spc, k = 30, type = "A")
  X_honigs$model

  expect_is(X_honigs, "list")

  sel_samples <- c(
    410, 619, 386, 141, 618, 687, 377, 37, 789, 592, 431, 402,
    572, 333, 186, 519, 710, 610, 342, 282, 330, 290, 614, 279,
    315, 284, 248, 147, 749, 823
  )
  expect_true(!any(!sel_samples == X_honigs$model))
})
