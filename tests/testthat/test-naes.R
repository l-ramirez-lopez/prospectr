context("test-naes")

test_that("naes works", {
  nirdata <- data("NIRsoil")
  skip_on_cran()
  skip_on_travis()

  set.seed(140920)
  X_naes <- naes(NIRsoil$spc, k = 30)
  X_naes$model

  expect_is(X_naes, "list")

  sel_samples <- c(
    7, 715, 380, 215, 483, 588, 530, 211, 253, 389, 57, 540, 166,
    30, 338, 370, 654, 620, 551, 770, 667, 4, 629, 439, 486, 687,
    10, 149, 797, 649
  )
  expect_true(!any(!sel_samples == X_naes$model))
})
