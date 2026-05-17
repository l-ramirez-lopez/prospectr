context("test-shenkWest")

test_that("shenkWest works", {
  nirdata <- data("NIRsoil")

  X_shenkWest <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)
  X_shenkWest$model

  expect_is(X_shenkWest, "list")

  sel_samples <- c(
    112, 200, 309, 690, 297, 452, 345, 608, 225, 595, 63, 732,
    824, 126, 706, 294, 313, 528, 154, 585, 612, 617
  )
  expect_true(!any(!sel_samples == X_shenkWest$model))
})
