context("test-duplex")

test_that("duplex works", {
  nirdata <- data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 30, metric = "mahal", pc = .99)
  X_duplex$model

  expect_is(X_duplex, "list")

  sel_samples <- c(
    789, 619, 39, 617, 594, 310, 737, 822, 683, 614, 204, 287,
    825, 218, 701, 268, 717, 350, 615, 702, 687, 653, 186, 479,
    421, 282, 178, 728, 254, 613
  )
  expect_true(!any(!sel_samples == X_duplex$model))
})
