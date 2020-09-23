context("test-kenStone")

test_that("kenStone works", {
  nirdata <- data("NIRsoil")
  
  X_kenStone <- kenStone(NIRsoil$spc, k = 30)
  X_kenStone$model
  
  expect_is(X_kenStone, "list")
  
  sel_samples <- c(619, 402, 141, 614, 186, 715, 618, 248, 710, 410, 823, 147, 
                   818, 686, 706, 617, 612, 290, 572, 825, 615, 284, 320, 578, 
                   313, 279, 282, 351, 540, 204)
  expect_true(!any(!sel_samples == X_kenStone$model))
})
