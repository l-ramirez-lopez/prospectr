context("test-kenStone")

test_that("kenStone works", {
  nirdata <- data("NIRsoil")

  X_kenStone <- kenStone(NIRsoil$spc, k = 50, metric = "mahal", pc = 3)
  X_kenStone$model

  expect_is(X_kenStone, "list")

  sel_samples <- c(687, 377, 410, 619, 87, 618, 611, 283, 317, 666, 789, 635,
                   147, 822, 285, 313, 737, 803, 819, 383, 823, 204, 591, 252, 
                   825, 272, 402, 39, 330, 590, 286, 608, 363, 234, 701, 718,
                   287, 270, 571, 192, 614, 1, 386, 615, 126, 755, 734, 428,
                   466, 426)
  expect_true(!any(!sel_samples == X_kenStone$model))
  
  X_kenStone_b <- kenStone(
    NIRsoil$spc,
    k = 50,
    metric = "mahal",
    pc = 3, 
    init = X_kenStone$model[1:10]
  )
  
  expect_true(!any(!sel_samples == X_kenStone_b$model))
})
