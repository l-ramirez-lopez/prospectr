context("test-kenStone")

test_that("kenStone works with Mahalanobis metric", {
  data("NIRsoil")

  X_kenStone <- kenStone(NIRsoil$spc, k = 50, metric = "mahal", pc = 3)

  expect_is(X_kenStone, "list")
  expect_true(all(c("model", "test") %in% names(X_kenStone)))

  sel_samples <- c(
    687, 377, 410, 619, 87, 618, 611, 283, 317, 666, 789, 635,
    147, 822, 285, 313, 737, 803, 819, 383, 823, 204, 591, 252,
    825, 272, 402, 39, 330, 590, 286, 608, 363, 234, 701, 718,
    287, 270, 571, 192, 614, 1, 386, 615, 126, 755, 734, 428,
    466, 426
  )
  expect_true(!any(!sel_samples == X_kenStone$model))

  # model + test covers all samples
  expect_equal(sort(c(X_kenStone$model, X_kenStone$test)), seq_len(nrow(NIRsoil$spc)))
  # correct number of model samples
  expect_equal(length(X_kenStone$model), 50)
})

test_that("kenStone respects pre-specified init samples", {
  data("NIRsoil")

  X_kenStone <- kenStone(NIRsoil$spc, k = 50, metric = "mahal", pc = 3)
  X_kenStone_b <- kenStone(
    NIRsoil$spc,
    k = 50,
    metric = "mahal",
    pc = 3,
    init = X_kenStone$model[1:10]
  )

  sel_samples <- c(
    687, 377, 410, 619, 87, 618, 611, 283, 317, 666, 789, 635,
    147, 822, 285, 313, 737, 803, 819, 383, 823, 204, 591, 252,
    825, 272, 402, 39, 330, 590, 286, 608, 363, 234, 701, 718,
    287, 270, 571, 192, 614, 1, 386, 615, 126, 755, 734, 428,
    466, 426
  )
  expect_true(!any(!sel_samples == X_kenStone_b$model))
})

test_that("kenStone with Mahalanobis on 1 single PC variable", {
  data("NIRsoil")
  expect_true(is.list(kenStone(NIRsoil$spc, k = 3, metric = "mahal", pc = 1)))
})

test_that("kenStone works with Euclidean metric", {
  data("NIRsoil")

  X_ks_euclid <- kenStone(NIRsoil$spc, k = 20, metric = "euclid")

  expect_is(X_ks_euclid, "list")
  expect_equal(length(X_ks_euclid$model), 20)
  expect_equal(sort(c(X_ks_euclid$model, X_ks_euclid$test)), seq_len(nrow(NIRsoil$spc)))
})

test_that("kenStone works with pc as variance proportion (pc < 1)", {
  data("NIRsoil")

  X_ks_pc <- kenStone(NIRsoil$spc, k = 20, metric = "mahal", pc = 0.99)

  expect_is(X_ks_pc, "list")
  expect_equal(length(X_ks_pc$model), 20)
})

test_that("kenStone works with groups", {
  data("NIRsoil")
  x <- NIRsoil$spc
  n_per_group <- 5
  my_groups <- rep(1:floor(nrow(x) / n_per_group), each = n_per_group)
  if (length(my_groups) != nrow(x)) {
    my_groups <- c(rep(nrow(x), nrow(x) - length(my_groups)), my_groups)
  }

  X_kenStone <- kenStone(x,
    k = 30,
    pc = 2,
    group = as.factor(my_groups)
  )

  expect_true(all(diff(X_kenStone$model[1:n_per_group]) == 1))
})

test_that("kenStone model indices are unique and valid", {
  data("NIRsoil")

  X_ks <- kenStone(NIRsoil$spc, k = 30, metric = "mahal", pc = 3)

  expect_equal(length(unique(X_ks$model)), 30)
  expect_true(all(X_ks$model >= 1))
  expect_true(all(X_ks$model <= nrow(NIRsoil$spc)))
})
