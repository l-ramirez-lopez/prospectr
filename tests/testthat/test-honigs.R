context("test-honigs")

test_that("honigs works with type = 'A' (absorbance)", {
  data("NIRsoil")

  X_honigs <- honigs(NIRsoil$spc, k = 30, type = "A")

  expect_is(X_honigs, "list")
  expect_true(all(c("model", "test", "bands") %in% names(X_honigs)))

  sel_samples <- c(
    410, 619, 386, 141, 618, 687, 377, 37, 789, 592, 431, 402,
    572, 333, 186, 519, 710, 610, 342, 282, 330, 290, 614, 279,
    315, 284, 248, 147, 749, 823
  )
  expect_true(!any(!sel_samples == X_honigs$model))
})

test_that("honigs model and test are complementary partitions", {
  data("NIRsoil")

  X_honigs <- honigs(NIRsoil$spc, k = 30, type = "A")

  expect_equal(length(X_honigs$model), 30)
  expect_equal(
    length(X_honigs$model) + length(X_honigs$test),
    nrow(NIRsoil$spc)
  )
  expect_equal(length(intersect(X_honigs$model, X_honigs$test)), 0)
})

test_that("honigs model indices are valid and unique", {
  data("NIRsoil")

  X_honigs <- honigs(NIRsoil$spc, k = 30, type = "A")

  expect_equal(length(unique(X_honigs$model)), 30)
  expect_true(all(X_honigs$model >= 1))
  expect_true(all(X_honigs$model <= nrow(NIRsoil$spc)))
})

test_that("honigs works with type = 'R' (reflectance)", {
  data("NIRsoil")

  X_honigs_R <- honigs(NIRsoil$spc, k = 20, type = "R")

  expect_is(X_honigs_R, "list")
  expect_equal(length(X_honigs_R$model), 20)
  expect_true(all(X_honigs_R$model >= 1))
  expect_true(all(X_honigs_R$model <= nrow(NIRsoil$spc)))
})

test_that("honigs type A and R select different samples", {
  data("NIRsoil")

  X_A <- honigs(NIRsoil$spc, k = 20, type = "A")
  X_R <- honigs(NIRsoil$spc, k = 20, type = "R")

  expect_false(isTRUE(all.equal(sort(X_A$model), sort(X_R$model))))
})
