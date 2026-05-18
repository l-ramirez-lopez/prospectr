context("test-shenkWest")

test_that("shenkWest works with rm.outlier = FALSE", {
  data("NIRsoil")

  X_shenkWest <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)

  expect_is(X_shenkWest, "list")
  expect_true(all(c("model", "test", "pc") %in% names(X_shenkWest)))

  sel_samples <- c(
    112, 200, 309, 690, 297, 452, 345, 608, 225, 595, 63, 732,
    824, 126, 706, 294, 313, 528, 154, 585, 612, 617
  )
  expect_true(!any(!sel_samples == X_shenkWest$model))
})

test_that("shenkWest model indices are valid and unique", {
  data("NIRsoil")

  X_sw <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)

  expect_true(length(X_sw$model) > 0)
  expect_equal(length(unique(X_sw$model)), length(X_sw$model))
  expect_true(all(X_sw$model >= 1))
  expect_true(all(X_sw$model <= nrow(NIRsoil$spc)))
})

test_that("shenkWest test set contains model samples as a subset", {
  data("NIRsoil")

  X_sw <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)

  # test set includes model samples plus their eliminated neighbors
  expect_true(all(X_sw$model %in% X_sw$test))
})

test_that("shenkWest test indices are valid", {
  data("NIRsoil")

  X_sw <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)

  expect_true(all(X_sw$test >= 1))
  expect_true(all(X_sw$test <= nrow(NIRsoil$spc)))
})

test_that("shenkWest larger d.min selects fewer samples", {
  data("NIRsoil")

  X_sw_small <- shenkWest(NIRsoil$spc, pc = .99, d.min = .2, rm.outlier = FALSE)
  X_sw_large <- shenkWest(NIRsoil$spc, pc = .99, d.min = .5, rm.outlier = FALSE)

  expect_true(length(X_sw_large$model) <= length(X_sw_small$model))
})

test_that("shenkWest pc scores matrix has correct number of rows", {
  data("NIRsoil")

  X_sw <- shenkWest(NIRsoil$spc, pc = .99, d.min = .3, rm.outlier = FALSE)

  expect_equal(nrow(X_sw$pc), nrow(NIRsoil$spc))
})

test_that("shenkWest works with integer pc", {
  data("NIRsoil")

  X_sw <- shenkWest(NIRsoil$spc, pc = 5, d.min = .3, rm.outlier = FALSE)

  expect_is(X_sw, "list")
  expect_equal(ncol(X_sw$pc), 5)

  sel_pc5 <- c(656, 222, 600, 214, 199, 488, 219, 468, 275, 288,
               791, 220, 469, 225, 260, 312, 562, 281, 358, 403,
               235, 262, 304, 310, 344, 393, 399, 580, 609,  28,
                44, 337, 424, 501, 593, 635, 737,  39, 101, 167,
               178, 326, 331, 449, 485, 583, 585, 612, 617)
  expect_equal(X_sw$model, sel_pc5)
})
