context("test-duplex")

test_that("duplex works with Mahalanobis metric", {
  data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 30, metric = "mahal", pc = .99)

  expect_is(X_duplex, "list")
  expect_true(all(c("model", "test") %in% names(X_duplex)))

  sel_samples <- c(
    789, 619, 39, 617, 594, 310, 737, 822, 683, 614, 204, 287,
    825, 218, 701, 268, 717, 350, 615, 702, 687, 653, 186, 479,
    421, 282, 178, 728, 254, 613
  )
  expect_true(!any(!sel_samples == X_duplex$model))
})

test_that("duplex selects k samples for both model and test sets", {
  data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 30, metric = "mahal", pc = .99)

  expect_equal(length(X_duplex$model), 30)
  expect_equal(length(X_duplex$test), 30)
})

test_that("duplex model and test sets do not overlap", {
  data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 30, metric = "mahal", pc = .99)

  expect_equal(length(intersect(X_duplex$model, X_duplex$test)), 0)
})

test_that("duplex model indices are valid and unique", {
  data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 20, metric = "mahal", pc = .99)

  expect_equal(length(unique(X_duplex$model)), 20)
  expect_true(all(X_duplex$model >= 1))
  expect_true(all(X_duplex$model <= nrow(NIRsoil$spc)))
})

test_that("duplex test indices are valid and unique", {
  data("NIRsoil")

  X_duplex <- duplex(NIRsoil$spc, k = 20, metric = "mahal", pc = .99)

  expect_equal(length(unique(X_duplex$test)), 20)
  expect_true(all(X_duplex$test >= 1))
  expect_true(all(X_duplex$test <= nrow(NIRsoil$spc)))
})

test_that("duplex works with Euclidean metric", {
  data("NIRsoil")

  X_duplex_euclid <- duplex(NIRsoil$spc, k = 20, metric = "euclid")

  expect_is(X_duplex_euclid, "list")
  expect_equal(length(X_duplex_euclid$model), 20)
  expect_equal(length(X_duplex_euclid$test), 20)

  sel_euclid <- c(410, 279, 824, 570, 633, 291, 455, 338, 618, 141,
                  734, 825, 800, 819, 257, 147, 87, 287, 399, 818)
  expect_equal(X_duplex_euclid$model, sel_euclid)
})

test_that("duplex works with pc as integer", {
  data("NIRsoil")

  X_duplex_pc <- duplex(NIRsoil$spc, k = 20, metric = "mahal", pc = 5)

  expect_is(X_duplex_pc, "list")
  expect_equal(length(X_duplex_pc$model), 20)

  sel_pc5 <- c(386, 377, 410, 619, 186, 617, 578, 592, 697, 723,
               594, 39, 311, 702, 501, 572, 391, 611, 303, 261)
  expect_equal(X_duplex_pc$model, sel_pc5)
})
