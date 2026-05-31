context("test-puchwein")

test_that("puchwein returns correct output structure and expected selection", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_is(sel, "list")
  expect_true(all(c("model", "test", "pc", "loop.optimal", "leverage") %in% names(sel)))
  expect_false("details" %in% names(sel))

  sel_samples <- c(
     20,  39,  41,  56, 105, 122, 141, 178, 186, 204, 205, 225, 238, 242,
    247, 248, 251, 254, 260, 266, 276, 279, 282, 286, 287, 294, 305, 312,
    313, 328, 330, 342, 345, 358, 391, 410, 438, 448, 455, 466, 484, 486,
    526, 534, 572, 574, 578, 594, 606, 608, 609, 611, 613, 614, 615, 618,
    619, 638, 648, 666, 669, 701, 702, 706, 709, 715, 732, 734, 736, 737,
    779, 788, 789, 793, 800, 803, 819, 822, 825
  )
  expect_equal(length(sel$model), 79)
  expect_equal(sort(sel$model), sel_samples)
})

test_that("puchwein model and test cover all samples", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_equal(sort(c(sel$model, sel$test)), seq_len(nrow(NIRsoil$spc)))
  expect_equal(length(intersect(sel$model, sel$test)), 0)
})

test_that("puchwein model indices are valid and unique", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_true(all(sel$model >= 1))
  expect_true(all(sel$model <= nrow(NIRsoil$spc)))
  expect_equal(length(unique(sel$model)), length(sel$model))
})

test_that("puchwein pc scores have correct dimensions", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_equal(nrow(sel$pc), nrow(NIRsoil$spc))
  expect_true(ncol(sel$pc) >= 1)
})

test_that("puchwein leverage data frame has correct structure", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_is(sel$leverage, "data.frame")
  expect_true(all(c("loop", "removed", "obs", "theor", "diff") %in% names(sel$leverage)))
  expect_true(sel$loop.optimal >= 1)
  expect_true(sel$loop.optimal <= nrow(sel$leverage))
})

test_that("puchwein with details = TRUE includes details component", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99, details = TRUE)

  expect_is(sel, "list")
  expect_true("details" %in% names(sel))
  expect_is(sel$details, "list")
})

test_that("puchwein works with integer pc", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = 5)

  expect_is(sel, "list")
  expect_equal(ncol(sel$pc), 5)
  expect_equal(length(sel$model), 238)
})

test_that("puchwein works with data.frame input", {
  data("NIRsoil")

  sel <- puchwein(as.data.frame(NIRsoil$spc), k = 0.2, pc = .99)

  expect_is(sel, "list")
  expect_true(length(sel$model) > 0)
})

test_that("puchwein larger k selects fewer samples", {
  data("NIRsoil")

  sel_small_k <- puchwein(NIRsoil$spc, k = 0.1, pc = .99)
  sel_large_k <- puchwein(NIRsoil$spc, k = 0.5, pc = .99)

  expect_true(length(sel_large_k$model) <= length(sel_small_k$model))
})

test_that("puchwein errors when X has only one column", {
  data("NIRsoil")
  expect_error(puchwein(NIRsoil$spc[, 1, drop = FALSE], k = 0.2, pc = .99))
})

test_that("puchwein errors when min.sel >= nrow(X)", {
  data("NIRsoil")
  expect_error(puchwein(NIRsoil$spc, k = 0.2, pc = .99, min.sel = nrow(NIRsoil$spc)))
})
