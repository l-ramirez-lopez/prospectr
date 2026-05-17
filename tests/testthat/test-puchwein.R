context("test-puchwein")

test_that("puchwein returns correct output structure", {
  data("NIRsoil")

  sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)

  expect_is(sel, "list")
  expect_true(all(c("model", "test", "pc", "loop.optimal", "leverage") %in% names(sel)))
  expect_false("details" %in% names(sel))
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
  expect_true(length(sel$model) > 0)
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
