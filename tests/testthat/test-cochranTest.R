context("test-cochranTest")

# Balanced design: 10 groups x 3 replicates, identical variance -> no outlier
make_homogeneous <- function() {
  id <- factor(rep(paste0("G", 1:10), each = 3))
  X  <- matrix(0, nrow = 30, ncol = 5)
  for (g in seq_len(10)) {
    rows      <- which(id == levels(id)[g])
    X[rows, ] <- matrix(rep(c(-0.001, 0, 0.001), 5), nrow = 3)
  }
  colnames(X) <- as.character(seq_len(5))
  list(X = X, id = id)
}

# ── error handling ────────────────────────────────────────────────────────────
test_that("cochranTest errors when id is not a factor", {
  d <- make_homogeneous()
  expect_error(cochranTest(d$X, as.character(d$id)))
})

# ── no-outlier path ───────────────────────────────────────────────────────────
test_that("cochranTest returns a list with X and outliers", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_is(res, "list")
  expect_true(all(c("X", "outliers") %in% names(res)))
})

test_that("cochranTest $X is a data.frame", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_is(res$X, "data.frame")
})

test_that("cochranTest $X has same ncol as input", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_equal(ncol(res$X), ncol(d$X))
})

test_that("cochranTest finds no outliers in homogeneous data", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_equal(length(res$outliers), 0L)
  expect_equal(nrow(res$X), nrow(d$X))
})

test_that("cochranTest $outliers is an integer vector", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_is(res$outliers, "integer")
})

test_that("cochranTest $X row values are unchanged when no outlier removed", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  expect_equal(as.matrix(res$X), d$X, tolerance = 1e-10)
})

# ── fun argument ──────────────────────────────────────────────────────────────
test_that("cochranTest fun='mean' finds no outliers in homogeneous data", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id, fun = "mean")
  expect_equal(length(res$outliers), 0L)
})

test_that("cochranTest fun='PC1' finds no outliers in homogeneous data", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id, fun = "PC1")
  expect_equal(length(res$outliers), 0L)
})

test_that("cochranTest fun='PC2' finds no outliers in homogeneous data", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id, fun = "PC2")
  expect_equal(length(res$outliers), 0L)
})

# ── alpha sensitivity ─────────────────────────────────────────────────────────
test_that("cochranTest alpha=1e-10 finds no outliers in homogeneous data", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id, alpha = 1e-10)
  expect_equal(length(res$outliers), 0L)
})

# ── data.frame input ──────────────────────────────────────────────────────────
test_that("cochranTest accepts data.frame input", {
  d   <- make_homogeneous()
  res <- cochranTest(as.data.frame(d$X), d$id)
  expect_is(res, "list")
  expect_equal(length(res$outliers), 0L)
})

# ── output indices are valid ──────────────────────────────────────────────────
test_that("cochranTest outlier indices are within row range of input", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  if (length(res$outliers) > 0) {
    expect_true(all(res$outliers >= 1))
    expect_true(all(res$outliers <= nrow(d$X)))
  } else {
    succeed()
  }
})

test_that("cochranTest X and outliers together cover all input rows", {
  d   <- make_homogeneous()
  res <- cochranTest(d$X, d$id)
  # retained rows + outlier rows == all input rows
  retained <- as.integer(rownames(res$X))
  expect_equal(sort(c(retained, res$outliers)), seq_len(nrow(d$X)))
})
