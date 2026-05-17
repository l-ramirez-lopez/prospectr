context("test-standardNormalVariate")

test_that("standardNormalVariate works on matrix input", {
  data("NIRsoil")

  X_snv <- standardNormalVariate(NIRsoil$spc)

  expect_is(X_snv, "matrix")
  expect_true(round(max(X_snv[1, ]), 5) == 2.63444)
  expect_equal(dim(X_snv), dim(NIRsoil$spc))
})

test_that("standardNormalVariate rows have mean ≈ 0", {
  data("NIRsoil")
  X_snv <- standardNormalVariate(NIRsoil$spc)
  row_means <- rowMeans(X_snv)
  expect_true(all(abs(row_means) < 1e-10))
})

test_that("standardNormalVariate rows have sd ≈ 1", {
  data("NIRsoil")
  X_snv <- standardNormalVariate(NIRsoil$spc)
  row_sds <- apply(X_snv, 1, sd)
  expect_true(all(abs(row_sds - 1) < 1e-10))
})

test_that("standardNormalVariate works on data.frame input", {
  data("NIRsoil")

  X_snv <- standardNormalVariate(as.data.frame(NIRsoil$spc))

  expect_is(X_snv, "matrix")
  expect_equal(dim(X_snv), dim(NIRsoil$spc))
})

test_that("standardNormalVariate errors on non-matrix non-data.frame input", {
  expect_error(standardNormalVariate(1:100))
  expect_error(standardNormalVariate(list(a = 1)))
})

test_that("standardNormalVariate preserves row and column names", {
  data("NIRsoil")
  X_snv <- standardNormalVariate(NIRsoil$spc)
  expect_equal(colnames(X_snv), colnames(NIRsoil$spc))
  expect_equal(rownames(X_snv), rownames(NIRsoil$spc))
})
