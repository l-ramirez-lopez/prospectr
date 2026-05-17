context("test-read_nircal")

nir_file <- test_path("testdata/muestras-tejido-foliar_transfer.nir")

# ── error handling ────────────────────────────────────────────────────────────
test_that("read_nircal errors on a non-existent file", {
  expect_error(
    read_nircal("does_not_exist.nir", progress = FALSE, verbose = FALSE)
  )
})

test_that("read_nircal errors on a file that is not a NIRCal file", {
  tmp <- tempfile(fileext = ".nir")
  writeLines("this is not a nircal file", tmp)
  expect_error(
    read_nircal(tmp, progress = FALSE, verbose = FALSE)
  )
  unlink(tmp)
})

# ── return type and structure ─────────────────────────────────────────────────
test_that("read_nircal returns a data.frame", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_is(d, "data.frame")
})

test_that("read_nircal output contains the expected metadata columns", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expected <- c(
    "ID", "GUID", "Scans", "resolution", "nWavenumbers",
    "WavenumberSteps", "WavenumberStart", "Device",
    "Software Version", "Created", "Modified",
    "Creator", "Creator login", "Modified by", "Modifier login",
    "Instrument serial", "Measurement cell", "Option serial",
    "Gain factor", "Gain", "Instrument temperature",
    "Sample temperature", "Comment", "Description"
  )
  expect_true(all(expected %in% colnames(d)))
})

test_that("read_nircal embeds spectra as a matrix in $spc", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_is(d$spc, "matrix")
})

test_that("read_nircal $spc ncol equals nWavenumbers", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_equal(ncol(d$spc), d$nWavenumbers[1])
})

test_that("read_nircal $spc nrow equals number of spectra", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_equal(nrow(d$spc), nrow(d))
})

# ── numeric metadata columns ──────────────────────────────────────────────────
test_that("read_nircal numeric columns are of type numeric", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  numeric_cols <- c(
    "Scans", "resolution", "nWavenumbers",
    "WavenumberSteps", "WavenumberStart"
  )
  for (col in numeric_cols) {
    expect_is(d[[col]], "numeric", info = col)
  }
})

test_that("read_nircal nWavenumbers is a positive integer value", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_true(all(d$nWavenumbers > 0, na.rm = TRUE))
  expect_true(all(d$nWavenumbers == round(d$nWavenumbers), na.rm = TRUE))
})

# ── wavenumber axis ───────────────────────────────────────────────────────────
test_that("read_nircal column names of $spc are numeric wavenumbers", {
  d   <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  wav <- suppressWarnings(as.numeric(colnames(d$spc)))
  expect_true(!any(is.na(wav)))
})

test_that("read_nircal wavenumber axis is monotone", {
  d   <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  wav <- as.numeric(colnames(d$spc))
  expect_true(all(diff(wav) > 0) || all(diff(wav) < 0))
})

# ── spectral values ───────────────────────────────────────────────────────────
test_that("read_nircal spectra contain only finite values", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_true(all(is.finite(d$spc)))
})

test_that("read_nircal spectral values are in a plausible absorbance range", {
  d <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_true(min(d$spc) > -1)
  expect_true(max(d$spc) <  5)
})

# ── parameter: spectra = FALSE ────────────────────────────────────────────────
test_that("read_nircal spectra=FALSE excludes the spc column", {
  d <- read_nircal(nir_file, spectra = FALSE, progress = FALSE, verbose = FALSE)
  expect_false("spc" %in% colnames(d))
})

test_that("read_nircal spectra=FALSE still returns a data.frame with metadata", {
  d <- read_nircal(nir_file, spectra = FALSE, progress = FALSE, verbose = FALSE)
  expect_is(d, "data.frame")
  expect_true("ID" %in% colnames(d))
})

# ── parameter: response = FALSE ───────────────────────────────────────────────
test_that("read_nircal response=FALSE produces fewer columns than the full call", {
  full    <- read_nircal(nir_file,                   progress = FALSE, verbose = FALSE)
  no_resp <- read_nircal(nir_file, response = FALSE, progress = FALSE, verbose = FALSE)
  expect_true(ncol(no_resp) <= ncol(full))
})

# ── parameter: metadata = FALSE ───────────────────────────────────────────────
test_that("read_nircal metadata=FALSE still returns ID and spectra", {
  d <- read_nircal(nir_file, metadata = FALSE, progress = FALSE, verbose = FALSE)
  expect_is(d, "data.frame")
  expect_true("ID" %in% colnames(d))
  expect_is(d$spc, "matrix")
})

test_that("read_nircal metadata=FALSE nWavenumbers equals ncol(spc)", {
  d <- read_nircal(nir_file, metadata = FALSE, progress = FALSE, verbose = FALSE)
  expect_equal(d$nWavenumbers[1], ncol(d$spc))
})

# ── reproducibility ───────────────────────────────────────────────────────────
test_that("read_nircal returns identical results on repeated calls", {
  d1 <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  d2 <- read_nircal(nir_file, progress = FALSE, verbose = FALSE)
  expect_equal(d1$spc, d2$spc)
  expect_equal(d1$ID,  d2$ID)
})

test_that("read_nircal spectra=FALSE and metadata=FALSE give same nrow as full call", {
  full <- read_nircal(nir_file,                                          progress = FALSE, verbose = FALSE)
  d    <- read_nircal(nir_file, spectra = FALSE, metadata = FALSE,       progress = FALSE, verbose = FALSE)
  expect_equal(nrow(d), nrow(full))
})
