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

# ── duplicate property name warning ──────────────────────────────────────────
test_that("read_nircal warns when property names are duplicated", {
  expect_warning(
    read_nircal(nir_file, progress = FALSE, verbose = FALSE),
    "duplicated"
  )
})

# ── return type and structure ─────────────────────────────────────────────────
test_that("read_nircal returns a data.frame", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_is(d, "data.frame")
})

test_that("read_nircal output contains the expected metadata columns", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
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
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_is(d$spc, "matrix")
})

test_that("read_nircal $spc ncol equals nWavenumbers", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(ncol(d$spc), d$nWavenumbers[1])
})

test_that("read_nircal $spc nrow equals number of spectra", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(nrow(d$spc), nrow(d))
})

# ── numeric metadata columns ──────────────────────────────────────────────────
test_that("read_nircal numeric columns are of type numeric", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  numeric_cols <- c(
    "Scans", "resolution", "nWavenumbers",
    "WavenumberSteps", "WavenumberStart"
  )
  for (col in numeric_cols) {
    expect_is(d[[col]], "numeric", info = col)
  }
})

test_that("read_nircal nWavenumbers is a positive integer value", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_true(all(d$nWavenumbers > 0, na.rm = TRUE))
  expect_true(all(d$nWavenumbers == round(d$nWavenumbers), na.rm = TRUE))
})

# ── wavenumber axis ───────────────────────────────────────────────────────────
test_that("read_nircal column names of $spc are numeric wavenumbers", {
  d   <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  wav <- suppressWarnings(as.numeric(colnames(d$spc)))
  expect_true(!any(is.na(wav)))
})

test_that("read_nircal wavenumber axis is monotone", {
  d   <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  wav <- as.numeric(colnames(d$spc))
  expect_true(all(diff(wav) > 0) || all(diff(wav) < 0))
})

# ── spectral values ───────────────────────────────────────────────────────────
test_that("read_nircal spectra contain only finite values", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_true(all(is.finite(d$spc)))
})

test_that("read_nircal spectral values are in a plausible absorbance range", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_true(min(d$spc) > -1)
  expect_true(max(d$spc) <  5)
})

# ── parameter: spectra = FALSE ────────────────────────────────────────────────
test_that("read_nircal spectra=FALSE excludes the spc column", {
  d <- suppressWarnings(read_nircal(nir_file, spectra = FALSE, progress = FALSE, verbose = FALSE))
  expect_false("spc" %in% colnames(d))
})

test_that("read_nircal spectra=FALSE still returns a data.frame with metadata", {
  d <- suppressWarnings(read_nircal(nir_file, spectra = FALSE, progress = FALSE, verbose = FALSE))
  expect_is(d, "data.frame")
  expect_true("ID" %in% colnames(d))
})

# ── parameter: response = FALSE ───────────────────────────────────────────────
test_that("read_nircal response=FALSE produces fewer columns than the full call", {
  full    <- suppressWarnings(read_nircal(nir_file,                   progress = FALSE, verbose = FALSE))
  no_resp <- suppressWarnings(read_nircal(nir_file, response = FALSE, progress = FALSE, verbose = FALSE))
  expect_true(ncol(no_resp) <= ncol(full))
})

# ── parameter: metadata = FALSE ───────────────────────────────────────────────
test_that("read_nircal metadata=FALSE still returns ID and spectra", {
  d <- suppressWarnings(read_nircal(nir_file, metadata = FALSE, progress = FALSE, verbose = FALSE))
  expect_is(d, "data.frame")
  expect_true("ID" %in% colnames(d))
  expect_is(d$spc, "matrix")
})

test_that("read_nircal metadata=FALSE nWavenumbers equals ncol(spc)", {
  d <- suppressWarnings(read_nircal(nir_file, metadata = FALSE, progress = FALSE, verbose = FALSE))
  expect_equal(d$nWavenumbers[1], ncol(d$spc))
})

# ── numerical regression ──────────────────────────────────────────────────────
test_that("read_nircal returns the expected number of samples", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(nrow(d), 20)
})

test_that("read_nircal nWavenumbers equals 1501", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(d$nWavenumbers[1], 1501)
})

test_that("read_nircal wavenumber axis spans 4000 to 10000", {
  d   <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  wav <- as.numeric(colnames(d$spc))
  expect_equal(wav[1],           4000)
  expect_equal(wav[length(wav)], 10000)
})

test_that("read_nircal spectral values match reference values", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(round(d$spc[1, 1],            6), 0.181285)
  expect_equal(round(d$spc[1, ncol(d$spc)], 6), 0.667604)
  expect_equal(round(mean(d$spc[1, ]),       6), 0.524597)
})

test_that("read_nircal response columns are present", {
  d <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expected_resp <- c("K", "Ca", "Mg", "Mn", "Zn", "Cu", "Fe", "N", "B", "P", "S")
  expect_true(all(expected_resp %in% colnames(d)))
})

# ── reproducibility ───────────────────────────────────────────────────────────
test_that("read_nircal returns identical results on repeated calls", {
  d1 <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  d2 <- suppressWarnings(read_nircal(nir_file, progress = FALSE, verbose = FALSE))
  expect_equal(d1$spc, d2$spc)
  expect_equal(d1$ID,  d2$ID)
})

test_that("read_nircal spectra=FALSE and metadata=FALSE give same nrow as full call", {
  full <- suppressWarnings(read_nircal(nir_file,                             progress = FALSE, verbose = FALSE))
  d    <- suppressWarnings(read_nircal(nir_file, spectra = FALSE, metadata = FALSE, progress = FALSE, verbose = FALSE))
  expect_equal(nrow(d), nrow(full))
})
