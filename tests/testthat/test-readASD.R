context("test-readASD")

asd_file <- test_path("testdata/3L9257.000")

# ── error handling ────────────────────────────────────────────────────────────
test_that("readASD errors on a non-existent file", {
  expect_error(
    readASD("does_not_exist.000", in_format = "binary", out_format = "matrix")
  )
})

# ── matrix output (default) ───────────────────────────────────────────────────
test_that("readASD binary returns a matrix by default", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_is(m, "matrix")
})

test_that("readASD matrix has one row per file", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(nrow(m), 1L)
})

test_that("readASD matrix colnames are numeric wavelengths", {
  m   <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  wav <- suppressWarnings(as.numeric(colnames(m)))
  expect_true(!any(is.na(wav)))
  expect_true(length(wav) > 0)
})

test_that("readASD matrix wavelength axis is monotone", {
  m   <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  wav <- as.numeric(colnames(m))
  expect_true(all(diff(wav) > 0) || all(diff(wav) < 0))
})

test_that("readASD matrix spectral values are all finite", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_true(all(is.finite(m)))
})

test_that("readASD matrix reflectance values are in a plausible range", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_true(min(m) >= 0)
  expect_true(max(m) <= 2)
})

# ── list output ───────────────────────────────────────────────────────────────
test_that("readASD binary out_format=list returns a list", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_is(lst, "list")
  expect_equal(length(lst), 1L)
})

test_that("readASD list element contains expected components", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expected <- c("name", "datetime", "header", "reflectance", "wavelength")
  expect_true(all(expected %in% names(lst[[1]])))
})

test_that("readASD list$name matches the filename", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_equal(lst[[1]]$name, "3L9257.000")
})

test_that("readASD list$datetime is POSIXct", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_is(lst[[1]]$datetime, "POSIXct")
})

test_that("readASD list$wavelength is numeric and monotone", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  wav <- lst[[1]]$wavelength
  expect_is(wav, "numeric")
  expect_true(all(diff(wav) > 0) || all(diff(wav) < 0))
})

test_that("readASD list$reflectance length equals wavelength length", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_equal(length(lst[[1]]$reflectance), length(lst[[1]]$wavelength))
})

test_that("readASD list$reflectance is all finite", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_true(all(is.finite(lst[[1]]$reflectance)))
})

test_that("readASD list$header is a list with expected fields", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  hdr <- lst[[1]]$header
  expect_is(hdr, "list")
  expected_hdr <- c(
    "name", "FileVersion", "ProgramVersion", "InstrumentSerialNumber",
    "DataType", "VNIRIntegrationTime", "SWIR1Gain", "SWIR2Gain",
    "Join1Wavelength", "Join2Wavelength"
  )
  expect_true(all(expected_hdr %in% names(hdr)))
})

test_that("readASD list$header$DataType is a known type", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  known <- c("Raw", "Reflectance", "Radiance", "No_Units",
             "Irradiance", "QI", "Transmittance", "Unknown", "Absorbance")
  expect_true(lst[[1]]$header$DataType %in% known)
})

# ── matrix vs list consistency ────────────────────────────────────────────────
test_that("readASD matrix and list reflectance values are identical", {
  m   <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_equal(as.numeric(m[1, ]), lst[[1]]$reflectance, tolerance = 1e-10)
})

test_that("readASD matrix colnames match list wavelengths", {
  m   <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  expect_equal(as.numeric(colnames(m)), lst[[1]]$wavelength, tolerance = 1e-6)
})

# ── multi-file input ──────────────────────────────────────────────────────────
test_that("readASD with two copies of the same file returns 2-row matrix", {
  m <- readASD(c(asd_file, asd_file), in_format = "binary", out_format = "matrix")
  expect_equal(nrow(m), 2L)
  expect_equal(m[1, ], m[2, ], tolerance = 1e-10)
})

test_that("readASD with two files returns list of length 2", {
  lst <- readASD(c(asd_file, asd_file), in_format = "binary", out_format = "list")
  expect_equal(length(lst), 2L)
})

# ── reproducibility ───────────────────────────────────────────────────────────
test_that("readASD returns identical results on repeated calls", {
  m1 <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  m2 <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(m1, m2)
})

# ── numerical regression ──────────────────────────────────────────────────────
test_that("readASD matrix has expected dimensions", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(nrow(m), 1L)
  expect_equal(ncol(m), 2151L)
})

test_that("readASD wavelength axis spans 350 to 2500 nm", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  wav <- lst[[1]]$wavelength
  expect_equal(wav[1],        350)
  expect_equal(wav[length(wav)], 2500)
})
