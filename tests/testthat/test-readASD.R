context("test-readASD")

asd_file <- test_path("testdata/3L9257.000")

# ── txt fixture helpers ───────────────────────────────────────────────────────

# Minimal txt: one comment line then header → pos = 2 (no full-header branch)
make_asd_txt_simple <- function(sep = "\t") {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c(
    "# comment",
    paste("Wavelength", "Reflectance", sep = sep),
    paste("350", "0.100", sep = sep),
    paste("351", "0.110", sep = sep),
    paste("352", "0.120", sep = sep),
    paste("353", "0.130", sep = sep),
    paste("354", "0.140", sep = sep)
  ), tmp)
  tmp
}

# Full-header txt: pos > 2, satisfies all grep patterns in readASD
make_asd_txt_full <- function(data_type    = "reflectance",
                              has_dark     = FALSE,
                              has_white    = FALSE,
                              has_foreoptic = FALSE) {
  type_line <- switch(
    data_type,
    reflectance = "Spectrum file is reflectance data",
    raw         = "Spectrum file is raw data",
                  "Spectrum file type is undefined"   # unknown
  )

  dark_lines <- if (has_dark) {
    c(
      "VNIR dark signal subtracted",
      "10 dark measurements taken Wed January 15 12:00:00 2020",
      "DCC value was 100"
    )
  } else character(0)

  white_lines <- if (has_white) {
    c(
      "Data is compared to a white reference",
      "5 white measurements taken Wed January 15 12:00:00 2020"
    )
  } else character(0)

  foreoptic_line <- if (has_foreoptic) {
    "There was a 25 degree foreoptic attached"
  } else {
    "There was no foreoptic attached"
  }

  tmp <- tempfile(fileext = ".txt")
  writeLines(c(
    "ASD spectrum file version 3",
    "--------",
    "synthetic test sample",
    type_line,
    "ASD instrument number: 12345",
    "ASD file version = 3.00 ",
    "ASD Program version = 8.00 ",
    "Spectrum saved 01/15/120 at 12:00:00",
    "Data averaged 10 samples per data value",
    "VNIR integration time: 136",
    "SWIR1 gain was 256, offset was 0",
    "SWIR2 gain was 256, offset was 0",
    "Detector join SWIR1 was 1000 nm",
    "Detector join SWIR2 was 1830 nm",
    dark_lines,
    white_lines,
    foreoptic_line,
    "GPS-Lat: 10",
    "GPS-Long: 20",
    "GPS-Alt: 30",
    "Wavelength\tReflectance",
    "350\t0.100",
    "351\t0.110",
    "352\t0.120",
    "353\t0.130",
    "354\t0.140"
  ), tmp)
  tmp
}

# ── error handling ────────────────────────────────────────────────────────────
test_that("readASD errors on a non-existent file", {
  expect_error(
    suppressWarnings(readASD("does_not_exist.000", in_format = "binary", out_format = "matrix"))
  )
})

test_that("readASD txt stops when only one data row follows the header", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(c("# comment", "Wavelength\tReflectance", "350\t0.1"), tmp)
  expect_error(readASD(tmp, in_format = "txt", out_format = "matrix"))
})

test_that("readASD txt stops when pos > 2 but file is not a recognised ASD file", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(c(
    "not an ASD file at all",
    "second header line",
    "third header line",
    "Wavelength\tReflectance",
    "350\t0.1",
    "351\t0.2"
  ), tmp)
  expect_error(readASD(tmp, in_format = "txt", out_format = "matrix"))
})

# ── binary matrix output ──────────────────────────────────────────────────────
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

# ── binary list output ────────────────────────────────────────────────────────
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

# ── binary matrix vs list consistency ────────────────────────────────────────
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

# ── binary multi-file input ───────────────────────────────────────────────────
test_that("readASD with two copies of the same file returns 2-row matrix", {
  m <- readASD(c(asd_file, asd_file), in_format = "binary", out_format = "matrix")
  expect_equal(nrow(m), 2L)
  expect_equal(m[1, ], m[2, ], tolerance = 1e-10)
})

test_that("readASD with two files returns list of length 2", {
  lst <- readASD(c(asd_file, asd_file), in_format = "binary", out_format = "list")
  expect_equal(length(lst), 2L)
})

# ── binary reproducibility ────────────────────────────────────────────────────
test_that("readASD returns identical results on repeated calls", {
  m1 <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  m2 <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(m1, m2)
})

# ── binary numerical regression ───────────────────────────────────────────────
test_that("readASD matrix has expected dimensions", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(nrow(m), 1L)
  expect_equal(ncol(m), 2151L)
})

test_that("readASD wavelength axis spans 350 to 2500 nm", {
  lst <- readASD(asd_file, in_format = "binary", out_format = "list")
  wav <- lst[[1]]$wavelength
  expect_equal(wav[1],           350)
  expect_equal(wav[length(wav)], 2500)
})

test_that("readASD spectral values match reference values", {
  m <- readASD(asd_file, in_format = "binary", out_format = "matrix")
  expect_equal(round(m[1, 1],        8), 0.0268232)
  expect_equal(round(m[1, ncol(m)],  8), 0.3356252)
  expect_equal(round(mean(m[1, ]),   8), 0.2743279)
})

# ── txt simple path (pos <= 2, no header parsed) ──────────────────────────────
test_that("readASD txt simple returns a matrix", {
  f <- make_asd_txt_simple()
  on.exit(unlink(f))
  m <- readASD(f, in_format = "txt", out_format = "matrix")
  expect_is(m, "matrix")
  expect_equal(nrow(m), 1L)
  expect_equal(ncol(m), 5L)
})

test_that("readASD txt simple colnames are the wavelength values", {
  f <- make_asd_txt_simple()
  on.exit(unlink(f))
  m <- readASD(f, in_format = "txt", out_format = "matrix")
  expect_equal(as.numeric(colnames(m)), c(350, 351, 352, 353, 354))
})

test_that("readASD txt simple reflectance values are correct", {
  f <- make_asd_txt_simple()
  on.exit(unlink(f))
  m <- readASD(f, in_format = "txt", out_format = "matrix")
  expect_equal(as.numeric(m[1, ]), c(0.1, 0.11, 0.12, 0.13, 0.14),
               tolerance = 1e-8)
})

test_that("readASD txt simple out_format=list returns expected components", {
  f <- make_asd_txt_simple()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_is(lst, "list")
  expect_equal(length(lst), 1L)
  expect_true(all(c("name", "reflectance", "wavelength") %in% names(lst[[1]])))
  expect_equal(lst[[1]]$reference, "Missing reference spectrum")
})

test_that("readASD txt simple list$reflectance matches matrix row", {
  f <- make_asd_txt_simple()
  on.exit(unlink(f))
  m   <- readASD(f, in_format = "txt", out_format = "matrix")
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(as.numeric(m[1, ]), lst[[1]]$reflectance, tolerance = 1e-10)
})

test_that("readASD txt simple detects semicolon separator", {
  f <- make_asd_txt_simple(sep = ";")
  on.exit(unlink(f))
  m <- readASD(f, in_format = "txt", out_format = "matrix")
  expect_is(m, "matrix")
  expect_equal(ncol(m), 5L)
})

# ── txt full-header path (pos > 2) ────────────────────────────────────────────
test_that("readASD txt full header returns a matrix", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  m <- readASD(f, in_format = "txt", out_format = "matrix")
  expect_is(m, "matrix")
  expect_equal(nrow(m), 1L)
  expect_equal(ncol(m), 5L)
})

test_that("readASD txt full header list has name, header, reflectance, wavelength", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_true(all(c("name", "header", "reflectance", "wavelength") %in%
                    names(lst[[1]])))
})

test_that("readASD txt full header list has datetime", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_true("datetime" %in% names(lst[[1]]))
})

test_that("readASD txt full header has correct DataType", {
  f <- make_asd_txt_full(data_type = "reflectance")
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(lst[[1]]$header$DataType, "Reflectance")
})

test_that("readASD txt full header has expected header fields", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  hdr <- lst[[1]]$header
  expect_true(all(c("FileVersion", "ProgramVersion", "InstrumentSerialNumber",
                    "DataType", "VNIRIntegrationTime", "SWIR1Gain", "SWIR2Gain",
                    "Join1Wavelength", "Join2Wavelength") %in% names(hdr)))
})

test_that("readASD txt full header parses join wavelengths", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(lst[[1]]$header$Join1Wavelength, 1000)
  expect_equal(lst[[1]]$header$Join2Wavelength, 1830)
})

test_that("readASD txt full header VNIRDarkSubtraction=FALSE when no dark lines", {
  f <- make_asd_txt_full(has_dark = FALSE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_false(lst[[1]]$header$VNIRDarkSubtraction)
})

test_that("readASD txt full header VNIRDarkSubtraction=TRUE when dark lines present", {
  f <- make_asd_txt_full(has_dark = TRUE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_true(lst[[1]]$header$VNIRDarkSubtraction)
  expect_equal(lst[[1]]$header$DarkCurrentAveraging, 10)
  expect_equal(lst[[1]]$header$DarkCurrentCorrectionValue, 100)
})

test_that("readASD txt full header WhiteReferenceMode=FALSE when no white ref", {
  f <- make_asd_txt_full(has_white = FALSE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_false(lst[[1]]$header$WhiteReferenceMode)
})

test_that("readASD txt full header WhiteReferenceMode=TRUE when white ref present", {
  f <- make_asd_txt_full(has_white = TRUE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_true(lst[[1]]$header$WhiteReferenceMode)
  expect_equal(lst[[1]]$header$WhiteReferenceAveraging, 5)
})

test_that("readASD txt ForeOptic='None' when no foreoptic", {
  f <- make_asd_txt_full(has_foreoptic = FALSE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(lst[[1]]$header$ForeOptic, "None")
})

test_that("readASD txt ForeOptic is numeric when foreoptic present", {
  f <- make_asd_txt_full(has_foreoptic = TRUE)
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_is(lst[[1]]$header$ForeOptic, "numeric")
  expect_equal(lst[[1]]$header$ForeOptic, 25)
})

test_that("readASD txt unknown DataType triggers a warning", {
  f <- make_asd_txt_full(data_type = "unknown")
  on.exit(unlink(f))
  expect_warning(
    readASD(f, in_format = "txt", out_format = "matrix"),
    "could not be identified"
  )
})

test_that("readASD txt full header matrix/list reflectance values are identical", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  m   <- readASD(f, in_format = "txt", out_format = "matrix")
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(as.numeric(m[1, ]), lst[[1]]$reflectance, tolerance = 1e-10)
})

test_that("readASD txt full header reference is 'Missing reference spectrum'", {
  f <- make_asd_txt_full()
  on.exit(unlink(f))
  lst <- readASD(f, in_format = "txt", out_format = "list")
  expect_equal(lst[[1]]$reference, "Missing reference spectrum")
})
