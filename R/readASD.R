#' @title Read ASD FieldSpec Pro binary and ASCII files
#' @description
#'
#' \ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#maturing'><img src='figures/lifecycle-maturing.svg' alt='Maturing lifecycle'></a>}}{\strong{Maturing}}
#'
#' Read single or multiple binary and ASCII files acquired with an ASD FieldSpec
#' Pro ([ASDi](https://www.malvernpanalytical.com/en/products/product-range/asd-range),
#' Boulder, CO) spectroradiometer
#' @usage
#' readASD(fnames, in_format, out_format)
#' @param fnames a character vector of the name(s) (with absolute path) of the file(s) to read.
#' @param in_format the format of the input file: `'binary'` or `'txt'`.
#' @param out_format the format of the output: 'matrix' (default) or 'list' (see below).
#' @return
#' if `out_format` = `'matrix'`, reflectance values of the input file(s) in a single matrix.
#'
#' if `out_format` = `'list'`, a `list` of the input file(s) data consisting of a list with components:
#' \itemize{
#'  \item{`Name`: name of the file imported}
#'  \item{`datetime`: date and time of acquisition in `POSIXct` format}
#'  \item{`header`: list with information from the header file}
#'  \item{`radiance`: if applicable, a numeric vector of radiance values}
#'  \item{`reference`: if applicable, a numeric vector of radiance values of the white reference}
#'  \item{`reflectance`: numeric vector of reflectance values}
#'  \item{`wavelength`: numeric vector of the band positions}
#' }
#' @author Antoine Stevens (\R port), Iain Robinson (matlab function) & Leonardo Ramirez-Lopez (\R port)
#' @references
#' Robinson, I., and A. MacArthur. 2011. The Field Spectroscopy Facility Post
#' Processing Toolbox User Guide. Post processing spectral data in MATLAB,
#' University of Edinburgh, Edinburgh, UK.
#' @note
#' There is a \R port of the \file{importasd.m} function from the
#' \sQuote{FSFPostProcessing} Matlab toolbox by Iain Robinson
#' (University of Edinburgh), which is based on some Java code provided
#' by Andreas Hunei (University of Zurich).
#'
#' It seems that ASD file format has changed quite a lot with file versions. The
#' function will possibly not work as expected for
#' all versions. Please report any bugs to the package maintainer.
#' @export
#'


## Leo 01.03.2020: According to elaliberte (github user), the spectrum description was not actually read
##                 from the binary file. This prevented the reference data to be imported correctly. To fix the bug
##                 referenceFlag was recoded from readBin(con, "integer", n = 2, size = 1) to
##                 readBin(con, "logical", size = 2) (as suggested by elaliberte). In addition,
##                 spectrumDescription <- readChar(con, nchars = descriptionLength)was added (as suggested by elaliberte)


readASD <- function(fnames, in_format = c("binary", "txt"), out_format = c("matrix", "list")) {
  in_format <- match.arg(in_format)
  out_format <- match.arg(out_format)

  spc <- vector("list", length(fnames))
  i <- 1

  for (f in fnames) {
    filename <- sub(".+/(.+)", "\\1", f) # retrieve name of the file without path

    if (in_format == "binary") {
      # open a connection
      con <- file(f, "rb")
      # Retrieve comments
      seek(con, where = 3, origin = "start", rw = "r")
      Comments <- paste(readBin(con, "character", n = 157), collapse = "")


      # Spectrum acquisition time
      seek(con, where = 182)
      tmp <- readBin(con, "integer", size = 4)

      DateTime <- as.POSIXct(tmp, origin = "1970-01-01")

      # Program and file version
      seek(con, where = 178)
      ProgramVersion <- readBin(con, "integer", size = 1)
      ProgramVersion <- paste(bitSR(ProgramVersion, 4), bitAND(ProgramVersion, 7), sep = ".") # The major version number is in the upper nibble, the minor version number is in the lower nibble.
      FileVersion <- readBin(con, "integer", size = 1) # idem for file version
      FileVersion <- paste(bitSR(FileVersion, 4), bitAND(FileVersion, 7), sep = ".")

      # Read the VNIR dark subtraction field.
      seek(con, where = 181)
      DC <- readBin(con, "integer", size = 1)
      if (DC == 1) {
        VNIRDarkSubtraction <- TRUE
      } else {
        if (DC == 0) {
          NIRDarkSubtraction <- FALSE
        } else {
          VNIRDarkSubtraction <- NA
        }
      }
      # Read the dark spectrum datetime. The date and time are represented as the number of seconds since midnight on 1st
      # January 1970.
      DarkMeasurementsDateTime <- as.POSIXct(readBin(con, "integer", size = 4), origin = "1970-01-01")

      # Read the spectrum data type. The type code is in range 0-8.
      DataType <- c(
        "Raw",
        "Reflectance",
        "Radiance",
        "No_Units",
        "Irradiance",
        "QI",
        "Transmittance",
        "Unknown",
        "Absorbance"
      )

      seek(con, where = 186)
      DataType <- DataType[readBin(con, "integer", size = 1) + 1]

      # Read the reference spectrum datetime.
      WhiteReferenceMeasurementsDateTime <- as.POSIXct(readBin(con, "integer", size = 4), origin = "1970-01-01")

      # Read GPS data.
      seek(con, where = 334)
      trueHeading <- readBin(con, "double")
      speed <- readBin(con, "double")
      latitude <- readBin(con, "double")
      longitude <- readBin(con, "double")
      altitude <- readBin(con, "double")

      # Read the integration time.
      seek(con, where = 390)
      VNIRIntegrationTime <- readBin(con, "integer", size = 4)
      VNIRIntegrationTimeUnits <- "ms"

      # Read the fore optic information.
      seek(con, where = 394)
      ForeOptic <- readBin(con, "integer", size = 2)

      # Read the dark current correction value.
      seek(con, where = 396)
      DarkCurrentCorrectionValue <- readBin(con, "integer", size = 2)

      # Read the instrument number
      seek(con, where = 400)
      InstrumentSerialNumber <- as.character(readBin(con, "integer", size = 2))

      # Read the warning flags
      seek(con, where = 421)
      warningFlags <- readBin(con, "integer", n = 4, size = 1)
      if (sum(warningFlags)) {
        warning(paste("There appears to be a warning flag in the file:", f, "\nThis may indicate a problem with one of the detectors caused either by saturation (too much light) or by a failure of thermoelectric cooling."))
      }

      # Read averaging information
      seek(con, where = 425)
      DarkCurrentAveraging <- readBin(con, "integer", size = 2)
      WhiteReferenceAveraging <- readBin(con, "integer", size = 2)
      Averaging <- readBin(con, "integer", size = 2)

      # Read the instrument model LS stands for LabSpec, FS for FieldSpec, FR for Full Range
      seek(con, where = 431)
      instrumentModelLookUpTable <- c(
        "Unknown", "
                                      PSII",
        "LSVNIR",
        "FSVNIR",
        "FSFR",
        "FSNIR",
        "CHEM",
        "FSFR Unattended"
      )

      InstrumentModel <- instrumentModelLookUpTable[readBin(con, "integer", size = 1) + 1]

      # Read the SWIR detector gain and offset settings.
      seek(con, where = 436)
      SWIR1Gain <- readBin(con, "integer", size = 2)
      SWIR2Gain <- readBin(con, "integer", size = 2)
      SWIR1Offset <- readBin(con, "integer", size = 2)
      SWIR2Offset <- readBin(con, "integer", size = 2)

      # Read the detector join wavelengths.
      seek(con, where = 444)
      Join1Wavelength <- readBin(con, "double", size = 4)
      Join1WavelengthUnits <- "nm"
      Join2Wavelength <- readBin(con, "double", size = 4)
      Join2WavelengthUnits <- "nm"

      # Read the smart detector data
      seek(con, where = 452)
      smartDetectorData <- readBin(con, "double", size = 4, n = 8)
      if (sum(smartDetectorData)) {
        warning(paste("There appears to be data from a smart detector in the file:", f, "\nThis function does not support importing smart detector data"))
      }

      # Read spectra data. First reads some relevent information from the file header, then builds the wavelength scale and
      # reads the spectrum data values. If a reference spectrum is also present it will read that too.

      # Read the number of channels on the detector.
      seek(con, where = 204)
      numberOfChannels <- readBin(con, "integer", size = 2)

      # Read the wavelength information.
      seek(con, where = 191)
      wstart <- readBin(con, "double", size = 4) # The first wavelengt
      seek(con, where = 195)
      wstep <- readBin(con, "double", size = 4) # The interval between wavelengths.
      wend <- wstart + numberOfChannels * wstep - 1 # Calculate the last wavelengt

      # Build the wavelength scale
      wavelength <- seq(wstart, wend, wstep)

      # Read the data format
      seek(con, where = 199)
      dataFormatCode <- readBin(con, "integer", size = 1) # In range 0 to 3.
      dataFormat <- c("numeric", "integer", "double", "Unknwown")[dataFormatCode + 1] # Format for arg in readBin

      # Read the instrument's dynamic range. This will be used for some basic validation.
      seek(con, where = 418)
      instrumentDynamicRange <- readBin(con, "integer", size = 2)

      # Read the target spectrum.  The 'Indico Version 8 File Format' document specifies that the spectrum starts at byte
      # 485. However it appears to actually start at byte 484.
      seek(con, where = 484)
      # The file format appears to have changed with file version and even file pre-processing (raw and ref) The following
      # code guess the size argument based on the number of channels it should retrieve
      sdata <- readBin(con, dataFormat, n = numberOfChannels)
      if (length(sdata) != numberOfChannels) {
        seek(con, where = 484)
        sdata <- readBin(con, dataFormat, n = numberOfChannels, size = 4)
      }

      # If any target spectrum data values lie outside the dynamic range of the instrument this probably indicates that
      # something has gone wrong. This could be due to an incorrect offset or data type when reading the binary file.
      if (any(abs(sdata) > 2^instrumentDynamicRange)) {
        warning(paste("It appears that the spectrum data from the file:", f, "\nwas not properly read."))
      }

      # Normalize the target spectrum
      normalizedData <- sdata
      if (DataType == "Raw") {
        isel1 <- wavelength <= Join1Wavelength
        isel2 <- wavelength > Join1Wavelength & wavelength <= Join2Wavelength
        isel3 <- wavelength > Join2Wavelength

        normalizedData[isel1] <- sdata[isel1] / VNIRIntegrationTime
        normalizedData[isel2] <- sdata[isel2] * SWIR1Gain / 2048
        normalizedData[isel3] <- sdata[isel3] * SWIR2Gain / 2048
      }

      # # Read the reference spectrum.
      # # The 'Indico Version 8 File Format' documents the reference and spectrum times as well as the spectrum description.
      # # However it is not clear what these data are, or how they are formatted. They are not used here.

      seek(con, where = 17692)
      referenceFlag <- readBin(con, "logical", size = 2) ## as suggested by elaliberte
      # The 'Indico Version 8 File Format' documents the reference and spectrum times as well as the spectrum description.
      # However it is not clear what these data are, or how they are formatted. They are not used here.
      seek(con, where = 17702)
      referenceTime <- readBin(con, "integer", size = 8)

      spectrumTime <- readBin(con, "integer", n = 8, size = 1)

      seek(con, where = 17710)
      descriptionLength <- readBin(con, integer(), size = 2, endian = "little")

      ## as suggested by elaliberte
      seek(con, where = 17712)
      spectrumDescription <- readChar(con, nchars = descriptionLength) ## as suggested by elaliberte

      rsize <- ifelse(as.numeric(FileVersion) < 6, 4, 8)
      nrec <- ifelse(as.numeric(FileVersion) > 7, numberOfChannels + 2, numberOfChannels)

      seek(con, where = 17712 + descriptionLength)
      referenceData <- readBin(con, dataFormat, n = nrec, size = rsize)

      if (as.numeric(FileVersion) > 7) {
        # it seems that for version > 7 the two first data points are wrong!
        referenceData <- referenceData[-c(1:2)]
      }

      # Normalize the reference spectrum
      normalizedReferenceData <- referenceData
      if (DataType == "Raw") {
        sel1 <- wavelength <= Join1Wavelength
        sel2 <- wavelength > Join1Wavelength & wavelength <= Join2Wavelength
        sel3 <- wavelength > Join2Wavelength

        normalizedReferenceData[sel1] <- referenceData[sel1] / VNIRIntegrationTime
        normalizedReferenceData[sel2] <- referenceData[sel2] * SWIR1Gain / 2048
        normalizedReferenceData[sel3] <- referenceData[sel3] * SWIR2Gain / 2048
      }

      # Collect reference data into a list
      reference <- normalizedReferenceData

      # Collect header information into a list
      H <- list(
        name = filename,
        Comments = Comments,
        ProgramVersion = ProgramVersion,
        FileVersion = FileVersion,
        InstrumentSerialNumber = InstrumentSerialNumber,
        DataType = DataType,
        GPS = list(latitude = latitude, longitude = longitude, altitude = altitude),
        VNIRIntegrationTime = VNIRIntegrationTime,
        VNIRIntegrationTimeUnits = VNIRIntegrationTimeUnits,
        ForeOptic = ForeOptic,
        VNIRDarkSubtraction = VNIRDarkSubtraction,
        DarkMeasurementsDateTime = DarkMeasurementsDateTime,
        DarkCurrentCorrectionValue = DarkCurrentCorrectionValue,
        WhiteReferenceMeasurementsDateTime = WhiteReferenceMeasurementsDateTime,
        DarkCurrentAveraging = DarkCurrentAveraging,
        WhiteReferenceAveraging = WhiteReferenceAveraging,
        Averaging = Averaging,
        SWIR1Gain = SWIR1Gain,
        SWIR2Gain = SWIR2Gain,
        SWIR1Offset = SWIR1Offset,
        SWIR2Offset = SWIR2Offset,
        Join1Wavelength = Join1Wavelength,
        Join1WavelengthUnits = Join1WavelengthUnits,
        Join2Wavelength = Join2Wavelength,
        Join2WavelengthUnits = Join2WavelengthUnits
      )

      # Collect spectral data and header into a single list
      target <- normalizedData
      # Close connection
      close(con)
    } else {
      # Read the file into a character array.
      fileRaw <- readLines(f)
      pos <- grep("^Wav", fileRaw) # Position of the spectral data in the string
      reference <- NULL

      # READ DATA
      if ((length(fileRaw) - pos) == 1) {
        stop("Error in file:", f, "\nSpectral data should be organized column-wise")
      }

      sep <- sub(".+([\t;,]).+", "\\1", fileRaw[pos + 1]) # Detect the separator
      sdata <- read.table(f, sep = sep, skip = max(pos - 1, 1), dec = ".", header = TRUE)

      if (pos > 2) {
        # Check if there is a header or not Check that the file is actually an FieldSpec text file.
        if (!any(grepl("ASD spectrum file", fileRaw[1:(pos - 1)]))) {
          stop(paste("The file:", f, "\nwas not a recognized Analytical Spectral Devices text file."))
        }

        # READ THE HEADER
        fhead <- fileRaw[1:(pos - 1)]
        if (any(grepl("Spectrum file is raw data", fhead))) {
          DataType <- "Raw"
        } else if (any(grepl("Spectrum file is reflectance data", fhead))) {
          DataType <- "Reflectance"
        } else {
          DataType <- "Unknown"
        }
        Comments <- fhead[(grep("--------", fhead) + 1)]
        # ...serial number is not necessarily numeric as it sometimes contains a forward slash character.
        srchsn <- ".+instrument number.+ ([[:digit:]].+)"
        InstrumentSerialNumber <- sub(srchsn, "\\1", fhead[grep("instrument number", fhead)])

        srchfv <- ".+file version = ([[:digit:]]+\\.[[:digit:]]+).+"
        FileVersion <- sub(srchfv, "\\1", fhead[grep("file version", fhead)])

        srchpv <- ".+Program version = ([[:digit:]]+\\.[[:digit:]]+).+"
        ProgramVersion <- sub(srchpv, "\\1", fhead[grep("Program version", fhead)])

        srchdstr <- ".+ ([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+) .+"
        dateString1 <- sub(srchdstr, "\\1-\\2", fhead[grep("Spectrum saved", fhead)])


        srchdstr2 <- ".+ ([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+) .+"
        dateString2 <- sub(srchdstr2, "\\3", fhead[grep("Spectrum saved", fhead)])
        # Years begin in 1900
        dateString2 <- as.character(as.numeric(dateString2) + 1900)

        srchtmstr <- ".+at ([[:digit:]]+:[[:digit:]]+:[[:digit:]]+)"
        timeString <- sub(srchtmstr, "\\1", fhead[grep("Spectrum saved", fhead)])

        DateTime <- as.POSIXlt(paste(dateString1, "-", dateString2, " ", timeString, sep = ""),
          format = "%m-%d-%Y %H:%M:%S"
        )


        srchavg <- ".+ ([[:digit:]]+).+"
        Averaging <- as.numeric(sub(srchavg, "\\1", fhead[grep("samples per data value", fhead)]))

        srchintt <- ".+ ([[:digit:]]+)"
        VNIRIntegrationTime <- as.numeric(sub(srchintt, "\\1", fhead[grep("VNIR integration", fhead)]))
        VNIRIntegrationTimeUnits <- "ms"

        srchgain1 <- "SWIR1 gain was ([[:digit:]]+).+"
        SWIR1Gain <- as.numeric(sub(srchgain1, "\\1", fhead[grep("SWIR1 gain was", fhead)]))

        srchoff1 <- ".+offset was ([[:digit:]]+)"
        SWIR1Offset <- as.numeric(sub(srchoff1, "\\1", fhead[grep("SWIR1 gain was", fhead)]))

        srchgain2 <- "SWIR2 gain was ([[:digit:]]+).+"
        SWIR2Gain <- as.numeric(sub(srchgain2, "\\1", fhead[grep("SWIR2 gain was", fhead)]))

        srchoff2 <- ".+offset was ([[:digit:]]+)"
        SWIR2Offset <- as.numeric(sub(srchoff2, "\\1", fhead[grep("SWIR2 gain was", fhead)]))

        srchjw1 <- ".+SWIR1 was ([[:digit:]]+).+"
        Join1Wavelength <- as.numeric(sub(srchjw1, "\\1", fhead[grep("SWIR1 was", fhead)]))
        Join1WavelengthUnits <- "nm"

        srchjw2 <- ".+SWIR2 was ([[:digit:]]+).+"
        Join2Wavelength <- as.numeric(sub(srchjw2, "\\1", fhead[grep("SWIR2 was", fhead)]))
        Join2WavelengthUnits <- "nm"


        if (any(grepl("VNIR dark signal subtracted", fhead))) {
          VNIRDarkSubtraction <- TRUE
          srchdrk <- "([[:digit:]]+) dark meas.+"
          DarkCurrentAveraging <- as.numeric(sub(srchdrk, "\\1", fhead[grep("dark meas", fhead)]))

          srchdrktm <- ".+dark measurements taken (.+)"
          DarkDateTimeString <- sub(srchdrktm, "\\1", fhead[grep("dark meas", fhead)])

          # ...a very odd way to write the date!
          DarkMeasurementsDateTime <- as.POSIXlt(DarkDateTimeString,
            format = "%a %B %d %H:%M:%S %Y"
          )

          srchcv <- "DCC value was ([[:digit:]]+)"
          DarkCurrentCorrectionValue <- as.numeric(sub(srchcv, "\\1", fhead[grep("DCC value was", fhead)]))
        } else {
          VNIRDarkSubtraction <- FALSE
          DarkCurrentAveraging <- DarkMeasurementsDateTime <- DarkCurrentCorrectionValue <- "Not applicable"
        }
        if (any(grepl("Data is compared to a white reference", fhead))) {
          WhiteReferenceMode <- TRUE
          srchwra <- "([[:digit:]]+) white meas.+"
          WhiteReferenceAveraging <- as.numeric(sub(srchwra, "\\1", fhead[grep("white meas", fhead)]))

          srchwrdt <- ".+white measurements taken (.+)"
          WhiteReferenceDateTimeString <- sub(srchwrdt, "\\1", fhead[grep("white meas", fhead)])

          # ...a very odd way to write the date!
          WhiteReferenceMeasurementsDateTime <- as.POSIXlt(WhiteReferenceDateTimeString,
            format = "%a %B %d %H:%M:%S %Y"
          )
        } else {
          WhiteReferenceMode <- FALSE
          WhiteReferenceAveraging <- WhiteReferenceMeasurementsDateTime <- "Not applicable"
        }

        if (any(grep("There was no foreoptic attached", fhead))) {
          ForeOptic <- "None"
        } else {
          srchfro <- "There was a ([[:digit:]]+).+"
          ForeOptic <- as.numeric(sub(srchfro, "\\1", fhead[grep("foreoptic attached", fhead)]))
        }

        srchcoor <- ".+([[:digit:]]+)"
        latitude <- as.numeric(sub(srchcoor, "\\1", fhead[grep("GPS-Lat", fhead)]))
        longitude <- as.numeric(sub(srchcoor, "\\1", fhead[grep("GPS-Long", fhead)]))
        altitude <- as.numeric(sub(srchcoor, "\\1", fhead[grep("GPS-Alt", fhead)]))

        # Collect header information into a list
        H <- list(
          name = filename,
          Comments = Comments,
          ProgramVersion = ProgramVersion,
          FileVersion = FileVersion,
          InstrumentSerialNumber = InstrumentSerialNumber,
          DataType = DataType,
          GPS = list(
            latitude = latitude,
            longitude = longitude,
            altitude = altitude
          ),
          VNIRIntegrationTime = VNIRIntegrationTime,
          VNIRIntegrationTimeUnits = VNIRIntegrationTimeUnits,
          ForeOptic = ForeOptic,
          VNIRDarkSubtraction = VNIRDarkSubtraction,
          DarkMeasurementsDateTime = DarkMeasurementsDateTime,
          DarkCurrentCorrectionValue = DarkCurrentCorrectionValue,
          WhiteReferenceMode = WhiteReferenceMode,
          WhiteReferenceMeasurementsDateTime = WhiteReferenceMeasurementsDateTime,
          DarkCurrentAveraging = DarkCurrentAveraging,
          WhiteReferenceAveraging = WhiteReferenceAveraging,
          Averaging = Averaging,
          SWIR1Gain = SWIR1Gain,
          SWIR2Gain = SWIR2Gain,
          SWIR1Offset = SWIR1Offset,
          SWIR2Offset = SWIR2Offset,
          Join1Wavelength = Join1Wavelength,
          Join1WavelengthUnits = Join1WavelengthUnits,
          Join2Wavelength = Join2Wavelength,
          Join2WavelengthUnits = Join2WavelengthUnits
        )

        # NORMALISE DATA if applicable
        #
        # if DataType != 'Reflectance', data from ASD FieldSpec 3 and FieldSpec Pro spectroradiometers needs to be normalized
        # by the detectors gains or integration times. The normalization method is described in the 'Guidelines for the FSF
        # Post Processing Toolbox' (Matlab).
        normalizedData <- sdata[, 2]
        if (DataType == "Raw") {
          sel1 <- wavelength <= Join1Wavelength
          sel2 <- wavelength > Join1Wavelength & wavelength <= Join2Wavelength
          sel3 <- wavelength > Join2Wavelength

          normalizedData[sel1] <- sdata[sel1] / VNIRIntegrationTime
          normalizedData[sel2] <- sdata[sel2] * SWIR1Gain / 2048
          normalizedData[sel3] <- sdata[sel3] * SWIR2Gain / 2048
        }
        if (DataType == "Unknown") {
          # Unknown data type, so issue a warning.
          warning(paste("The type of data in the file:", f, "\ncould not be identified. The data have been imported, but have not been normalized"))
        }
        # Collect spectral data and header into a single list
        target <- normalizedData
      } else {
        target <- sdata[, 2]
      }
      wavelength <- sdata[, 1]
    }

    # Copy data into a list
    if (length(reference)) {
      reflectance <- target
      reflectance <- reflectance / reference
      spc[[i]] <- list(
        name = filename,
        datetime = DateTime,
        header = H,
        radiance = target,
        reference = reference,
        reflectance = reflectance,
        wavelength = wavelength
      )
    } else {
      if (in_format == "txt") {
        if (pos <= 2) {
          spc[[i]] <- list(
            name = filename,
            reflectance = target,
            reference = "Missing reference spectrum",
            wavelength = wavelength
          )
        }
      } else {
        spc[[i]] <- list(
          name = filename,
          datetime = DateTime,
          header = H,
          reflectance = target,
          reference = "Missing reference spectrum",
          wavelength = wavelength
        )
      }
    }
    i <- i + 1
  }
  names(spc) <- sub(".+/(.+)", "\\1", fnames)
  if (out_format == "matrix") {
    spc <- do.call(rbind, lapply(spc, function(x) x$reflectance))
    colnames(spc) <- wavelength
  }
  return(spc)
}
