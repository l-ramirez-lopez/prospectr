#' @title Read ASD FieldSpec Pro binary and ASCII files
#' @description
#' Read single or multiple binary and ASCII files acquired with an ASD FieldSpec Pro (\href{http://www.asdi.com/}{ASDi}, Boulder, CO) spectroradiometer
#' @usage
#' readASD(fnames,in_format,out_format)
#' @param fnames character \code{vector} of the name(s) (with absolute path) of the file(s) to read
#' @param in_format format of the input file: \code{'binary'} or \code{'txt'}
#' @param out_format format of the output: \code{'matrix'} (default) or \code{'list'} (see below)
#' @return 
#' if \code{out_format} = \code{'matrix'}, reflectance values of the input file(s) in a single \code{matrix}.
#' 
#' if \code{out_format} = \code{'list'}, a \code{list} of the input file(s) data consisting of a \code{list} with components:
#' \itemize{
#'  \item{\code{Name}}{ name of the file imported}
#'  \item{\code{datetime}}{ date and time of acquisition in \code{POSIXct} format}
#'  \item{\code{header}}{ \code{list} with information from the header file}
#'  \item{\code{radiance}}{ if applicable, a numeric \code{vector} of radiance values}
#'  \item{\code{reference}}{ if applicable, a numeric \code{vector} of radiance values of the white reference}
#'  \item{\code{reflectance}}{ numeric \code{vector} of reflectance values}
#'  \item{\code{wavelength}}{ numeric \code{vector} of the band positions}
#' } 
#' @author Antoine Stevens (\R port) and Iain Robinson (matlab function)
#' @references 
#' \url{http://fsf.nerc.ac.uk/user_group/user_group.shtml} 
#' 
#' \url{http://www.mathworks.com/matlabcentral/fileexchange/31547}
#' 
#' Indico Version 8 file format (\url{http://support.asdi.com/Document/Documents.aspx})
#' @note 
#' The is a \R port of the \file{importasd.m} function from the \sQuote{FSFPostProcessing} Matlab toolbox by Iain Robinson
#' (University of Edinburgh), which is based on some Java code provided by Andreas Hunei (University of Zurich)
#' 
#' It seems that ASD file format has changed quite a lot with file versions. The function will possibly not work as expected for
#' all versions. Please report any bugs to the package maintainer.
#' @export
#'
readASD <- function(fnames, in_format = c("binary", "txt"), out_format = c("matrix","list")) {
    
    format <- match.arg(in_format)
    out_format <- match.arg(out_format)
    
    spc <- vector("list", length(fnames))
    i <- 1
    for (f in fnames) {
        
        Name <- sub(".+/(.+)", "\\1", f)  # retrieve name of the file without path
        
        if (format == "binary") {
            # open a connection
            con <- file(f, "rb")
            # Retrieve comments
            seek(con, 3)
            Comments <- paste(readBin(con, "character", n = 157), collapse = "")
            
            # Spectrum acquisition time
            tmp <- readBin(con, "integer", size = 2, n = 6)
            DateTime <- ISOdatetime(tmp[6] + 1900, tmp[5] + 1, tmp[4], tmp[3], tmp[2], tmp[1])  # Format time
            
            # Program and file version
            seek(con, 178)
            ProgramVersion <- readBin(con, "integer", size = 1)
            ProgramVersion <- paste(bitSR(ProgramVersion, 4), bitAND(ProgramVersion, 7), sep = ".")  # The major version number is in the upper nibble, the minor version number is in the lower nibble.
            FileVersion <- readBin(con, "integer", size = 1)  # idem for file version
            FileVersion <- paste(bitSR(FileVersion, 4), bitAND(FileVersion, 7), sep = ".")
            
            # Read the VNIR dark subtraction field.
            seek(con, 181)
            DC <- readBin(con, "integer", size = 1)
            if (DC == 1) 
                VNIRDarkSubtraction <- T 
            else if (DC == 0) 
                VNIRDarkSubtraction <- F
            else
                VNIRDarkSubtraction <- NA
            
            # Read the dark spectrum datetime. The date and time are represented as the number of seconds since midnight on 1st January 1970.
            DarkMeasurementsDateTime <- as.POSIXct(readBin(con, "integer", size = 4), origin = "1970-01-01")
            
            # Read the spectrum data type. The type code is in range 0-8.
            DataType <- c("Raw", "Reflectance", "Radiance", "No_Units", "Irradiance", "QI", "Transmittance", "Unknown", "Absorbance")
            seek(con, 186)
            DataType <- DataType[readBin(con, "integer", size = 1) + 1]
            
            # Read the reference spectrum datetime.
            WhiteReferenceMeasurementsDateTime <- as.POSIXct(readBin(con, "integer", size = 4), origin = "1970-01-01")
            
            # Read GPS data.
            seek(con, 334)
            trueHeading <- readBin(con, "double")
            speed <- readBin(con, "double")
            latitude <- readBin(con, "double")
            longitude <- readBin(con, "double")
            altitude <- readBin(con, "double")
            
            # Read the integration time.
            seek(con, 390)
            VNIRIntegrationTime <- readBin(con, "integer", size = 4)
            VNIRIntegrationTimeUnits <- "ms"
            
            # Read the fore optic information.
            seek(con, 394)
            ForeOptic <- readBin(con, "integer", size = 2)
            
            # Read the dark current correction value.
            seek(con, 396)
            DarkCurrentCorrectionValue <- readBin(con, "integer", size = 2)
            
            # Read the instrument number
            seek(con, 400)
            InstrumentSerialNumber <- as.character(readBin(con, "integer", size = 2))
            
            # Read the warning flags
            seek(con, 421)
            warningFlags <- readBin(con, "integer", n = 4, size = 1)
            if (sum(warningFlags)) 
                warning(paste("There appears to be a warning flag in the file:", f, "\nThis may indicate a problem with one of the detectors caused either by saturation (too much light) or by a failure of thermoelectric cooling."))
            
            # Read averaging information
            seek(con, 425)
            DarkCurrentAveraging <- readBin(con, "integer", size = 2)
            WhiteReferenceAveraging <- readBin(con, "integer", size = 2)
            Averaging <- readBin(con, "integer", size = 2)
            
            # Read the instrument model LS stands for LabSpec, FS for FieldSpec, FR for Full Range
            seek(con, 431)
            instrumentModelLookUpTable <- c("Unknown", "PSII", "LSVNIR", "FSVNIR", "FSFR", "FSNIR", "CHEM", "FSFR Unattended")
            InstrumentModel <- instrumentModelLookUpTable[readBin(con, "integer", size = 1) + 1]
            
            # Read the SWIR detector gain and offset settings.
            seek(con, 436)
            SWIR1Gain <- readBin(con, "integer", size = 2)
            SWIR2Gain <- readBin(con, "integer", size = 2)
            SWIR1Offset <- readBin(con, "integer", size = 2)
            SWIR2Offset <- readBin(con, "integer", size = 2)
            
            # Read the detector join wavelengths.
            seek(con, 444)
            Join1Wavelength <- readBin(con, "double", size = 4)
            Join1WavelengthUnits <- "nm"
            Join2Wavelength <- readBin(con, "double", size = 4)
            Join2WavelengthUnits <- "nm"
            
            # Read the smart detector data
            seek(con, 452)
            smartDetectorData <- readBin(con, "double", size = 4, n = 8)
            if (sum(smartDetectorData)) 
                warning(paste("There appears to be data from a smart detector in the file:", f, "\nThis function does not support importing smart detector data"))
            
            # Read spectra data. First reads some relevent information from the file header, then builds the wavelength scale and reads the
            # spectrum data values. If a reference spectrum is also present it will read that too.
            
            # Read the number of channels on the detector.
            seek(con, 204)
            numberOfChannels <- readBin(con, "integer", size = 2)
            
            # Read the wavelength information.
            seek(con, 191)
            start <- readBin(con, "double", size = 4)  # The first wavelengt
            seek(con, 195)
            step <- readBin(con, "double", size = 4)  # The interval between wavelengths.
            end <- start + numberOfChannels * step - 1  # Calculate the last wavelengt
            
            # Build the wavelength scale
            wavelength <- seq(start, end, step)
            
            # Read the data format
            seek(con, 199)
            dataFormatCode <- readBin(con, "integer", size = 1)  # In range 0 to 3.
            dataFormat <- c("numeric", "integer", "double", "Unknwown")[dataFormatCode + 1]  # Format for arg in readBin
            
            # Read the instrument's dynamic range. This will be used for some basic validation.
            seek(con, 418)
            instrumentDynamicRange <- readBin(con, "integer", size = 2)
            
            # Read the target spectrum.  The 'Indico Version 8 File Format' document specifies that the spectrum starts at byte 485. However
            # it appears to actually start at byte 484.
            seek(con, 484)
            # The file format appears to have changed with file version and even file pre-processing (raw and ref)
            # The following code guess the size argument based on the number of channels it should retrieve
            if(length(readBin(con, dataFormat, n = numberOfChannels)) != numberOfChannels){
               seek(con, 484)            
               data <- readBin(con, dataFormat, n = numberOfChannels, size = 4)  
            } else {
               seek(con, 484)
               data <- readBin(con, dataFormat, n = numberOfChannels)
            }
            
            # If any target spectrum data values lie outside the dynamic range of the instrument this probably indicates that something has
            # gone wrong. This could be due to an incorrect offset or data type when reading the binary file.
            if (any(abs(data) > 2^instrumentDynamicRange)) 
                warning(paste("It appears that the spectrum data from the file:", f, "\nwere not read correctly."))
            
            # Normalize the target spectrum
            normalizedData <- data
            if (DataType == "Raw") {
                normalizedData[wavelength <= Join1Wavelength] <- data[wavelength <= Join1Wavelength]/VNIRIntegrationTime
                normalizedData[wavelength > Join1Wavelength & wavelength <= Join2Wavelength] <- data[wavelength > Join1Wavelength & wavelength <= Join2Wavelength] * SWIR1Gain/2048
                normalizedData[wavelength > Join2Wavelength] <- data[wavelength > Join2Wavelength] * SWIR2Gain/2048
            }
            # Read the reference spectrum.
            referenceFlag <- readBin(con, "integer", n = 2, size = 1)
            # The 'Indico Version 8 File Format' documents the reference and spectrum times as well as the spectrum description. However it
            # is not clear what these data are, or how they are formatted. They are not used here.
            referenceTime <- readBin(con, "integer", n = 8, size = 1)
            spectrumTime <- readBin(con, "integer", n = 8, size = 1)
            descriptionLength <- readBin(con, "integer", size = 2)
            if (as.numeric(FileVersion) < 6) 
              referenceData <- readBin(con, dataFormat, n = numberOfChannels, size = 4) 
            else if (as.numeric(FileVersion) < 7) 
              referenceData <- readBin(con, dataFormat, n = numberOfChannels)
            else # it seems that for version > 7 the two first data points are wrong!
              referenceData <- readBin(con, dataFormat, n = numberOfChannels+2)[-c(1:2)]
            
            # Normalize the reference spectrum
            normalizedReferenceData <- referenceData
            if (DataType == "Raw") {
              normalizedReferenceData[wavelength <= Join1Wavelength] <- referenceData[wavelength <= Join1Wavelength]/VNIRIntegrationTime
              normalizedReferenceData[wavelength > Join1Wavelength & wavelength <= Join2Wavelength] <- referenceData[wavelength > 
                Join1Wavelength & wavelength <= Join2Wavelength] * SWIR1Gain/2048
              normalizedReferenceData[wavelength > Join2Wavelength] <- referenceData[wavelength > Join2Wavelength] * SWIR2Gain/2048
            }
            # Collect reference data into a list
            reference <- normalizedReferenceData
           
            # Collect header information into a list
            H <- list(name = Name, Comments = Comments, ProgramVersion = ProgramVersion, FileVersion = FileVersion, InstrumentSerialNumber = InstrumentSerialNumber, 
                DataType = DataType, GPS = list(latitude = latitude, longitude = longitude, altitude = altitude), VNIRIntegrationTime = VNIRIntegrationTime, 
                VNIRIntegrationTimeUnits = VNIRIntegrationTimeUnits, ForeOptic = ForeOptic, VNIRDarkSubtraction = VNIRDarkSubtraction, 
                DarkMeasurementsDateTime = DarkMeasurementsDateTime, DarkCurrentCorrectionValue = DarkCurrentCorrectionValue, WhiteReferenceMeasurementsDateTime = WhiteReferenceMeasurementsDateTime, 
                DarkCurrentAveraging = DarkCurrentAveraging, WhiteReferenceAveraging = WhiteReferenceAveraging, Averaging = Averaging, 
                SWIR1Gain = SWIR1Gain, SWIR2Gain = SWIR2Gain, SWIR1Offset = SWIR1Offset, SWIR2Offset = SWIR2Offset, Join1Wavelength = Join1Wavelength, 
                Join1WavelengthUnits = Join1WavelengthUnits, Join2Wavelength = Join2Wavelength, Join2WavelengthUnits = Join2WavelengthUnits)
            
            # Collect spectral data and header into a single list
            target <- normalizedData
            # Close connection
            close(con)
        } else {
            # Read the file into a character array.
            fileRaw <- readLines(f)
            pos <- grep("^Wav", fileRaw)  # Position of the spectral data in the string
            reference <- NULL
            # READ DATA
            if ((length(fileRaw) - pos) == 1) 
                stop("Error in file:", f, "\nSpectral data should be organized column-wise")
            sep <- sub(".+([\t;,]).+", "\\1", fileRaw[pos + 1])  # Detect the separator
            data <- read.table(f, sep = sep, skip = max(pos - 1, 1), dec = ".", header = T)
            
            if (pos > 2) {
                # Check if there is a header or not
                # Check that the file is actually an FieldSpec text file.
                if (!any(grepl("ASD spectrum file", fileRaw[1:(pos - 1)]))) 
                  stop(paste("The file:", f, "\nwas not a recognized Analytical Spectral Devices text file."))
                
                # READ THE HEADER
                fhead <- fileRaw[1:(pos - 1)]
                if (any(grepl("Spectrum file is raw data", fhead))) {
                  DataType <- "Raw"
                } else if (any(grepl("Spectrum file is reflectance data",fhead))) {
                  DataType <- "Reflectance"
                } else {
                  DataType <- "Unknown"
                }
                Comments <- fhead[(grep("--------", fhead) + 1)]
                InstrumentSerialNumber <- sub(".+instrument number.+ ([[:digit:]].+)", "\\1", fhead[grep("instrument number", fhead)])  #...serial number is not necessarily numeric as it sometimes contains a forward slash character.
                ProgramVersion <- sub(".+file version = ([[:digit:]]+\\.[[:digit:]]+).+", "\\1", fhead[grep("file version", fhead)])
                FileVersion <- sub(".+Program version = ([[:digit:]]+\\.[[:digit:]]+).+", "\\1", fhead[grep("Program version", 
                  fhead)])
                dateString1 <- sub(".+ ([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+) .+", "\\1-\\2", fhead[grep("Spectrum saved", 
                  fhead)])
                dateString2 <- as.character(as.numeric(sub(".+ ([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+) .+", "\\3", fhead[grep("Spectrum saved", 
                  fhead)])) + 1900)  # Years begin in 1900
                timeString <- sub(".+at ([[:digit:]]+:[[:digit:]]+:[[:digit:]]+)", "\\1", fhead[grep("Spectrum saved", fhead)])
                DateTime <- as.POSIXlt(paste(dateString1, "-", dateString2, " ", timeString, sep = ""), format = "%m-%d-%Y %H:%M:%S")
                Averaging <- as.numeric(sub(".+ ([[:digit:]]+).+", "\\1", fhead[grep("samples per data value", fhead)]))
                VNIRIntegrationTime <- as.numeric(sub(".+ ([[:digit:]]+)", "\\1", fhead[grep("VNIR integration", fhead)]))
                VNIRIntegrationTimeUnits <- "ms"
                SWIR1Gain <- as.numeric(sub("SWIR1 gain was ([[:digit:]]+).+", "\\1", fhead[grep("SWIR1 gain was", fhead)]))
                SWIR1Offset <- as.numeric(sub(".+offset was ([[:digit:]]+)", "\\1", fhead[grep("SWIR1 gain was", fhead)]))
                SWIR2Gain <- as.numeric(sub("SWIR2 gain was ([[:digit:]]+).+", "\\1", fhead[grep("SWIR2 gain was", fhead)]))
                SWIR2Offset <- as.numeric(sub(".+offset was ([[:digit:]]+)", "\\1", fhead[grep("SWIR2 gain was", fhead)]))
                Join1Wavelength <- as.numeric(sub(".+SWIR1 was ([[:digit:]]+).+", "\\1", fhead[grep("SWIR1 was", fhead)]))
                Join1WavelengthUnits <- "nm"
                Join2Wavelength <- as.numeric(sub(".+SWIR2 was ([[:digit:]]+).+", "\\1", fhead[grep("SWIR2 was", fhead)]))
                Join2WavelengthUnits <- "nm"
                if (any(grepl("VNIR dark signal subtracted", fhead))) {
                  VNIRDarkSubtraction <- T
                  DarkCurrentAveraging <- as.numeric(sub("([[:digit:]]+) dark meas.+", "\\1", fhead[grep("dark meas", fhead)]))
                  DarkDateTimeString <- sub(".+dark measurements taken (.+)", "\\1", fhead[grep("dark meas", fhead)])
                  DarkMeasurementsDateTime <- as.POSIXlt(DarkDateTimeString, format = "%a %B %d %H:%M:%S %Y")  #...a very odd way to write the date!
                  DarkCurrentCorrectionValue <- as.numeric(sub("DCC value was ([[:digit:]]+)", "\\1", fhead[grep("DCC value was", 
                    fhead)]))
                } else {
                  VNIRDarkSubtraction <- F
                  DarkCurrentAveraging <- "Not applicable"
                  DarkMeasurementsDateTime <- "Not applicable"
                  DarkCurrentCorrectionValue <- "Not applicable"
                }
                if (any(grepl("Data is compared to a white reference", fhead))) {
                  WhiteReferenceMode <- T
                  WhiteReferenceAveraging <- as.numeric(sub("([[:digit:]]+) white meas.+", "\\1", fhead[grep("white meas", fhead)]))
                  WhiteReferenceDateTimeString <- sub(".+white measurements taken (.+)", "\\1", fhead[grep("white meas", fhead)])
                  WhiteReferenceMeasurementsDateTime <- as.POSIXlt(WhiteReferenceDateTimeString, format = "%a %B %d %H:%M:%S %Y")  #...a very odd way to write the date!
                } else {
                  WhiteReferenceMode <- F
                  WhiteReferenceAveraging <- "Not applicable"
                  WhiteReferenceMeasurementsDateTime <- "Not applicable"
                }
                
                if (any(grep("There was no foreoptic attached", fhead))) {
                  ForeOptic <- "None"
                } else {
                  ForeOptic <- as.numeric(sub("There was a ([[:digit:]]+).+", "\\1", fhead[grep("foreoptic attached", fhead)]))
                }
                
                latitude <- as.numeric(sub(".+([[:digit:]]+)", "\\1", fhead[grep("GPS-Lat", fhead)]))
                longitude <- as.numeric(sub(".+([[:digit:]]+)", "\\1", fhead[grep("GPS-Long", fhead)]))
                altitude <- as.numeric(sub(".+([[:digit:]]+)", "\\1", fhead[grep("GPS-Alt", fhead)]))
                
                # Collect header information into a list
                H <- list(name = Name, Comments = Comments, ProgramVersion = ProgramVersion, FileVersion = FileVersion, InstrumentSerialNumber = InstrumentSerialNumber, 
                  DataType = DataType, GPS = list(latitude = latitude, longitude = longitude, altitude = altitude), VNIRIntegrationTime = VNIRIntegrationTime, 
                  VNIRIntegrationTimeUnits = VNIRIntegrationTimeUnits, ForeOptic = ForeOptic, VNIRDarkSubtraction = VNIRDarkSubtraction, 
                  DarkMeasurementsDateTime = DarkMeasurementsDateTime, DarkCurrentCorrectionValue = DarkCurrentCorrectionValue, WhiteReferenceMode = WhiteReferenceMode, 
                  WhiteReferenceMeasurementsDateTime = WhiteReferenceMeasurementsDateTime, DarkCurrentAveraging = DarkCurrentAveraging, 
                  WhiteReferenceAveraging = WhiteReferenceAveraging, Averaging = Averaging, SWIR1Gain = SWIR1Gain, SWIR2Gain = SWIR2Gain, 
                  SWIR1Offset = SWIR1Offset, SWIR2Offset = SWIR2Offset, Join1Wavelength = Join1Wavelength, Join1WavelengthUnits = Join1WavelengthUnits, 
                  Join2Wavelength = Join2Wavelength, Join2WavelengthUnits = Join2WavelengthUnits)
                
                # NORMALISE DATA if applicable
                # 
                # if DataType != 'Reflectance', data from ASD FieldSpec 3 and FieldSpec Pro spectroradiometers needs to be normalized by the
                # detectors gains or integration times. The normalization method is described in the 'Guidelines for the FSF Post Processing
                # Toolbox' (Matlab).
                normalizedData <- data[, 2]
                if (DataType == "Raw") {
                  normalizedData[wavelength <= Join1Wavelength] <- data[wavelength <= Join1Wavelength]/VNIRIntegrationTime
                  normalizedData[wavelength > Join1Wavelength & wavelength <= Join2Wavelength] <- data[wavelength > Join1Wavelength & 
                    wavelength <= Join2Wavelength] * SWIR1Gain/2048
                  normalizedData[wavelength > Join2Wavelength] <- data[wavelength > Join2Wavelength] * SWIR2Gain/2048
                }
                if (DataType == "Unknown") {
                  # Unknown data type, so issue a warning.
                  warning(paste("The type of data in the file:", f, "\ncould not be identified. The data have been imported, but have not been normalized"))
                }
                # Collect spectral data and header into a single list
                target <- normalizedData
            } else {
                target <- data[, 2]
            }
            wavelength <- data[, 1]
        }
        # Copy data into a list
        if (length(reference)){
            reflectance <- target
            reflectance <- reflectance/reference
            spc[[i]] <- list(name = Name, datetime = DateTime, header = H, radiance = target, reference = reference, reflectance = reflectance, 
                wavelength = wavelength)
        } else {
            if(pos>2)
              spc[[i]] <- list(name = Name, datetime = DateTime, header = H, reflectance = target, reference = "Missing reference spectrum", wavelength = wavelength)
            else
              spc[[i]] <- list(name = Name, reflectance = target, reference = "Missing reference spectrum", wavelength = wavelength)
        }
        i <- i + 1
    }
    names(spc) <- sub(".+/(.+)", "\\1", fnames)
    if(out_format=="matrix"){
      spc <- do.call(rbind,lapply(spc,function(x)x$reflectance))
      colnames(spc) <- wavelength
    }
    return(spc)
} 