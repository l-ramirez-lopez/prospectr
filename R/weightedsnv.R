

# Determine the best selection of wavelengths  for which one can write: S2 = a.S1 + b0.
# More precisely, for each selected wavelength wl:
# S2(wl) = a.S1(wl) + b0 + epsilon, where  abs(epsilon) is lower than a tolerance threshold.

#Inputs:
# S1, S2: spectra  as line vectors
# tolerance: tolerancethreshold for abs(S2 - a.S1 -b)

#Return values:
# IndexWL:  indexes of selected wavelengths
# a, b: values such that   S2w ~ aS1w + b for  each wl in IndexWL
WLSelection2param<-function(S1, S2, tolerance=1*10^-4, iterations=5000){

  IndexWL<-NA
  a<-NA
  b<-NA
  NbInliers <- 0

  if(length(S1) != length(S2)){
    stop("Spectra have different length !")
  }

  #scanning of every wavelength couple
  for(i in 1:iterations){

    #random draw of 2 wl
    mysample <-  sample(1:length(S1), 2, replace=FALSE)
    index1 <- mysample[1]
    index2 <- mysample[2]

    if(abs(S1[index2]- S1[index1])>=.Machine$double.eps){

      # (a,b) computation for this couple
      al <- (S2[index2] - S2[index1])/(S1[index2] - S1[index1])
      bl <- S2[index1] - al*S1[index1]

      # noumber of inliers ?

      diff <- abs(S2 - al*S1 - bl)

      inliers <- which(diff < tolerance)

      if(length(inliers) > NbInliers){
        # best that current selection
        NbInliers <- length(inliers)
        IndexWL <- inliers
        a <- al
        b <- bl
      }
      rm(al, bl, diff, inliers)
    }
    rm(mysample, index1, index2)
  }
  return(list(IndexWL=IndexWL, a=a, b=b))
}




# Determine the best selection of wavelengths  for which one can write: S2 = a.S1 + b + C*wl.
# More precisely, for each selected wavelength wl:
# S2(wl) = a.S1(wl) + b + c*wl + epsilon, where  abs(epsilon) is lower than a tolerance threshold

#Inputs:
# S1, S2: spectra  as line vectors
# tolerance: tolerancethreshold for abs(S2 - a.S1 -b)

#Return values:
# IndexWL:  indexes of selected wavelengths
# a, b, c: values such that   S2w ~ aS1w + b c*wl for  each wl in IndexWL
WLSelection3param<-function(S1, S2, tolerance=50, iterations=5000){

  IndexWL<-NA
  a<-NA
  b<-NA
  c<-NA
  NbInliers <- 0

  if(length(S1) != length(S2)){
    stop("Spectra have different length !")
  }

  #scanning of every wavelength couple
  for(i in 1:iterations){

    #random draw of 3 wl
    mysample <-  sample(1:length(S1), 3)
    index1 <- mysample[1]
    index2 <- mysample[2]
    index3 <- mysample[3]

    #resolution through matrix inversion:
    Y <- t(as.matrix(c(S2[index1], S2[index2], S2[index3])))
    X <- matrix(0,3,3)
    X[1,] <- c(S1[index1], S1[index2], S1[index3])
    X[2,] <- c(1, 1, 1)
    X[3,] <- c(index1, index2, index3)

    #we have  Y = [a b  c]*X  with a unique solution:
    rk <- qr(t(X)*X)$rank

    if(rk>=3){

      if(class(try(ABC <- Y%*%solve(X, tol=2*10^-25), silent=TRUE))=="matrix"){

        # number of inliers ?

        diff <- abs(S2 - ABC[1,1]*S1 - ABC[1,2] - ABC[1,3]*c(1:length(S1)))

        inliers <- which(diff < tolerance)

        # best that current selection
        if(length(inliers)>NbInliers){

          NbInliers <- length(inliers)
          IndexWL <- inliers
          a <- ABC[1,1]
          b <- ABC[1,2]
          c <- ABC[1,3]

        }
        rm(ABC, diff, inliers)
      }
    }
    rm(mysample, index1, index2, index3, rk, X, Y)
  }
  return(list(IndexWL=IndexWL, a=a, b=b, c=c))
}



# Determine the best selection of wavelengths  for which one can write: S2 = a.S1 + b + C*wl.
# More precisely, for each selected wavelength wl:
# S2(wl) = a.S1(wl) + b + c*wl + epsilon, where  abs(epsilon) is lower than a tolerance threshold

#Inputs:
# S1, S2: spectra  as line vectors
# tolerance: tolerancethreshold for abs(S2 - a.S1 -b)

#Return values:
# IndexWL: indexes of selected wavelengths
# a, b, c: values such that   S2w ~ aS1w + b c*wl for  each wl in IndexWL
WLSelection4param<-function(S1, S2, tolerance=5000, iterations=5000){

  IndexWL<-NA
  a<-NA
  b<-NA
  c<-NA
  d<-NA
  NbInliers <- 0

  if(length(S1) != length(S2)){
    stop("Spectra have different length !")
  }

  #scanning of every wavelength couple
  for(i in 1:iterations){

    #random draw of 4 wl
    mysample <-  sample(1:length(S1), 4)
    index1 <- mysample[1]
    index2 <- mysample[2]
    index3 <- mysample[3]
    index4 <- mysample[4]

    #resolution trough matrix inversion:
    Y <- t(as.matrix(c(S2[index1], S2[index2], S2[index3], S2[index4])))
    X <- matrix(0,4,4)
    X[1,] <- c(S1[index1], S1[index2], S1[index3], S1[index4])
    X[2,] <- c(1, 1, 1, 1)
    X[3,] <- c(index1, index2, index3, index4)
    X[4,] <- c(index1*index1, index2*index2, index3*index3, index4*index4)

    #we have  Y = [a b  c]*X with a unique solution:
    rk<-qr(t(X)*X)$rank

    if(rk>=4){

      if(class(try(ABCD <- Y%*%solve(X, tol=2*10^-25), silent=TRUE))=="matrix"){

        # number of inliers ?
        diff <- abs(S2 - ABCD[1,1]*S1 - ABCD[1,2] - ABCD[1,3]*c(1:length(S1)) - ABCD[1,4]*(c(1:length(S1))*c(1:length(S1))))
        inliers <- which(diff < tolerance)

        if(length(inliers)>NbInliers){
          # best that current selection:
          NbInliers <- length(inliers)
          IndexWL <- inliers
          a <- ABCD[1,1]
          b <- ABCD[1,2]
          c <- ABCD[1,3]
          d <- ABCD[1,4]
        }
        rm(ABCD, diff, inliers)
      }
    }
    rm(mysample, index1, index2, index3, index4, rk, X, Y)
  }
  return(list(IndexWL=IndexWL, a=a, b=b, c=c, d=d))
}



#General Weight computing function:
computeWeights<-function(x, tol, nbParam, nbIter, progress){

  N <- nrow(x)
  p <- ncol(x)

  Nc <- N*(N-1)/2
  Ponderation<-rep(NA, p)

  if(nbParam<2 || nbParam>4){
    stop("Number of parameters must be 2, 3 or 4 !")
  }

  TotalArray <- matrix(0, nrow=Nc, ncol=p)
  LineIndex <- 1

  cl<-makeCluster(detectCores()-1)
  registerDoParallel(cl)

  for(i in 1:(N-1)){

    temp<-foreach(j=(i+1):N, .packages="prospectr") %dopar% {
    #for(j in (i+1):N){

      do.call(paste0("WLSelection",nbParam,"param"), list(S1=as.numeric(x[i,]),
                                                               S2=as.numeric(x[j,]),
                                                               tolerance=tol,
                                                               iterations=nbIter))

      # TotalArray[LineIndex, res$IndexWL] <- 1
      # LineIndex <- LineIndex + 1
      # rm(res)

    }

    tempArray<-matrix(0, nrow=length(temp), ncol=p)

    for(k in 1:length(temp)){tempArray[, temp[[k]]$IndexWL] <- 1}

    TotalArray[LineIndex:(LineIndex+length(temp)-1), ]<-tempArray
    LineIndex <- LineIndex + length(temp)
    rm(temp, tempArray, k)

    if(progress){
      cat('\r', paste0("PROGRESS: ", round(i/(N-1)*100,2)," %"))
    }
  }

  stopCluster(cl)

  Ponderation <- colSums(TotalArray,1)
  Ponderation <- Ponderation/Nc

  return(Ponderation)
}


#' Weighted SNV or Variable Sorting for Normalization (VSN)
#'
#' Function used to normalize the spectra and that computes the optimal wavelength weighting by considering
#' all possible spectrum couples (RANSAC algorithm) and summing their results.
#'
#' @param x a \code{matrix} - the spectral matrix (each spectrum is a row).
#' @param tol a \code{numeric(1)} - error tolerance threshold for solving the equations (default value: 0.001).
#' @param nbParam an \code{integer(1)} - number of parameter considered in the equations (can be 2, 3 or 4).
#' @param nbIter an \code{numeric(1)} - number of iteration for the RANSAC algorithm (default value: 5000).
#' @param progress a \code{logical} - if TRUE, algorithm progress is shown in the console.
#'
#' @return a \code{matrix} of corrected spectra.
#'
#' @examples
#' NIRsoil_wsnv<-wsnv(NIRsoil$spc[1:5,])
#'
#' @references
#' Rabatel, G., Marini, F., Walczak, B. & Roger, J.-M. (2020) VSN: Variable sorting for normalization.
#' Journal of Chemometrics 34:e3164. DOI: \url{https://doi.org/10.1002/cem.3164}
#'
#' @author
#' Original MATLAB code: Jean-Michel Roger.
#' Conversion to R & Parallelization: Guillaume Hans (with permission of the author).
#'
#' @seealso \code{\link{snv}} \code{\link{wmsc}}
#'
#' @import foreach
#' @importFrom parallel makeCluster detectCores stopCluster
#'
#' @export
wsnv<-function(x, tol=0.001, nbParam=3, nbIter=5000, progress=FALSE){

  N <- nrow(x)
  p <- ncol(x)

  weights<-computeWeights(x=x, tol=tol, nbParam=nbParam, nbIter=nbIter, progress=progress)

  # for repeating spectra
  un <- t(as.matrix(rep(1, p)))

  sw <- sum(weights) # will be used for re-normalization

  # make a proba
  weights <- weights / sw
  # create weighting matrix
  W <- diag(weights, nrow=length(weights), ncol=length(weights))

  # apply proba
  sp_ponderes <- as.matrix(x) %*% W

  #removing additive effect:
  a <- as.matrix(rowSums(sp_ponderes))
  newspectra <- x - a %*% un

  #normalisation:

  sdev <- as.matrix(apply(as.matrix(newspectra)%*%W, 1, sd))  ## version of the paper
  #sdev = sqrt(diag(spectra*W*spectra'));

  spectresSNV <- newspectra / (sdev %*% un)

  spectresSNV <- spectresSNV / sw


  return(spectresSNV)

}


#' Weighted MSC
#'
#' Function used to normalize the spectra and that computes the optimal wavelength weighting by considering
#' all possible spectrum couples (RANSAC algorithm) and summing their results.
#'
#' @inheritParams wsnv
#' @param degree an \code{integer} - polynomial degree of the baseline
#'
#' @return  a \code{matrix} of corrected spectra.
#'
#' @examples
#' NIRsoil_wmsc<-wmsc(NIRsoil$spc[1:5,])
#'
#' @references
#' Rabatel, G., Marini, F., Walczak, B. & Roger, J.-M. (2020) VSN: Variable sorting for normalization.
#' Journal of Chemometrics 34:e3164. DOI: \url{https://doi.org/10.1002/cem.3164}
#'
#' @author
#' Original MATLAB code: Jean-Michel Roger.
#' Conversion to R & Parallelization: Guillaume Hans (with permission of the author).
#'
#' @seealso \code{\link{msc}} \code{\link{wsnv}}
#'
#' @import doParallel
#'
#' @export
wmsc<-function(x, degree=2, tol=0.001, nbParam=3, nbIter=5000, progress=FALSE){

  n <- nrow(x)
  p <- ncol(x)

  weights<-computeWeights(x=x, tol=tol, nbParam=nbParam, nbIter=nbIter, progress=progress)

  # make a proba
  weights <- weights / sum(weights)

  # create weighting matrix
  W <- diag(weights, nrow=length(weights), ncol=length(weights))

  # regressors matrix
  v<-matrix(NA, ncol=degree+2, nrow=length(weights))
  v[,1]<-mean(x)
  j<-2
  for(i in seq(degree, 0, by=-1)){
      v[,j] <- matrix(1:p, ncol=1)^i
      j<-j+1
  }

  m <- solve(t(v) %*% W %*% v, tol=2*10^-55) %*% t(v) %*% W

  z<-matrix(NA, nrow=n, ncol=p)
  for(i in 1:n){
    abcd <- m %*% matrix(x[i,], ncol=1)
    z[i,] <- (x[i,] - t(abcd[2:length(abcd)]) %*% t(v[,2:length(abcd)])) / abcd[1]
  }

  return(z)

}
