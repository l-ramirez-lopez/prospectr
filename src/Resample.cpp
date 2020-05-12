#include <Rcpp.h> 
using namespace Rcpp;

//' @title Resample to given band position and fwhm
//' @description
//' Resample, written in C++
//' @param X matrix to resample
//' @param wav a numeric \code{vector} giving the original band positions
//' @param new_wav a numeric \code{vector} giving the new band positions
//' @param fwhm numeric \code{vector} giving the full width half maximums of the new band positions.
//' @keywords internal
//' @useDynLib prospectr
// [[Rcpp::export]]
NumericMatrix resample_fwhm(NumericMatrix X, NumericVector wav, NumericVector new_wav, NumericVector fwhm) {
  int nX = X.nrow(),  np = new_wav.size();
  NumericMatrix output(nX,np);
  
  for(int j = 0; j < np; j++){
    double sdx = fwhm[j]/(2*sqrt(2*log((double)2)));
    if(new_wav[j] - (3*sdx) >= min(wav)&&new_wav[j] + (3*sdx) <= max(wav)) { // Bad interpolation
      double sdx2 = 2*pow(sdx,2);
      NumericVector dn = exp(-pow(wav-new_wav[j],2)/sdx2);  //gaussian density
      double sumdn = sum(dn);
      for(int i = 0; i < nX; i++){ 
        output(i,j) = sum(dn * X(i,_))/sumdn;
      }
    }
  }
  return output;
}

// [[Rcpp::export]]
NumericVector resample_fwhm_vec(NumericVector X, NumericVector wav, NumericVector new_wav, NumericVector fwhm) {
  int np = new_wav.size();
  NumericVector output(np);
  
  for(int i = 0; i < np; i++){
    double sdx = fwhm[i]/(2*sqrt(2*log((double)2)));
    if(new_wav[i] - (3*sdx) >= min(wav)&&new_wav[i] + (3*sdx) <= max(wav)) { // Bad interpolation
      double sdx2 = 2*pow(sdx,2);
      NumericVector dn = exp(-pow(wav-new_wav[i],2)/sdx2);  //gaussian density
      double sumdn = sum(dn);
      output[i] = sum(dn * X)/sumdn;      
    }
  }
  return output;
}