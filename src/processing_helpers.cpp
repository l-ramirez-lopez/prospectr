#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' @title bitwise operations 
//' @description
//' bitwise operations  in C++
//' @param a integer 
//' @param b integer
//' @keywords internal
//' @useDynLib prospectr
// [[Rcpp::export]]
int bitAND(int aa, int bb) { 
   return (aa & bb);
}

// [[Rcpp::export]]
int bitSR(int a, int b) { 
   unsigned int c = a;
   return (c >> b);
}

//' @title Convolve 
//' @description
//' Convolution, written in C++
//' @param X matrix to convolve
//' @param f filter
//' @keywords internal
//' @useDynLib prospectr
// [[Rcpp::export]]

NumericMatrix convCppM(NumericMatrix X, NumericVector f) {
  int nX = X.nrow(), pX = X.ncol(), nf = f.size();
  int p = pX - nf + 1;    
  
  NumericMatrix XX(nX,p);
  for(int i = 0; i < nX; i++)
    for (int j = 0; j < p; j++)
      for (int k = 0; k < nf; k++)
        XX(i,j) += X(i,j+k) * f[k];
  
  return XX;
}

// [[Rcpp::export]]
NumericVector convCppV(NumericVector X, NumericVector f) {
  int pX = X.size(), nf = f.size();
  int p = pX - nf + 1;    
  
  NumericVector XX(p);
  
  for (int i = 0; i < p; i++)
    for (int j = 0; j < nf; j++)
      XX[i] += X[i+j] * f[j];
  
  return XX;
}

//' @title A fast distance algorithm for two matrices written in C++ 
//' @usage 
//' fastDist(X,Y,method)
//' @param X a \code{matrix}
//' @param Y a \code{matrix}
//' @param method a \code{string} with possible values "euclid", "cor", "cosine"
//' @return a distance \code{matrix}
//' @keywords internal
//' @useDynLib prospectr
//' @author Antoine Stevens and Leonardo Ramirez-Lopez
// [[Rcpp::export]]   
arma::mat fastDist(NumericMatrix X, NumericMatrix Y, String method){  
  int nX = X.nrow(), kX = X.ncol(), nY = Y.nrow(), kY = Y.ncol();
  arma::mat XX(X.begin(), nX, kX, false); // reuses memory and avoids extra copy
  arma::mat YY(Y.begin(), nY, kY, false); // reuses memory and avoids extra copy 
  if(method=="euclid"){
    arma::mat output = arma::ones(nY,1) * arma::sum(arma::square(XX),1).t() + arma::sum(arma::square(YY),1)  * arma::ones(1,nX) - 2 * YY * XX.t();
    return output;
  }   
  if(method=="cor"){
    arma::mat output = (1 - arma::cor(XX.t(), YY.t()))/2;   
    return output.t();
  }
  else{ // cosine
    arma::mat numerator = XX * YY.t();
    arma::mat dvsr = arma::sqrt(arma::sum(arma::square(XX),1)) * arma::sqrt(arma::sum(arma::square(YY),1)).t();
    arma::mat output = arma::acos(numerator/dvsr);     
    return output.t();
  }   
}

//' @title A fast distance algorithm for a matrix and a vector written in C++ 
//' @usage 
//' fastDistV(X,Y,method)
//' @param X a \code{matrix}
//' @param Y a \code{vector}
//' @param method a \code{string} with possible values "euclid", "cor", "cosine"
//' @return a distance \code{vector}
//' @author Antoine Stevens and Leonardo Ramirez-Lopez
//' @keywords internal 
//' @useDynLib prospectr
// [[Rcpp::export]]   
NumericVector fastDistV(NumericMatrix X, NumericVector Y, String method){  
  int nX = X.nrow(), kX = X.ncol(),  kY = Y.size();
  arma::mat XX(X.begin(), nX, kX, false); // reuses memory and avoids extra copy
  arma::rowvec YY(Y.begin(), kY, false); // reuses memory and avoids extra copy 
  typedef std::vector<double> stdvec;
  if(method=="euclid"){
    stdvec output = arma::conv_to<stdvec>::from(arma::sum(arma::square(XX),1).t() + arma::sum(arma::square(YY))  * arma::ones(1,nX) - 2 * YY * XX.t());
    return wrap(output);
  }   
  if(method=="cor"){
    stdvec output = arma::conv_to<stdvec>::from((1 - arma::cor(XX.t(), YY.t()))/2);   
    return wrap(output);
  }
  else{ //cosine
    arma::mat numerator = XX * YY.t();
    arma::mat dvsr = arma::sqrt(arma::sum(arma::square(XX),1)) * arma::sqrt(arma::sum(arma::square(YY),1)).t();
    stdvec output = arma::conv_to<stdvec>::from(arma::acos(numerator/dvsr));     
    return wrap(output);
  }   
}

//' @title get_msc_coeff
//' @description
//' Coefficients for multiplicative Scatter Correction written in C++
//' @param X matrix 
//' @param reference_spc a matrix of one row and same columns as in X
//' @keywords internal
//' @useDynLib prospectr
// [[Rcpp::export]]
NumericMatrix get_msc_coeff(arma::mat X, 
                            arma::vec reference_spc) {
  
  
  arma::mat ref = arma::ones(X.n_cols, 2);
  ref.col(1) = reference_spc;
  arma::mat aa = trans(ref) * ref;
  arma::mat bb = trans(X * ref);
  
  arma::mat lm = arma::solve(aa, bb);
  
  return Rcpp::wrap(lm);
}

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

// [[Rcpp::export]]
NumericMatrix residLm(NumericMatrix Yr, NumericMatrix Xr) {
  
  int nX = Xr.nrow(), nY = Yr.nrow();
  arma::mat Y(Yr.begin(), nY, nX, false); // reuses memory and avoids extra copy
  arma::mat X(Xr.begin(), nX, Xr.ncol(), false);
  arma::mat resid(nX, nY);
  arma::colvec y;
  for(int i = 0; i < nY; i++){     
    y = Y.row(i).t();
    resid.col(i) = y - X * arma::solve(X, y);    
  }
  
  return wrap(resid.t());
}