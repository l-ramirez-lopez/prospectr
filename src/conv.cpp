#include <Rcpp.h> 
using namespace Rcpp;
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
