#include <RcppArmadillo.h>
#include <math.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
//' @title Convolve 
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