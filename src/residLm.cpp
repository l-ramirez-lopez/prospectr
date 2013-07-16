#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix residLm(NumericMatrix Yr, NumericMatrix Xr) {
   
   int nX = Xr.nrow(), nY = Yr.nrow();
   arma::mat Y(Yr.begin(), nY, nX, false); // reuses memory and avoids extra copy
   arma::mat X(Xr.begin(), nX, 2, false);
   arma::mat resid(nX,nY);
   arma::colvec y;
   for(int i = 0; i < nY; i++){     
     y = Y.row(i).t();
     resid.col(i) = y - X*arma::solve(X, y);    
  }
     
   return wrap(resid.t());
}