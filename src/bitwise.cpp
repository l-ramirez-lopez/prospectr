#include <Rcpp.h> 

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