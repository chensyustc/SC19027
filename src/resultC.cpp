#include <Rcpp.h>
using namespace Rcpp;

//' @title Bias of estimated sigma and average model size using Rcpp
//' @description Bias of estimated sigma and average model size using Rcpp
//' @param hsigma the estimated sigma
//' @param hbeta the estimated coefficients
//' @return bias of estimated sigma and average model size
//' @export
// [[Rcpp::export]]
NumericVector resultC(double hsigma, NumericVector hbeta) {
      NumericVector out(2);
      out[0]=hsigma/1-1;
      int n=hbeta.size();
      int k=0;
      for(int i = 0; i < n; ++i) {
            if(hbeta[i]>0)   k=k+1;
       }
      out[1]=k;
     return out;
}