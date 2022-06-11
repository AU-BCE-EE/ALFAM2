#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_calcEmis(const NumericVector ct, const double a0, 
                   const double u0, const NumericVector r1, 
                   const NumericVector r2, const NumericVector r3, 
                   const NumericVector f4) {
    //Number of intervals
    R_xlen_t l = ct.length();
    NumericVector a(l), u(l), e(l), ddt(l);
    
    //time steps
    ddt[0] = ct[0] - 0;
    for (R_xlen_t i = 1; i < l; ++i) {
      ddt[i] = ct[i] - ct[i-1];
    }
    
    //initial values
    double ati0 = a0;
    double uti0 = u0;
    double eti = 0;
    double ati, uti;
    
    for (R_xlen_t i = 0; i < l; ++i) {
      // Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and ati = a[i])
      ati = f4[i] * ati0;
      uti = (1 - f4[i]) * ati0 + uti0;
      
      //Calculate pools at *end* of ct[i]
      a[i] = ati * exp(-(r1[i] + r2[i]) * ddt[i]);
      u[i] = exp(-r3[i] * ddt[i]) * (r2[i] * ati * (exp((-r1[i] - r2[i] + r3[i]) * ddt[i]) - 1.0)/(-r1[i] - r2[i] + r3[i]) + uti);
      e[i] = eti + (uti - u[i]) +  (ati - a[i]);
        
      //save pools for next step
        ati0 = a[i];
        uti0 = u[i];
        eti = e[i];
    }
    
    return List::create(_["ct"] = ct, 
                        _["dt"] = ddt,
                        _["f0"] = a0/(u0 + a0),
                        _["r1"] = r1, 
                        _["r2"] = r2,
                        _["r3"] = r3, 
                        _["f4"] = f4, 
                        _["f"] = a, 
                        _["s"] = u, 
                        _["j"] = NA_REAL, 
                        _["e"] = e, 
                        _["e.int"] = NA_REAL, 
                        _["er"] = e/(a0 + u0));
}
