#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_calcEmis(const NumericVector cta, const NumericVector F0a, 
                   const NumericVector S0a, const NumericVector r1a, 
                   const NumericVector r2a, const NumericVector r3a, 
                   const NumericVector f4a, const NumericVector r5a,
                   const IntegerVector gstart, const IntegerVector gend) {

  //Suffix "a" is for "all", e.g., cta has values for *all* groups, ct is for 1 group
  //Empty output vectors
  R_xlen_t nobs = cta.length();
  NumericVector Fa(nobs), Sa(nobs), Ea(nobs), dt(nobs);

  //Group loop
  R_xlen_t ngrp = gstart.length(); // ngrp = number of groups

  //Loop through groups, g = group
  for (R_xlen_t g = 0; g < ngrp; ++g) {
    //Group g indices
    R_xlen_t gs = gstart[g];
    R_xlen_t ge = gend[g];
    R_xlen_t gl = ge - gs + 1;

    //initial values
    double F0 = F0a[gs];
    double S0 = S0a[gs];
    double Fti0 = F0;
    double Sti0 = S0;
    double ct0 = 0.;
    double Eti = 0.;
    double erdt = 0.;
    
    //intermediates
    double femis, semis;
    double Fti, Sti;
    double F, S, E;
    double r1, r2, r3, f4, r5, rf, rs, rd;
    double ddt;
    
    for (R_xlen_t i = 0; i < gl; ++i) {
      ddt = cta[gs + i] - ct0;
      r1 = r1a[gs + i];
      r2 = r2a[gs + i];
      r3 = r3a[gs + i];
      f4 = f4a[gs + i];
      rf = r1 + r2;
      rs = r3 + r5a[gs + i];
      rd = rf - rs;
      // Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and Fti = F)
      Fti = f4 * Fti0;
      Sti = (1 - f4) * Fti0 + Sti0;
      
      //Calculate intermediates
      //Calculate pools at *end* of ct[i]
      F = Fti * exp(-rf * ddt);
      erdt = exp(-rd * ddt);
      if (erdt > 1.e200) {
        erdt = 1.e200;
      }
      S = exp(-rs * ddt) * (Sti + r2 * Fti * (1 - erdt) / rd);
      femis = r1 / rf * Fti * (1 - exp(-rf * ddt));
      semis = r3 / rs * (Fti + Sti - F - S - femis);
      E = Eti + femis + semis;

      //save pools for next step
      Fti0 = F;
      Sti0 = S;
      Eti = E;
      ct0 = cta[gs + i];

      //Put results in "all" vectors
      //i = relative element index
      Fa[gs + i] = F;
      Sa[gs + i] = S;
      Ea[gs + i] = E;
      dt[gs + i] = ddt;
    }

  }

  //Return results
  return List::create(_["ct"] = cta, 
                      _["dt"] = dt, 
                      _["f"] = Fa, 
                      _["s"] = Sa, 
                      _["e"] = Ea);
}  
