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

    //Extract group values of inputs
    NumericVector ct(gl), r1(gl), r2(gl), r3(gl), f4(gl), r5(gl), rf(gl), rs(gl), rd(gl);

    //i = relative element index
    for (R_xlen_t i = 0; i < gl; ++i) {
      ct[i] = cta[gs + i];
      r1[i] = r1a[gs + i];
      r2[i] = r2a[gs + i];
      r3[i] = r3a[gs + i];
      f4[i] = f4a[gs + i];
      r5[i] = r5a[gs + i];
      rf[i] = r1[i] + r2[i];
      rs[i] = r3[i] + r5[i];
      rd[i] = rf[i] - rs[i];
    }

    //Number of intervals
    R_xlen_t l = ct.length();
    NumericVector F(l), S(l), E(l), ddt(l);
    
    //time steps
    ddt[0] = ct[0];
    for (R_xlen_t i = 1; i < l; ++i) {
      ddt[i] = ct[i] - ct[i-1];
    }

    //intermediates
    double femis, semis, em;
    double Fti, Sti;
    
    //initial values
    double F0 = F0a[gs];
    double S0 = S0a[gs];
    double Fti0 = F0;
    double Sti0 = S0;
    double Eti = 0.;
    double erdt = 0.;
    
    for (R_xlen_t i = 0; i < l; ++i) {
      // Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and Fti = F[i])
      Fti = f4[i] * Fti0;
      Sti = (1 - f4[i]) * Fti0 + Sti0;
      
      //Calculate intermediates
      //Calculate pools at *end* of ct[i]
      F[i] = Fti * exp(-rf[i] * ddt[i]);
      erdt = exp(-rd[i] * ddt[i]);
      if (erdt > 1.e200) {
        erdt = 1.e200;
      }
      S[i] = exp(-rs[i] * ddt[i]) * (Sti + r2[i] * Fti * (1 - erdt) / rd[i]);
      femis = r1[i] / rf[i] * Fti * (1 - exp(-rf[i] * ddt[i]));
      semis = r3[i] / rs[i] * (Fti + Sti - F[i] - S[i] - femis);
      E[i] = Eti + femis + semis;

      //save pools for next step
      Fti0 = F[i];
      Sti0 = S[i];
      Eti = E[i];

    }

    //Put results in "all" vectors
    //i = relative element index
    for (R_xlen_t i = 0; i < gl; ++i) {
      Fa[gs + i] = F[i];
      Sa[gs + i] = S[i];
      Ea[gs + i] = E[i];
      dt[gs + i] = ddt[i];
    }

  }

  //Return results
  return List::create(_["ct"] = cta, 
                      _["dt"] = dt, 
                      _["f"] = Fa, 
                      _["s"] = Sa, 
                      _["e"] = Ea);
}  
