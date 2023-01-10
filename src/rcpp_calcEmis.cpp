#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_calcEmis(const NumericVector cta, const NumericVector a0a, 
                   const NumericVector u0a, const NumericVector r1a, 
                   const NumericVector r2a, const NumericVector r3a, 
                   const NumericVector f4a, const NumericVector r5a,
                   const IntegerVector gstart, const IntegerVector gend) {

  //Suffix "a" is for "all", e.g., cta has values for *all* groups, ct is for 1 group
  //Empty output vectors
  R_xlen_t nobs = cta.length();
  NumericVector aa(nobs), ua(nobs), ea(nobs), dt(nobs);

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
    for (R_xlen_t i = 0; i < gl; i++) {
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
    NumericVector a(l), u(l), e(l), ddt(l);
    
    //time steps
    ddt[0] = ct[0];
    for (R_xlen_t i = 1; i < l; ++i) {
      ddt[i] = ct[i] - ct[i-1];
    }

    //intermediates
    double femis, semis, em;
    double ati, uti;
    
    //initial values
    double a0 = a0a[gs];
    double u0 = u0a[gs];
    double ati0 = a0;
    double uti0 = u0;
    double eti = 0.;
    
    for (R_xlen_t i = 0; i < l; ++i) {
      // Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and ati = a[i])
      ati = f4[i] * ati0;
      uti = (1 - f4[i]) * ati0 + uti0;
      
      //Calculate intermediates
      //Calculate pools at *end* of ct[i]
      a[i] = ati * exp(-(rf[i]) * ddt[i]);
      u[i] = exp(-rs[i] * ddt[i]) * (uti + r2[i] * (1 - exp(-rd[i] * ddt[i])) / rd[i] * ati);
      femis = r1[i] * (1 - exp(-rf[i] * ddt[i])) / rf[i] * ati;
      em = (ati + uti - a[i] - u[i] - femis) / rs[i];
      semis = r3[i] * em;
      e[i] = eti + femis + semis;

      //save pools for next step
      ati0 = a[i];
      uti0 = u[i];
      eti = e[i];

    }

    //Put results in "all" vectors
    //i = relative element index
    for (R_xlen_t i = 0; i < gl; i++) {
      aa[gs + i] = a[i];
      ua[gs + i] = u[i];
      ea[gs + i] = e[i];
      dt[gs + i] = ddt[i];
    }

  }

  //Return results
  return List::create(_["ct"] = cta, 
                      _["dt"] = dt, 
                      _["f"] = aa, 
                      _["s"] = ua, 
                      _["e"] = ea);
}  
