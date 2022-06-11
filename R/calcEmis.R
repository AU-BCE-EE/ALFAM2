calcEmis <- function(ct, a0, u0, r1, r2, r3, f4, drop.rows) {
  # t is interval length (hr)

  if(is.unsorted(ct)) {
    stop('In calcEmis, ct is not sorted.')
  }

  # call Rcpp function
  out <- as.data.frame(rcpp_calcEmis(ct, a0, 
                                     u0, r1, 
                                     r2, r3, 
                                     f4))

  out <- out[!drop.rows, ]

  # Recalculate ddt, e.int, and j.ave.int in case there were dropped rows
  out$e.int <- diff(c(0, out$e))
  out$j <- out$e.int/out$dt

  return(out)
}
