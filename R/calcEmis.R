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

.wrap_calcEmis <- function(exp.list.list){
  out <- lapply(exp.list.list, function(exp.list){
    data.frame(
      orig.order = exp.list[["orig.order"]],
      calcEmis(
        ct = exp.list[[3]],
        # Calculate a0 and u0 (f4 transfers done in calcEmis())
        a0 = exp.list[1, "__f0"]*exp.list[1, 4],
        u0 = (1 - exp.list[1, "__f0"])*exp.list[1, 4],
        r1 = exp.list[["__r1"]],
        r2 = exp.list[["__r2"]],
        r3 = exp.list[["__r3"]],
        f4 = exp.list[["__f4"]],
        drop.rows = exp.list[["__drop.row"]]), 
      row.names = NULL, check.names = FALSE)
    })
  do.call(rbind, out)
}
