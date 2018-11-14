calcEmis <- function(ct, a0, u0, r1, r2, r3, f4, drop.rows) {
  # t is interval length (hr)

  if(is.unsorted(ct)) {
    stop('In calcEmis, ct is not sorted.')
  }

  # Number of intervals
  l <- length(ct)

  # Empty result vectors
  a <- u <- e <- numeric(l)

  # Time step
  ddt <- diff(c(0, ct))

  # Initial values
  ati0 <- a0
  uti0 <- u0
  eti <- 0

  # Then calculate pools at end of ddt[1]
  # ti = at transfer interval start
  # Loop through the periods
  for(i in 1:l) {

    # Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and ati = a[i])
    ati <- f4[i] * ati0
    uti <- (1 - f4[i]) * ati0 + uti0

    # Calculate pools at *end* of ct[i]
    a[i] <- ati*exp(-(r1[i] + r2[i])*ddt[i])
    u[i] <- exp(-r3[i]*ddt[i]) * (r2[i] * ati * (exp((-r1[i]-r2[i]+r3[i]) * ddt[i]) - 1)/(-r1[i]-r2[i]+r3[i]) + uti)
    e[i] <- eti + (uti - u[i]) +  (ati - a[i])

    # save pools for next step
    ati0 <- a[i]
    uti0 <- u[i]
    eti <- e[i]

  }

  # Now combine and drop rows that were added for incorporation in afMod()
  out <- data.frame(ct = ct, dt = NA, 
                    f0 = a0/(u0 + a0), r1 = r1, r2 = r2, r3 = r3, f4 = f4, 
                    f = a, s = u, 
                    j = NA, e = e, e.int = NA, er = e/(a0 + u0))

  out <- out[!drop.rows, ]

  # Recalculate ddt, e.int, and j.ave.int in case there were dropped rows
  out$dt <- diff(c(0, out$ct))
  out$e.int <- diff(c(0, out$e))
  out$j <- out$e.int/out$dt

  return(out)
}
