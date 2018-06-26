calcEmis <- function(ct, a0, u0, r1, r2, r3, f5, drop.rows) {
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
  ati <- a0
  uti <- u0
  eti <- 0

  # Then calculate pools at end of ddt[1]
  # ti = at transfer interval start
  # Loop through the periods
  for(i in 1:l) {

    # Calculate pools at *end* of ct[i]
    if(r2[i] > 0) { # These don't work well for r2 == 0
      a[i] <- ati*exp(-(r1[i] + r2[i])*ddt[i])
      u[i] <- (
                exp(-r3[i]*ddt[i]) *
                (-ati*r2[i] + ati*exp((-r1[i]-r2[i])* ddt[i]+r3[i]*ddt[i])* r2[i] - r1[i]*uti - r2[i]*uti + r3[i]*uti)
              )/(-r1[i]-r2[i]+r3[i]) 
      e[i] <- eti + (uti - u[i]) +  (ati - a[i])
    } else {
      a[i] <- ati*exp(-r1[i]*ddt[i])
      u[i] <- uti*exp(-r3[i]*ddt[i])
      e[i] <- eti + (uti - u[i]) +  (ati - a[i])
    }

    # Make incorporation transfer (at *end* of interval) (otherwise f5 = 0 and ati = a[i])
    ati <- f5[i]*a[i]
    uti <- (1 - f5[i])*a[i] + u[i]
    eti <- e[i]

  }

  # Now combine and drop rows that were added for incorporation in afMod()
  out <- data.frame(ct = ct, dt = NA, 
                    f0 = a0/(u0 + a0), r1 = r1, r2 = r2, r3 = r3, f5 = f5,
                    f = a, s = u, 
                    j = NA, e = e, e.int = NA, er = e/(a0 + u0))

  out <- out[!drop.rows, ]

  # Recalculate ddt, e.int, and j.ave.int in case there were dropped rows
  out$dt <- diff(c(0, out$ct))
  out$e.int <- diff(c(0, out$e))
  out$j <- out$e.int/out$dt

  return(out)
}


