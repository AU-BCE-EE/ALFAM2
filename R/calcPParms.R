calcPParms <-
function(p, dat, tr = 'log10', upr = Inf, warn = TRUE) {
  # Intercept must be first term!
  which.int <- grep('int', names(p))
  if(length(which.int)>1) stop('calcPParms called with more than one intercept (int) term for p argument: ', names(p)[which.int])
  if(length(which.int) == 0) {
    p <- c(int = 0, p)
    which.int = 1
  }
  r <- rep(p[[which.int]], nrow(dat)) # Rep required in case int is the only parameter

  for(i in names(p)[-which.int]) {
    r <- r + p[[i]]*dat[[i]]
  }

  if(tr == 'log10') r <- 10^r
  if(tr == 'logistic') r <- (exp(r)/(1 + exp(r)))

  # Apply limit
  if (upr < Inf && any(r > upr)) {
    if (warn) {
      warning('Some calculated primary parameters are at the limit. Check input parameters.')
    }
    r[r > upr] <- upr
  }

  if (any(is.na(r))) {
    stop('Missing values in a calculated primary parameter vector.')
  }

  return(r)

}

logistic <-
function (x) {
  exp(x)/(1 + exp(x))
}

logit <-
function (p) {
  log(p/(1 - p))
}
