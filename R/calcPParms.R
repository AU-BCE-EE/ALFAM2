calcPParms <-
function(p, dat, tr = 'log10') {
  # Intercept must be first term!
  which.int <- grep('int', names(p))
  if(length(which.int)>1) stop('calcPParms called with more than one intercept (int) term for p argument: ', names(p)[which.int])
  if(length(which.int) == 0) {
    p <- c(int = 0, p)
    which.int = 1
  }
  r <- rep(p[[which.int]], nrow(dat)) # Rep required incase int is the only parameter

  for(i in names(p)[-which.int]) {
    r <- r + p[[i]]*dat[[i]]
  }

  if(tr == 'log10') return(10^r)
  if(tr == 'logistic') return(exp(r)/(1 + exp(r)))

}

logistic <-
function (x) {
  exp(x)/(1 + exp(x))
}

logit <-
function (p) {
  log(p/(1 - p))
}
