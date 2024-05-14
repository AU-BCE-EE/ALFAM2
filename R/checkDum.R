# Check for multiple values of mutually exclusive dummy variables

checkDum <- function(dat, vnames = c('^app\\.mthd\\.[ocbt]', '^incorp\\.[ds]', '^man\\.source\\.[cp]')) {

  for (i in vnames) {
    dd <- dat[, grepl(i, names(dat))]
    if (ncol(dd) > 0) {
      cs <- colSums(dd)
      if (any(cs > 1)) {
        return(1)
      } else {
        return(0)
      }
    }
  }
  
}
