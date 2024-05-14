# Check for multiple values of mutually exclusive dummy variables

checkDum <- function(dat, vnames = c('^app\\.mthd\\.[ocbt]', '^incorp\\.[ds]', '^man\\.source\\.[cp]')) {

  for (i in vnames) {
    dd <- dat[, grepl(i, names(dat))]
    if (ncol(dd) > 0) {
      rs <- rowSums(dd)
      if (any(rs > 1)) {
        return(1)
      } else {
        return(0)
      }
    }
  }
  
}
