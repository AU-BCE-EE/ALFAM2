# Check for multiple values of mutually exclusive dummy variables
# Returns 0 for no problems

checkDum <- function(dat, vnames = c('^app\\.mthd\\.[ocbt]', '^incorp\\.[ds]', '^man\\.source\\.[cp]')) {

  for (i in vnames) {
    if (any(grepl(i, names(dat)))) {
      dd <- dat[, grepl(i, names(dat)), drop = FALSE]
      if (any(dd > 0)) {
        rs <- rowSums(dd)
        if (any(rs > 1)) {
          return(1)
        } 
      }
    }
  }

  return(0) 
  
}
