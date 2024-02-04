# Confidence intervals for alfam2() predictions

alfam2CI <- function(
  dat,
  pars,
  add.pars,
  app.name,
  time.name,
  time.incorp,
  group,
  center,
  pass.col,
  incorp.names,
  prep.dum,
  prep.incorp,
  add.incorp.rows,
  check,
  warn,
  value,
  conf.int,
  pars.ci,
  n.ci,
  var.ci,
  ...
  ) {

  # Check argument values
  checkArgClassValue(pars.ci, expected.class = c('data.frame', 'matrix', 'array'))
  checkArgClassValue(var.ci, expected.class = 'character', expected.values = c('f0', 'r1', 'r2', 'r3', 'f4', 'r5', 'f', 's', 'j', 'ei', 'e', 'er'))

  if (is.null(n.ci) || is.na(n.ci)) {
    n.ci <- nrow(pars.ci)
  }

  checkArgClassValue(n.ci, expected.class = c('integer', 'numeric'), expected.range = c(0, nrow(pars.ci)))

  group.orig <- group
  if (is.null(group.orig)) {
    group <- '_g_r_o_u_p_42_'
    dat[, group] <- 'A'
  }

  # First run, with main pars
  out.base <- alfam2(dat = dat, pars = pars, add.pars = add.pars, app.name = app.name, time.name = time.name, 
                     time.incorp = time.incorp, group = group, center = center, pass.col = pass.col,
                     incorp.names = incorp.names, prep.dum = prep.dum, prep.incorp = prep.incorp,
                     add.incorp.rows = add.incorp.rows, check = check, warn = warn, value = 'emis')

  out.base$`__order__` <- 1:nrow(out.base)

  # Prepare data--dummy variables and incorporation
  # No checking or warning because messages are confusing when users asked for CI, and call above should indicate any problems
  datprepped <- alfam2(dat = dat, pars = pars, add.pars = add.pars, app.name = app.name, time.name = time.name, 
                 time.incorp = time.incorp, group = group, center = center, pass.col = pass.col,
                 incorp.names = incorp.names, prep.dum = prep.dum, prep.incorp = prep.incorp,
                 add.incorp.rows = add.incorp.rows, check = FALSE, warn = FALSE, 
                 value = 'incorp')

  if (!is.null(n.ci) && !is.na(n.ci)) {
    i.ci <- sample(nrow(pars.ci), n.ci)
  } else {
    i.ci <- 1:nrow(pars.ci)
  }

  # Empty data frame for results
  # NTS: return par set ID or iteration so users can externally select limits for all vars together?
  out.var <- data.frame()
  for (i in i.ci) {
    pp <- pars.ci[i, ]

    # Incorporation info already added from data prep in previous call
    out.it <- alfam2(dat = datprepped, pars = pp, add.pars = add.pars, app.name = app.name, time.name = time.name, 
                     group = group, center = center, pass.col = pass.col,
                     prep.dum = FALSE, prep.incorp = FALSE,
                     check = FALSE, warn = FALSE, value = 'emis')

    out.var <- rbind(out.var, out.it)

  }

  if (conf.int == 'all') {
    out <- out.var
  } else {
    out.var <- out.var[is.finite(rowSums(out.base[, var.ci, drop = FALSE])), ]
    lwr <- aggregate(out.var[, var.ci, drop = FALSE], out.var[, c(group, time.name)], function(x) quantile(x, (1 - conf.int) / 2))
    names(lwr)[-1:-2] <- paste0(names(lwr)[-1:-2], '.lwr')
    upr <- aggregate(out.var[, var.ci, drop = FALSE], out.var[, c(group, time.name)], function(x) quantile(x, 1 - (1 - conf.int) / 2))
    names(upr)[-1:-2] <- paste0(names(upr)[-1:-2], '.upr')
    # NTS: sort out sorting order!
    out <- merge(out.base, lwr, by = c(group, time.name), all.x = TRUE)
    out <- merge(out, upr, by = c(group, time.name), all.x = TRUE)
    out <- out[order(out$`__order__`), ]
    out$`__order__` <- NULL
  }

  if (is.null(group.orig)) {
    out[, group] <- NULL
  }

}
