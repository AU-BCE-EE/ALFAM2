# ALFAM2 function for inventory calculations

# Inputs
# Outputs
alfamei <- function(
  dat, 
  pars = ALFAM2::alfam2pars03, 
  time.name,
  app.tan.name = 'app.tan',
  time.incorp = NULL,
  eventkey = NULL,
  aggkey = NULL,
  uncert = NULL,
  pars.uncert = ALFAM2::alfam2pars03var,
  uncert.settings = NULL,
  nu = 100,
  cl = 0.8,
  seed = NULL,
  quiet = FALSE,
  ...
) {

  dat <- as.data.frame(dat)
  uncert <- tolower(uncert)

  # If missing event key, assume all rows are separate events
  if (is.null(eventkey) || is.na(eventkey)) {
    eventkey <- '__eventkeydum'
    dat[, eventkey] <- 1:nrow(dat)
    rmcols <- eventkey
  }

  # Get wind.sqrt if missing
  if (!'wind.sqrt' %in% names(dat) & 'wind.2m'%in% names(dat)) {
    dat$wind.sqrt <- sqrt(dat$wind.2m)
  }

  # Reference emission
  pred <- alfam2(dat, pars = pars, 
		 app.name = NULL, 
                 time.name = time.name, 
		 time.incorp = time.incorp,
                 prep.dat = TRUE, group = eventkey, warn = FALSE, list(...))

  pred.ref <- pred
  dat.out <- dat
  dat.out$emis.fact <- pred$er
  dat.out$emis.tot <- dat.out$emis.fact * dat.out[, app.tan.name]
  
  # First sum 
  # Get max time by eventkey
  mxt <- aggregate(dat.out[, time.name], dat.out[, eventkey, drop = F], FUN = max)
  names(mxt)[2] <- time.name
  dat.final <- merge(mxt, dat.out)
  s0 <- aggregate2(dat.final, c(app.tan.name, 'emis.tot'), by = aggkey, FUN = list(sum))
  s0$emis.fact <- s0$emis.tot / s0[, app.tan.name]
  
  # Uncertainty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # NTS: may want to use improbable names with `__` start
  if ('vars' %in% uncert || 'pars' %in% uncert) {
    # Uncertainty input data frame
    dat.uc <- dat[rep(1:nrow(dat), nu), ]
    dat.uc$uset <- rep(1:nu, each = nrow(dat))
    dat.uc$ukey <- paste(dat.uc[, eventkey], dat.uc$uset)
 
    # First input
    if ('vars' %in% uncert && any(!is.na(uncert.settings[, 3:5])) && any(uncert.settings[, 3:5] > 0)) {
      if (!quiet) {
        message(paste0('Including uncertainty from predictor variable values using ', deparse(substitute(uncert.settings))))
      }
      if (is.null(uncert.settings)[1] | is.na(uncert.settings)[1]) {
        stop('uncert argument includes \'vars\' uncertainty, but uncert.settings is not defined.')
      }

      # Add error to input variable values
      # Loop through predictor variables in uncert data frame
      for (j in 1:nrow(uncert.settings)) {
        uccv <- as.list(uncert.settings[j, ])
        if (any(!is.na(uncert.settings[j, 3:5]))) {
          pvar <- uccv$pvar
          # Get error for each uncertainty simulation (total = nu)
          if (uccv$dist.type == 'Uniform') {
            if (!is.null(seed) && !is.na(seed)) {
              set.seed(seed)
            }
            e <- runif(nu, min = uccv$min, max = uccv$max)
          } else if (uccv$dist.type == 'Normal') {
            if (uccv$rel == 'Relative') {
              dmean = 1 
            } else {
              dmean = 0 
            }
            if (!is.null(seed) && !is.na(seed)) {
              set.seed(seed)
            }
            e <- rnorm(nu, mean = dmean, sd = uccv$sd)
            if (all(!is.na(c(uccv$min, uccv$max)))) {
              e[e < uccv$min] <- uccv$min
              e[e > uccv$max] <- uccv$max
            }
          }

          # Get error into data frame by uncertainty set
          edat <- data.frame(uset = 1:nu, e = e)
          names(edat)[2] <- paste0('e.', pvar)
          dat.uc <- merge(dat.uc, edat, by = 'uset', all.x = TRUE)

          # And adjust predictor variable values by error
          if (uccv$rel == 'Absolute') {
            if (uccv$dist.type == 'Uniform') {
              dat.uc[, pvar] <- dat.uc[, paste0('e.', pvar)]
            } else {
              dat.uc[, pvar] <- dat.uc[, pvar] + dat.uc[, paste0('e.', pvar)]
            }
          } else if (uccv$rel == 'Centered') {
            dat.uc[, pvar] <- dat.uc[, pvar] + dat.uc[, paste0('e.', pvar)]
          } else if (uccv$rel == 'Relative') {
            dat.uc[, pvar] <- dat.uc[, pvar] * dat.uc[, paste0('e.', pvar)]
          } else {
            stop('Check uncert.settings relative column')
          }

          # And finally check any absolute limits
          if (uccv$rel == 'Absolute') {
            if (!is.na(uccv$min)) {
              dat.uc[dat.uc[, pvar] < uccv$min, pvar] <- uccv$min
            }
            if (!is.na(uccv$max)) {
              dat.uc[dat.uc[, pvar] > uccv$max, pvar] <- uccv$max
            }
          }

        }
      }
    } 

    # Now parameter uncertainty (if any) and predictions
    if ('pars' %in% uncert) {
      if (!quiet) {
        message(paste0('Including uncertainty from alfam2() parameter values using ', deparse(substitute(pars.uncert))))
      }
      if (!is.null(seed) && !is.na(seed)) {
        set.seed(seed)
      }
      replace <- FALSE
      if (nu > nrow(pars.uncert)) {
        message('Sampling from pars.uncert with replacement because nu > nrow(pars.uncert)')
        replace <- TRUE
      }
      whichpars <- sample(1:nrow(pars.uncert), nu, replace = replace)
    } else {
      whichpars <- 0
      pp <- pars
    }
    
    pred.uc <- data.frame()

    dat.uc$parset <- NA
    
    for (i in 1:nu) {

      dat.uc[dat.uc$uset == i, 'parset'] <- whichpars[i]
    
      if (min(whichpars) > 0) {
        pp <- pars.uncert[whichpars[i], ]
      }

      pred <- alfam2(dat.uc[dat.uc$uset == i, ], pars = pp, 
          	   app.name = NULL, 
                     time.name = time.name, 
          	   time.incorp = time.incorp,
                     prep.dat = TRUE, group = eventkey, warn = FALSE, list(...))

      dat.uc[dat.uc$uset == i, 'emis.fact'] <- pred$er

      pred.uc <- rbind(pred.uc, pred)
    
    }
    
    # Two levels of quantiles: by dat row and aggregation level
    dat.uc$emis.tot <- dat.uc$emis.fact * dat.uc[, app.tan.name]
    # Get final (latest) values (the merge() call drops the other key x time combinations--not need to end all keys at same time)
    dat.uc.final <- merge(mxt, dat.uc)
    s1 <- aggregate2(dat.uc.final, c(app.tan.name, 'emis.tot', 'emis.fact'), by = eventkey, 
	       FUN = list(lwr = function(x) quantile(x, (1 - cl) / 2), 
			  upr = function(x) quantile(x, 0.5 + cl / 2)))
    dat.final <- merge(dat.final, s1, by = eventkey, all.x = TRUE)
    
    # Aggregate within uncertainty set
    s2 <- aggregate2(dat.uc.final, c(app.tan.name, 'emis.tot'), by = c('uset', aggkey), FUN = list(sum))
    s2$emis.fact <- s2$emis.tot / s2[, app.tan.name]
    
    # And then get quantiles
    s3 <- aggregate2(s2, c(app.tan.name, 'emis.tot', 'emis.fact'), by = aggkey, 
	       FUN = list(lwr = function(x) quantile(x, (1 - cl) / 2), 
			  upr = function(x) quantile(x, 0.5 + cl / 2)))

    # Combine with summary s0
    s0 <- merge(s0, s3, by = aggkey)
    
  } else {
    dat.uc.final <- NULL
    pred.uc <- NULL
  }

  # Drop added columns
  dat.final[, c('__eventkeydum')] <- NULL 
  dat.uc.final[, c('__eventkeydum')] <- NULL 
  pred.ref[, c('__eventkeydum')] <- NULL 

  return(list(emisdis = dat.final, emisagg = s0, predref = pred.ref, emisuc = dat.uc.final, preduc = pred.uc))

} 
