# Function for running ALFAM2 model

alfam2 <- function(
  dat,
  pars = ALFAM2::alfam2pars03,
  add.pars = NULL,
  app.name = 'TAN.app',
  time.name = 'ct',
  time.incorp = NULL,
  group = NULL,
  center = c(app.rate = 40,
             man.dm   =  6.0,
             man.tan   =  1.2,
             man.ph    =  7.5,
             air.temp  = 13,
             wind.2m   =  2.7,
             wind.sqrt =  sqrt(2.7),
             crop.z    = 10),
  pass.col = NULL,
  incorp.names = c('incorp', 'deep', 'shallow'),
  prep.dum = TRUE,
  prep.incorp = TRUE,
  add.incorp.rows = FALSE,
  check = TRUE,
  warn = TRUE,
  value = 'emis',
  conf.int = NULL,
  pars.ci = ALFAM2::alfam2pars03var,
  n.ci = NULL,
  var.ci = 'er',
  ...
  ) {

  # Get all args to pass to CI function
  actargs <- as.list(environment())

  # Check CI arguments
  if (check) {
    checkArgClassValue(conf.int, expected.class = c('numeric', 'integer', 'character', 'NULL', 'NA'))
    if (inherits(conf.int, 'character')) {
      checkArgClassValue(conf.int, expected.class = 'character', expected.values = 'all')
    }
    checkArgClassValue(pars.ci, expected.class = c('data.frame', 'matrix', 'array', 'NULL', 'NA'))
    checkArgClassValue(n.ci, expected.class = c('integer', 'numeric', 'NULL', 'NA'))
  }

  # Confidence interval requested
  if (!is.null(conf.int) && !is.na(conf.int)) {
    out <- do.call(alfam2CI, actargs)
    return(out)
  }

  # Or normal call, without confidence interval
  # Add predictor variables if given in '...' optional arguments
  # and look for secret flatout argument (with it alfam2() goes as fast as possible without checks and without some conversions (requires more data prep prior to call))
  if (!missing(...)) {
    ovars <- list(...)
    dat <- data.frame(dat, ovars, check.names = FALSE)
    if (any(names(ovars) == 'flatout')) {
      flatout <- ovars[[names(ovars) == 'flatout']]
      ovars <- ovars[!names(ovars) == 'flatout']
    } else {
      flatout <- FALSE    
    }
  } else {
    flatout <- FALSE    
  }

  if (flatout) {
    prep.dum <- prep.incorp <- check <- warn <- FALSE
    warning('You are using the flatout = TRUE option.\n   Be sure to verify output.')
  }

  if (!prep.incorp && warn) {
    warning('You specified prep.incorp = FALSE.\n   Columns `__f4`, `__add.row`, `__group` must be present (and correct) in input data.')
  }

  if (!check && warn) {
    warning('You set check = FALSE. Be sure to verify output!')
  }

  # Convert data.table to data.frame
  if (inherits(dat, c('data.table', 'tbl_df'))) {
    dat <- as.data.frame(dat)
  } 
  
  # Relative emission only if app.name is missing
  if (is.null(app.name) || is.na(app.name) || !app.name %in% names(dat)) {
    if (warn) {
      warning('Argument app.name is missing or dat is missing column of given name.\n    So function will return relative emission only.\n')
    }
    relonly <- TRUE
    dat$`__appplaceholder94` <- 1
    app.name <- '__appplaceholder94'
  }

  # Argument checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (check) {
    checkArgClassValue(dat, expected.class = 'data.frame')
    checkArgClassValue(pars, expected.class = c('numeric', 'list'), allow.na = FALSE)
    checkArgClassValue(add.pars, expected.class = c('numeric', 'list', 'NULL'), allow.na = FALSE)
    checkArgClassValue(app.name, expected.class = c('character', 'NULL'), allow.na = TRUE)
    checkArgClassValue(time.name, expected.class = 'character', allow.na = FALSE)
    checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))
    checkArgClassValue(group, expected.class = c('character', 'NULL'))
    checkArgClassValue(center, expected.class = c('numeric', 'list', 'NULL'), allow.na = FALSE)
    checkArgClassValue(pass.col, expected.class = c('character', 'NULL'), allow.na = FALSE)
    checkArgClassValue(incorp.names, expected.class = c('character', 'NULL'), allow.na = FALSE)
    checkArgClassValue(prep.dum, expected.class = 'logical', allow.na = FALSE)
    checkArgClassValue(prep.incorp, expected.class = 'logical', allow.na = FALSE)
    checkArgClassValue(add.incorp.rows, expected.class = 'logical', allow.na = FALSE)
    checkArgClassValue(check, expected.class = 'logical', allow.na = FALSE)
    checkArgClassValue(warn, expected.class = 'logical', allow.na = FALSE)
    value <- tolower(value)
    checkArgClassValue(value, expected.class = 'character', expected.values = c('emis', 'incorp'), allow.na = FALSE)

    if (nrow(dat) == 0) stop('dat has no rows!')

    # Warning if center is changed
    if (!identical(center, eval(formals(alfam2)$center))) {
      if (warn) {
        warning('You specified values for the center argument for centering means.\n    Only use this option if you know what you are doing and centering means match the parameter set.\n    User-supplied values will replace or extend default values.')
      }
    }

    # Check that arguments that should be column names are actually present in dat
    if (!time.name %in% names(dat)) {
      stop(paste0('time.name argument you specified (', time.name, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
    }

    if (!all(pass.col %in% names(dat))) {
      stop(paste0('One or some values of pass.col you specified (', paste(pass.col, collapse = ', '), ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
    }

    if (!is.null(group) && !group %in% names(dat)) {
      stop(paste0('group argument you specified (', group, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
    }

    if (any(is.na(dat[, c(time.name, app.name)]))) stop('Missing values in time or application rate columns.\nSee ', time.name, ' and ', app.name, ' columns.')

    # Fix negative times with a warning
    if (warn && any(dat[, time.name] < 0)) {
      warning(paste0('Negative times (variable "', time.name, '") found and set to 0.'))
      dat[dat$time.name < 0, time.name] <- 0
    }

    # Tell user whether default or user-supplied parameters are in use
    # version note: update message with changes to default parameters!
    if (warn) {
      if (identical(pars, eval(formals(alfam2)$pars))) {
	if (identical(pars, ALFAM2::alfam2pars01)) {
          message('Default parameters (Set 1) are being used.')
	} else if (identical(pars, ALFAM2::alfam2pars02)) {
          message('Default parameters (Set 2) are being used.')
	} else if (identical(pars, ALFAM2::alfam2pars03)) {
          message('Default parameters (Set 3) are being used.')
	}
      } else {
        message('User-supplied parameters are being used.')
      }
    }

    # If pars was given as list, change to vector
    if(is.list(pars)) {
      pars <- unlist(pars)
    }

    # Throw error if pars has duplicate names 
    if(any(duplicated(names(pars)))) {
      stop('Check for duplicate names in pars')
    }
    
    # Continue with pars conversion, switch order for names that start with e.g. f0 or r3
    if(any(chg.nms <- grepl('^[fr]{1}[0-4]{1}[.]', names(pars)))){
      names(pars)[chg.nms] <- gsub('^([fr][0-4])[.](.*)', '\\2\\.\\1', names(pars)[chg.nms])
    }

    # Additional pars that override or extend pars
    if (!is.null(add.pars)) {
      if (warn) {
        message('Additional parameters were specified and will replace or extend pars argument.')
      }

      # If add.pars was given as list, change to vector
      if(is.list(add.pars)) {
        add.pars <- unlist(add.pars)
      }

      # Continue with add.pars conversion as with pars above
      if(any(chg.nms <- grepl('^[fr]{1}[0-4]{1}[.]', names(add.pars)))){
        names(add.pars)[chg.nms] <- gsub('^([fr][0-4])[.](.*)', '\\2\\.\\1', names(add.pars)[chg.nms])
      }

      # Combine add.pars with pars, with add.pars overriding pars
      pars <- c(add.pars, pars[!names(pars) %in% add.pars])
    }

    # Check that all names for pars end with a number
    if(any(!grepl('[0-9]$', names(pars)))) {
      stop('One or more entries in argument "pars" cannot be assigned to parameters f0, r1, r2, r3, f4, r5.\n Make sure that the naming is correct. Either append the corresponding primary parameter or number (e.g., 0 to 4, or f0, r1) at the name endings (e.g. int.f0)\n or prepend the parameter separated by a dot (e.g. f0.int) or provide an appropriately named list.')
    }

    # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
    reserved.names <- c('__group', '__add.row', '__f4', '__f0', '__r1', '__r2', '__r3', '__r5', '__drop.row', '__orig.order')
    if (warn && any(names(dat) %in% reserved.names)) {
      warning('dat data frame has some columns with reserved names.\nYou can proceed, but there may be problems.\nBetter to remove/rename the offending columns: ', reserved.names[reserved.names %in% names(dat)])
    }

    # Remove non-existent pass.col columns if pass-through requested
    pass.col <- intersect(pass.col, names(dat))

  } # End check skip

  # Extend centering means
  # Warning if center is changed
  if (!identical(center, eval(formals(alfam2)$center))) {
    # Replace or extend center vector 
    center_defaults <- eval(formals(alfam2)$center)
    center <- c(center, center_defaults[!(names(center_defaults) %in% names(center))])
  }


  # Prepare input data (dummy variables)
  if (prep.dum) {
    dum <- prepDat(dat, warn = warn)
    if (!is.null(dum) && nrow(dum) == nrow(dat)) {
      dat <- cbind(dat, dum)
    } else {
      if (!is.null(dum)) {
        stop('Problem with dummy variable creation.')
      }
    }
  } else {
    if (warn) {
      warning('You set prep.dum = FALSE,\n   so categorical predictors will not be converted to dummy variables.\n  To include these predictors add dummy variables externally or set prep.dum = TRUE.')
    }
  }

  # Check for dummy variables problems
  if (check && checkDum(dat) != 0) {
    stop('Dummy variable problem--multiple (suspected) mututally exclusive dummy variables are 1 or TRUE.\n    Check input data.')
  }

  # Set app.rate.ni to 0 for any injection method
  # check to see if any of these rows have app.rate.ni > 0, if so, warning() and change to 0
  if (check && any(c('app.mthd.os', 'app.mthd.cs') %in% names(dat)) && 'app.rate.ni' %in% names(dat) && 
      any(dat[rowSums(abs(dat[, grepl('app.mthd.[oc]s', names(dat))])) > 0, 'app.rate.ni'] > 0)) {
    # abs() included below just in case there is somehow a negative value (should never occur, but user could do it manually)
    dat[rowSums(abs(dat[, grepl('app.mthd.[oc]s', names(dat))])) > 0, 'app.rate.ni'] <- 0
    if (warn) {
      warning('Input data dat had application rate app.rate.ni > 0 for injection application methods app.mthd.os or app.mthd.cs.\n    But app.rate.ni should not affect emissions from injection\n   (ni = no injection), so app.rate.ni was set to 0 for these rows.')
    }
  }

  # If there is no grouping variable, add one to simplify code below (only one set, for groups)
  if(is.null(group)) {
    dat$`__group` <- 'a' 
  } else {
    # Create new group column that could combine multiple columns (typically only 1 column though)
    dat$`__group` <- apply(dat[, group, drop = FALSE], 1, paste, collapse = '//')
  }

  # Center numeric predictors
  if(value != 'incorp') {
    if(!is.null(center)[1] && !is.na(center)[1]) {
      # Get columns that will be centered 
      c_cols <- names(center)[names(center) %in% names(dat)]

      # Center
      if(length(c_cols)) {
        dat[, c_cols] <- scale(dat[, c_cols, drop = FALSE], center = center[c_cols], scale = FALSE)
        #dat[, c_cols] <- sweep(dat[, c_cols, drop = FALSE], 2, center[c_cols])
      }
    } else if (warn) {
      # Warning if centering is turned off
      warning('You turned off centering by setting center = NULL.\n   Only use this option if you know what you are doing,\n   and only with a matching parameter set.')
    }
  }

  # Original order (for sorting before return)
  dat$`__orig.order` <- 1:nrow(dat)

  # Sort 
  # prepIncorp() should require sorted ct 
  dat <- dat[order(dat$`__group`, dat[, time.name]), ]

  # Sort out incorporation inputs and pars
  if (prep.incorp) {
    # Default f4 value (for no incorporation in group, or incorporation only later)
    # If using flatout, __f4 should (must) already be in input data, and is only fixed at 1 if there is no incorporation
    dat$`__add.row` <- FALSE 
    dat[, '__f4'] <- 1
    # Skipped for prep.incorp == FALSE (must be done externally before calling alfam2())
    if (prep.incorp && !is.null(time.incorp)) {
      incprepout <- prepIncorp(dat, pars, time.name, time.incorp, incorp.names, warn)
      dat <- incprepout[['dat']]
      time.incorp <- incprepout[['time.incorp']]
    }
    if (value == 'incorp') {
      if (warn) {
        warning('You set values = "incorp" so output does not include emission results.')
      }
      dat <- dat[order(dat$`__orig.order`), ]
      return(dat)
    }
  } else {
    if (warn) {
      warning('Skipping incorporation prep because you set prep.incorp = FALSE.\n   Incorporation will not be applied\n   (unless correct values for variables __f4 and __add.row are first added externally,\n    e.g., with alfam2(...,value = "incorp"))')
    }
  }

  if (check && is.null(time.incorp)) {
    # Remove any incorporation columns if no incorporation time is provided (to avoid incorrect incorp effect on r3)
    names.orig <- names(dat)
    dat <- dat[, !(ii <- grepl(paste(incorp.names, collapse = '|'), names(dat))), drop = FALSE]
    # Note that dum is just for output (not calculations at this point, those already in dat)
    dum <- dum[, !(jj <- grepl(paste(incorp.names, collapse = '|'), names(dum))), drop = FALSE]
    if (warn) {
      warning(paste('Incorporation columns', paste(names.orig[ii], collapse = ', '), 'were dropped \n    because argument time.incorp is NULL\n    So there is no incorporation.\n    Set check = FALSE to not drop, but then check output.\n'))
    }
  }


  # Drop parameters for missing predictors
  p.orig <- pars
  ppnames <- gsub('\\.{1}[rf]{1}[0-9]$', '', names(pars))
  pars <- pars[predpres <- ppnames %in% names(dat) | ppnames == 'int']

  if(warn && check && any(!predpres)) {
    warning('Running with ', sum(predpres), ' parameters. Dropped ', sum(!predpres), ' with no match.\n',
            'These secondary parameters have been dropped:\n  ', 
            paste(names(p.orig)[!predpres], collapse = '\n  '), '\n\n') #,
            #'These secondary parameters are being used:\n  ', 
            #paste(names(p.orig)[predpres], collapse = '\n  '), '\n')
  }

  # Associate (secondary) parameters with primary parameters (r1, etc.)
  which0 <- grep('0$', names(pars)) # For f0
  which1 <- grep('1$', names(pars)) # For r1
  which2 <- grep('2$', names(pars)) # For r2
  which3 <- grep('3$', names(pars)) # For r3
  which4 <- grep('4$', names(pars)) # For a to u transfer at specific times, incorporation, will be applied once only!
  which5 <- grep('5$', names(pars)) # For r5

  names(pars) <- gsub('\\.[rf][0-9]$', '', names(pars))

  if(check && !all(ww <- sort(c(which0, which1, which2, which3, which4, which5)) == 1:length(pars))) {
    stop('Something wrong with parameter argument pars: ', paste(ww, collapse = ', '))
  }

  # Make sure parameter names can be found in dat
  if(check && any(ncheck <- !(names(pars) %in% c('int', names(dat))))) stop ('Names in parameter vector pars not in dat (or not "int"): ', paste(names(pars)[ncheck], collapse = ', '))

  # Missing values
  if (check || warn) {
    nn <- unique(names(pars[!grepl('^int', names(pars))]))
    if (any(anyNA(dat[, nn]))) {
      if (check) {
        ddd <- dat[!dat$'__add.row', ]
        print(apply(ddd[, nn], 2, function(x) sum(is.na(x))))
        stop(paste('Missing value(s) in predictor variable(s)\n   See above for variables.\n   Check these rows:', paste(as.integer(which(is.na(rowSums(ddd[, nn])))), collapse = ', ')))
      }
      if (warn) {
        cat('Warning!\n')
        cat('Missing values in predictors:\n')
        ddd <- dat[!dat$'__add.row', ]
        print(apply(ddd[, nn], 2, function(x) sum(is.na(x))))
        warning(paste('Missing value in predictor variable(s)\n   Check these rows:', paste(as.integer(which(is.na(rowSums(ddd[, nn])))), collapse = ', ')))
      }
    }
  }

  # Calculate primary parameters
  if(length(which0) > 0) dat[, '__f0'] <- calcPParms(pars[which0], dat, warn = warn, tr = 'logistic') else dat[, '__f0'] <- 0
  if(length(which1) > 0) dat[, '__r1'] <- calcPParms(pars[which1], dat, warn = warn)                  else dat[, '__r1'] <- 0
  if(length(which2) > 0) dat[, '__r2'] <- calcPParms(pars[which2], dat, warn = warn)                  else dat[, '__r2'] <- 0
  if(length(which3) > 0) dat[, '__r3'] <- calcPParms(pars[which3], dat, warn = warn, upr = 100)       else dat[, '__r3'] <- 0
  if(length(which5) > 0) dat[, '__r5'] <- calcPParms(pars[which5], dat, warn = warn, upr = 100)       else dat[, '__r5'] <- 0
  # f4 only calculated where it is already 0 (not default of 1)
  if(length(which4) > 0) dat[dat[, '__f4'] == 0, '__f4'] <- calcPParms(pars[which4], dat[dat[, '__f4'] == 0, ], tr = 'logistic')

  # Add drop row indicator
  dat$'__drop.row' <- dat$'__add.row' & !add.incorp.rows

  # Pare down to essential columns
  # No good reason for this anymore except debugging ease
  dat <- dat[, c(group, '__group', '__orig.order', time.name, app.name, '__add.row', '__f4', '__f0', '__r1', '__r2', '__r3', '__r5', '__drop.row', pass.col)]

  # Sort required for gstart and gend to work, also ct loop
  # _orig.order used to sort back to original order later
  dat <- dat[order(dat[, '__group'], dat[, time.name]), ]

  # Group positions
  # Note - 1 to get C++ approach of first index = 0
  gstart <- match(unique(dat[, '__group']), dat[, '__group']) - 1
  gend <- c(gstart[-1], nrow(dat)) - 1

  # Calculate emission for all groups, all in C++ function
  ce <- rcpp_calcEmis(
    cta = dat[, time.name],
    F0a = dat[, "__f0"] * dat[, app.name],
    S0a = (1 - dat[, "__f0"]) * dat[, app.name],
    r1a = dat[, "__r1"],
    r2a = dat[, "__r2"],
    r3a = dat[, "__r3"],
    f4a = dat[, "__f4"], 
    r5a = dat[, "__r5"], 
    gstart = gstart,
    gend = gend 
  )

  # ce, from rcpp_calcEmis, contains columns ct, dt, f, s, e
  ce <- as.data.frame(ce)

  # Drop added rows unless requested through add.incorp.rows
  ce <- ce[!dat[, '__drop.row'], ]
  # Keep up with dat to use for sorting below
  dat <- dat[!dat[, '__drop.row'], ]

  # Recalculate dt, e.int after possibly dropping rows and get j
  gstart <- match(unique(dat[, '__group']), dat[, '__group'])
  ct.prev <- c(0, ce[-nrow(ce), 'ct'])
  ct.prev[gstart] <- 0
  ce$dt <- ce[, 'ct'] - ct.prev

  e.prev <- c(0, ce$e[-nrow(ce)])
  e.prev[gstart] <- 0
  ce$ei <- ce$e - e.prev
  ce$j <- ce$ei / ce$dt

  # Change ct name so it matches input
  names(ce)[names(ce) == 'ct'] <- time.name

  # Add relative emission
  ce$er <- ce$e / dat[, app.name]

  # Sort to match original order, with any added row at end (might be better to keep time order in those cases)
  # NTS there was a , -1, what first col was dropped before? Prob group?
  ce <- ce[order(dat$`__orig.order`), ]
  # Keep up with dat for grouped operation below
  dat <- dat[order(dat$`__orig.order`), ]
  row.names(ce) <- seq.int(nrow(ce))

  # Add dummy variables *after* switching back to original sort order
  if (!add.incorp.rows && prep.dum) {
    if (!is.null(dum) && nrow(dum) == nrow(ce)) {
      ce <- cbind(dum, ce)
    }
  }

  # Get primary parameters (could be done in data.frame() below but there is name issue. . .
  ppars <- dat[, c('__f0', '__r1', '__r2', '__r3', '__f4', '__r5')]
  names(ppars) <- gsub('__', '', names(ppars))

  # Add other columns
  # If group not specified by user, group = NULL and is automatically left out
  out <- data.frame(dat[, c(group, pass.col), drop = FALSE],
                    ce, 
                    ppars,
                    row.names = NULL, check.names = FALSE)

  # Instantaneous flux
  out$jinst <- out$r1 * out$f + out$r3 * out$s

  return(out)

}

