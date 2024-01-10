# Function for running ALFAM2 model

alfam2 <- function(
  dat,
  pars = ALFAM2::alfam2pars02,
  add.pars = NULL,
  app.name = 'TAN.app',
  time.name = 'ct',
  time.incorp = NULL,
  group = NULL,
  center = c(app.rate  = 40,
             man.dm    =  6.0,
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
  ...
  ) {

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
  if (class(dat)[1] %in% c('data.table', 'tbl_df')) {
    dat <- as.data.frame(dat)
  }

  # Argument checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (check) {
    checkArgClassValue(dat, expected.class = 'data.frame')
    checkArgClassValue(pars, expected.class = c('numeric', 'list'), allow.na = FALSE)
    checkArgClassValue(add.pars, expected.class = c('numeric', 'list', 'NULL'), allow.na = FALSE)
    checkArgClassValue(app.name, expected.class = 'character', allow.na = FALSE)
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
    if (warn && !identical(center, eval(formals(alfam2)$center))) {
      warning('You specified values for the center argument for centering means. Only use this option if you know what you are doing.')
    }

    if (!time.name %in% names(dat)) {
      stop(paste0('time.name argument you specified (', time.name, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
    }

    if (!app.name %in% names(dat)) {
      stop(paste0('app.name argument you specified (', app.name, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
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
      if (missing(pars)) {
        message('Default parameters (Set 2) are being used.')
      } else {
        message('User-supplied parameters are being used.')
      }
    }

    # If pars was given as list, change to vector
    if(is.list(pars)) {
      pars <- unlist(pars)
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

  # Prepare input data (dummy variables)
  if (prep.dum) {
    dum <- prepDat(dat, value = 'dummy', warn = warn)
    if (!is.null(dum) && nrow(dum) == nrow(dat)) {
      dat <- cbind(dat, dum)
    } else {
      stop('Problem with dummy variable creation.')
    }
  } else {
    if (warn) {
      warning('You set prep.dum = FALSE,\n   so categorical predictors will not be converted to dummy variables.\n  To include these predictors add dummy variables externally or set prep.dum = TRUE.')
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

  # Original order (for sorting before return)
  dat$`__orig.order` <- 1:nrow(dat)

  # Sort 
  # prepIncorp() should require sorted ct 
  dat <- dat[order(dat$`__group`, dat[, time.name]), ]

  # Sort out incorporation inputs and pars
  if (prep.incorp) {
    # Default f4 value (for no incorporation in group, or incorporation only later)
    # If using flatout, __f4 should (must) already be in input data, and is only fixed at 1 if there is no incorporation
    # Below skipped for flatout == TRUE (must be done externally before calling alfam2())
    dat$`__add.row` <- FALSE 
    dat[, '__f4'] <- 1
    # Skipped for flatout == TRUE (must be done externally before calling alfam2())
    if (prep.incorp && !is.null(time.incorp)) {
      incprepout <- prepIncorp(dat, pars, time.name, time.incorp, incorp.names, warn)
      dat <- incprepout[['dat']]
      time.incorp <- incprepout[['time.incorp']]
    }
    if (tolower(value) == 'incorp') {
      if (warn) {
        warning('You set values = "incorp" so output does not include emission results.')
      }
      return(dat)
    }
  } else {
    if (warn) {
      warning('Skipping incorporation prep because you set prep.incorp = FALSE.\n   Incorporation will not be applied\n   (unless correct values for variables __f4 and __add.row are first added externally)')
    }
  }

  # Drop parameters for missing predictors
  p.orig <- pars
  ppnames <- gsub('\\.{1}[rf]{1}[0-9]$', '', names(pars))
  pars <- pars[predpres <- ppnames %in% names(dat) | ppnames == 'int']

  if(warn && check && any(!predpres)) {
    warning('Running with ', sum(predpres), ' parameters. Dropped ', sum(!predpres), ' with no match.\n',
            'These secondary parameters have been dropped:\n  ', 
            paste(names(p.orig)[!predpres], collapse = '\n  '), '\n\n',
            'These secondary parameters are being used:\n  ', 
            paste(names(p.orig)[predpres], collapse = '\n  '), '\n')
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
    # Trial run of par calcuations without upr to check for missing inputs
    # Calculate primary parameters
    if(length(which0) > 0) dat[, '__f0'] <- calcPParms(pars[which0], dat, warn = FALSE, tr = 'logistic') else dat[, '__f0'] <- 0
    if(length(which1) > 0) dat[, '__r1'] <- calcPParms(pars[which1], dat, warn = FALSE)                  else dat[, '__r1'] <- 0
    if(length(which2) > 0) dat[, '__r2'] <- calcPParms(pars[which2], dat, warn = FALSE)                  else dat[, '__r2'] <- 0
    if(length(which3) > 0) dat[, '__r3'] <- calcPParms(pars[which3], dat, warn = FALSE)                  else dat[, '__r3'] <- 0
    if(length(which5) > 0) dat[, '__r5'] <- calcPParms(pars[which5], dat, warn = FALSE)                  else dat[, '__r5'] <- 0
    if(length(which4) > 0) dat[dat[, '__f4'] == 0, '__f4'] <- calcPParms(pars[which4], dat[dat[, '__f4'] == 0, ], tr = 'logistic')

    if (any(anyNA(dat[, c('__f0', '__r1', '__r2', '__r3', '__f4', '__r5')]))) {
      if (check) {
        cat('Error!\n')
        cat('Missing values in predictors:\n')
        nn <- unique(names(pars[!grepl('^int', names(pars))]))
        print(apply(dat[, nn], 2, function(x) sum(is.na(x))))
        stop('NA values in primary parameters.\n   Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
      }
      if (warn) {
        cat('Warning!\n')
        cat('Missing values in predictors:\n')
        nn <- unique(names(pars[!grepl('^int', names(pars))]))
        print(apply(dat[, nn], 2, function(x) sum(is.na(x))))
        warning('NA values in primary parameters.\n   Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
      }
    }
  }

  # Calculate primary parameters
  if(length(which0) > 0) dat[, '__f0'] <- calcPParms(pars[which0], dat, warn = warn, tr = 'logistic') else dat[, '__f0'] <- 0
  if(length(which1) > 0) dat[, '__r1'] <- calcPParms(pars[which1], dat, warn = warn, upr = 1000)      else dat[, '__r1'] <- 0
  if(length(which2) > 0) dat[, '__r2'] <- calcPParms(pars[which2], dat, warn = warn, upr = 1E15)      else dat[, '__r2'] <- 0
  if(length(which3) > 0) dat[, '__r3'] <- calcPParms(pars[which3], dat, warn = warn)                  else dat[, '__r3'] <- 0
  if(length(which5) > 0) dat[, '__r5'] <- calcPParms(pars[which5], dat, warn = warn, upr = 1000)      else dat[, '__r5'] <- 0
  # f4 only calculated where it is already 0 (not default of 1)
  if(length(which4) > 0) dat[dat[, '__f4'] == 0, '__f4'] <- calcPParms(pars[which4], dat[dat[, '__f4'] == 0, ], tr = 'logistic')

  # Add drop row indicator
  dat$'__drop.row' <- dat$'__add.row' & !add.incorp.rows & check

  # Pare down to essential columns
  # No good reason for this anymore except debugging ease
  dat <- dat[, c('__group', '__orig.order', time.name, app.name, group, '__add.row', '__f4', '__f0', '__r1', '__r2', '__r3', '__r5', '__drop.row', pass.col)]

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

  if (check && !add.incorp.rows && prep.dum) {
    if (!is.null(dum) && nrow(dum) == nrow(ce)) {
      ce <- cbind(dum, ce)
    }
  }

  # Recalculate dt, e.int after possibly dropping rows and get j
  gstart <- match(unique(dat[, '__group']), dat[, '__group'])
  ct.prev <- c(0, ce[-nrow(ce), 'ct'])
  ct.prev[gstart] <- 0
  ce$dt <- ce[, 'ct'] - ct.prev

  e.prev <- c(0, ce$e[-nrow(ce)])
  e.prev[gstart] <- 0
  ce$e.int <- ce$e - e.prev
  ce$j <- ce$e.int/ce$dt

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

  # Get primary parameters (could be done in data.frame() below but there is name issue. . .
  ppars <- dat[, c('__f0', '__r1', '__r2', '__r3', '__f4', '__r5')]
  names(ppars) <- gsub('__', '', names(ppars))

  # Add other columns
  # If group not specified by user, group = NULL and is automatically left out
  out <- data.frame(dat[, c(group, pass.col), drop = FALSE],
                    ce, 
                    ppars,
                    row.names = NULL, check.names = FALSE)

  return(out)

}

