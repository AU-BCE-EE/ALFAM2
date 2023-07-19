# Function for running the model
# Use group = character name of column in dat to run by group
# time.incorp is name of column with incorporation time. Only first value (in each group) is used.
# group is name of group column, app.name is name of total pool (a0 + u0) column

alfam2 <- ALFAM2mod <- function(
  dat, 
  pars = ALFAM2::alfam2pars02, 
  add.pars = NULL,
  app.name = 'TAN.app', 
  time.name = 'ct', 
  time.incorp = NULL, # NULL with no incorporation, otherwise numeric value or column name. If column name value should be NA for no incorporation (w groups)
  group = NULL, 
  center = TRUE, 
  cmns = c(app.rate  = 40, 
           man.dm    =  6.0, 
           man.tan   =  1.2, 
           man.ph    =  7.5, 
           air.temp  = 13, 
           wind.2m   =  2.7, 
           crop.z    = 10), 
  check.NA = TRUE, 
  pass.col = NULL, 
  incorp.names = c('incorp', 'deep', 'shallow'),
  add.incorp.rows = FALSE, 
  prep = FALSE,
  warn = TRUE,
  parallel = FALSE, 
  n.cpus = 1,
  ...                 # Additional predictor variables with fixed values for all times and groups (all rows) (or the secret flatout = TRUE option)
  ) {

  # Add predictor variables if given in "..." optional arguments
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

  if (!flatout) {
    # Argument checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # NTS: Work needed here, add checks for all arguments
    # Convert data.table to data.frame
    if (class(dat)[1] %in% c('data.table', 'tbl_df')) {
      dat <- as.data.frame(dat)
    }
    checkArgClassValue(dat, expected.class = 'data.frame')
    checkArgClassValue(pars, expected.class = c('numeric', 'list'))
    checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))

    if (nrow(dat) == 0) stop('dat has no rows!')

    if (parallel) warning('parallel argument ignored >v2.1.3')
    if (!missing(n.cpus)) warning('n.cpus argument ignored >v2.1.3')

    # Warning if cmns is changed
    if (!identical(cmns, eval(formals(alfam2)$cmns))) {
      warning('You specified values for the cmns argument for centering means. Only use this option if you know what you are doing.')
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

    # Check for specified columns etc. *after* adding additional variables above
    if (any(is.na(dat[, c(time.name, app.name)]))) stop('Missing values in time or application rate columns.\nSee ', time.name, ' and ', app.name, ' columns.')

    # Fix negative times with a warning
    if (any(dat[, time.name] < 0)) {
      warning(paste0('Negative times (variable "', time.name, '") found and set to 0.'))
      dat[dat$time.name < 0, time.name] <- 0
    }

    # Prepare input data (dummy variables)
    if (prep) {
      dum <- prepDat(dat, value = 'dummy')
      #dat <- prepDat(dat, value = 'data')
      dat <- cbind(dat, dum)
    }

    # Tell user whether default or user-supplied parameters are in use
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
    if(any(chg.nms <- grepl("^[fr]{1}[0-4]{1}[.]", names(pars)))){
      names(pars)[chg.nms] <- gsub("^([fr][0-4])[.](.*)", "\\2\\.\\1", names(pars)[chg.nms])
    }

    # Additional pars that override or extend pars
    if (!is.null(add.pars)) {
      if (warn) {
        message('Additional parameters were specified.')
      }

      # If add.pars was given as list, change to vector
      if(is.list(add.pars)) {
        add.pars <- unlist(add.pars)
      }

      # Continue with add.pars conversion as with pars above
      if(any(chg.nms <- grepl("^[fr]{1}[0-4]{1}[.]", names(add.pars)))){
        names(add.pars)[chg.nms] <- gsub("^([fr][0-4])[.](.*)", "\\2\\.\\1", names(add.pars)[chg.nms])
      }

      # Combine add.pars with pars, with add.pars overriding pars
      pars <- c(add.pars, pars[!names(pars) %in% add.pars])
    }


    # Check that all names for pars end with a number
    if(any(!grepl('[0-9]$', names(pars)))) stop('One or more entries in argument "pars" cannot be assigned to parameters f0, r1, r2, r3, f4, r5.\n Make sure that the naming is correct. Either append the corresponding primary parameter or number (e.g., 0 to 4, or f0, r1) at the name endings (e.g. int.f0)\n or prepend the parameter separated by a dot (e.g. f0.int) or provide an appropriately named list.')

    # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
    # -> possibly extend names as done below?
    # Yup! Will work on.
    reserved.names <-  c('__group', '__add.row', '__f4', '__f0', '__r1', '__r2', '__r3', '__r5', '__drop.row', '__orig.order')
    if (any(names(dat) %in% reserved.names)) {
      warning('dat data frame has some columns with reserved names.\nYou can proceed, but there may be problems.\nBetter to remove/rename the offending columns: ', reserved.names[reserved.names %in% names(dat)])
    }

    # Remove non-existent pass.col columns if pass-through requested
    pass.col <- intersect(pass.col, names(dat))

  } # End flatout skip

  # If there is no grouping variable, add one to simplify code below (only one set, for groups)
  if(is.null(group)) {
    dat$`__group` <- 'a' 
  } else {
    dat$`__group` <- apply(dat[, group, drop = FALSE], 1, paste, collapse = "//")
  }

  # Center numeric predictors
  if(center) {
    # get columns that will be centered 
    c_cols <- names(cmns)[names(cmns) %in% names(dat)]

    # center
    if(length(c_cols)) {
      dat[, c_cols] <- scale(dat[, c_cols, drop = FALSE], center = cmns[c_cols], scale = FALSE)
      #dat[, c_cols] <- sweep(dat[, c_cols, drop = FALSE], 2, cmns[c_cols])
    }
  } else {
    # Warning if centering is turned off
    warning('You turned off centering by setting center = FALSE. Only use this option if you know what you are doing.')
  }

  # Original order (for sorting before return)
  dat$`__orig.order` <- 1:nrow(dat)


  # Sort out incorporation
  # Default f4 value (for no incorporation in group, or incorporation only later)
  # If using flatout, __f4 should (must) already be in input data
  # Skipped for flatout == TRUE (must be done externally before calling alfam2())
  dat$`__add.row` <- FALSE 
  dat[, '__f4'] <- 1
  if (!flatout) {

    if(!is.null(time.incorp)) {
      incprepout <- prepIncorp(dat, pars, time.name, time.incorp, incorp.names, warn)
      dat <- incprepout[['dat']]
      time.incorp <- incprepout[['time.incorp']]
    }

  }

  # Sort 
  dat <- dat[order(dat$`__group`, dat[, time.name]), ]

  # NTS: parameter calculation below could be moved to a separate function for simpler code in alfam2()
  # Drop parameters for missing predictors
  p.orig <- pars
  ppnames <- gsub('\\.{1}[rf]{1}[0-9]$', '', names(pars))
  pars <- pars[predpres <- ppnames %in% names(dat) | ppnames == 'int']

  if(!flatout && any(!predpres) && warn) {
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

  if(!flatout && !all(ww <- sort(c(which0, which1, which2, which3, which4, which5)) == 1:length(pars))) {
    stop('Something wrong with parameter argument p. ', paste(ww, collapse = ', '))
  }

  # Make sure parameter names can be found in dat
  if(!flatout && any(ncheck <- !(names(pars) %in% c('int', names(dat))))) stop ('Names in parameter vector pars not in dat (or not "int"): ', paste(names(pars)[ncheck], collapse = ', '))

  # Calculate primary parameters
  if(length(which0) > 0) dat[, "__f0"] <- calcPParms(pars[which0], dat, tr = 'logistic') else dat[, "__f0"] <- 0
  if(length(which1) > 0) dat[, "__r1"] <- calcPParms(pars[which1], dat, upr = 100)       else dat[, "__r1"] <- 0
  if(length(which2) > 0) dat[, "__r2"] <- calcPParms(pars[which2], dat)                  else dat[, "__r2"] <- 0
  if(length(which3) > 0) dat[, "__r3"] <- calcPParms(pars[which3], dat)                  else dat[, "__r3"] <- 0
  if(length(which5) > 0) dat[, "__r5"] <- calcPParms(pars[which5], dat, upr = 100)       else dat[, "__r5"] <- 0
  # f4 only calculated where it is already 0 (not default of 1)
  if(length(which4) > 0) dat[dat[, "__f4"] == 0, "__f4"] <- calcPParms(pars[which4], dat[dat[, "__f4"] == 0, ], tr = 'logistic') ##else dat[, "__f4"] <- 1

  # Add drop row indicator
  dat$"__drop.row" <- dat$"__add.row" & !add.incorp.rows

  # Missing values
  if(!flatout && check.NA && any(anyNA(dat[, c("__f0", "__r1", "__r2", "__r3", "__f4", "__r5")]))) {
    cat('Error!\n')
    cat('Missing values in predictors:\n')
    nn <- unique(names(pars[!grepl('^int', names(pars))]))
    print(apply(dat[, nn], 2, function(x) sum(is.na(x))))
    stop('NA values in primary parameters. Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
  }

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

  ce <- as.data.frame(ce)

  # Drop added rows unless requested through add.incorp.rows
  ce <- ce[!dat[, '__drop.row'], ]
  # Keep up with dat to use for sorting below
  dat <- dat[!dat[, '__drop.row'], ]

  if (!flatout && !add.incorp.rows && prep) {
    ce <- cbind(dum, ce)
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

  # Get primary parameters
  ppars <- dat[, c('__f0', '__r1', '__r2', '__r3', '__f4', '__r5')]
  names(ppars) <- gsub('__', '', names(ppars))

  # Sort to match original order, with any added row at end (might be better to keep time order in those cases)
  # NTS there was a , -1, what first col was dropped before? Prob group?
  ce <- ce[order(dat$`__orig.order`), ]
  # Keep up with dat for grouped operation below
  dat <- dat[order(dat$`__orig.order`), ]
  row.names(ce) <- seq.int(nrow(ce))

  # Add other columns
  # If group not specified by user, group = NULL and is automatically left out
  out <- data.frame(dat[, c(group, pass.col), drop = FALSE],
                    ce, 
                    ppars,
                    row.names = NULL, check.names = FALSE)

  return(out)

}

