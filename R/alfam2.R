# Function for running the model
# Use group = character name of column in dat to run by group
# time.incorp is name of column with incorporation time. Only first value (in each group) is used.
# group is name of group column, app.name is name of total pool (a0 + u0) column

alfam2 <- ALFAM2mod <- function(
  dat, 
  pars = ALFAM2::alfam2pars02, 
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
  ...                 # Additional predictor variables with fixed values for all times (all rows)
  ) {


  # NTS: Work needed here. 
  # Add checks for all arguments
  checkArgClassValue(dat, expected.class = 'data.frame')
  checkArgClassValue(pars, expected.class = c('numeric', 'list'))
  checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))

  if (nrow(dat) == 0) stop('dat has no rows!')

  if (parallel) warning('parallel argument ignored >v2.1.3')

  # Warning if cmns is changed
  if (!identical(cmns, eval(formals(alfam2)$cmns))) {
    warning('You specified values for the cmns argument for centering means. Only use this option if you know what you are doing.')
  }

  # Warning if centering is turned off
  if (!center) {
    warning('You turned off centering by setting center = FALSE. Only use this option if you know what you are doing.')
  }

  # Check for specified columns after adding additional variables
  # Add predictor variables if given in "..." optional arguments
  if (!missing(...)) {
    ovars <- list(...)
    dat <- data.frame(dat, ovars)
  }

  if (!app.name %in% names(dat)) {
    stop(paste0('app.name argument you specified (', app.name, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ', ')))
  }

  if (!time.name %in% names(dat)) {
    stop(paste0('time.name argument you specified (', time.name, ') is not present in dat data frame, which has these columns: ', paste(names(dat), collapse = ',')))
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
    # NTS: Also print par list?
  }

  # If pars was given as list, change to vector
  if(is.list(pars)) {
    pars <- unlist(pars)
  }

  # Continue with pars conversion, switch order for names that start with e.g. f0 or r3
  if(any(chg.nms <- grepl("^[fr]{1}[0-4]{1}[.]", names(pars)))){
    names(pars)[chg.nms] <- gsub("^([fr][0-4])[.](.*)", "\\2\\.\\1", names(pars)[chg.nms])
  }

  # Check that all names for pars end with a number
  if(any(!grepl('[0-9]$', names(pars)))) stop('One or more entries in argument "pars" cannot be assigned to parameters f0, r1, r2, r3, f4.\n Make sure that the naming is correct. Either append the corresponding primary parameter or number (e.g., 0 to 4, or f0, r1) at the name endings (e.g. int.f0)\n or prepend the parameter separated by a dot (e.g. f0.int) or provide an appropriately named list as argument.')

  # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
  # -> possibly extend names as done below?
  # Yup! Will work on.

  # Remove non-existent columns if pass-through requested
  pass.col <- intersect(pass.col, names(dat))

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
    if(length(c_cols)) dat[, c_cols] <- sweep(dat[, c_cols, drop = FALSE], 2, cmns[c_cols])
  }

  # Original order (for sorting before return)
  dat$orig.order <- 1:nrow(dat)

  # Extend dat data frame with incorporation time if needed
  dat$`__add.row` <- FALSE 

  # Default f4 value (for no incorporation in group, or incorporation only later)
  dat[, '__f4'] <- 1

  # Sort out incorporation
  if(!is.null(time.incorp)) {

    # Get actual incorporation parameter names (if any) from parameters
    inc.names <- unique(gsub("\\.{1}[rf]{1}[0-4]$", "", unlist(mapply(function(x) grep(x, names(pars), value = TRUE), incorp.names))))

    if(length(inc.names) > 0){

      # Do they exist?
      inc.ex <- intersect(inc.names, names(dat))

      # Get times and types
      if(is.numeric(time.incorp)){
        # Unique groups
        u.group <- unique(dat$`__group`)
        # Repeat time.incorp values to match number of groups
        incorp.time <- rep(time.incorp, length(u.group))[seq_along(u.group)] # NTS: why is [] needed?
        names(incorp.time) <- u.group
      } else {
        # Get time.incorp column entries (first row, so rows underneath could have time.incorp = NA etc., all ignored
        incorp.time <- tapply(dat[, time.incorp], dat$`__group`, "[", 1)
      }

      # Get number of incorp columns by group for 1) checking that there is not more than 1 applied, 2) skipping incorp when there is none (even when t.incorp may have a value)
      n.incorp.vals <- rowSums(dat[, inc.ex, drop = FALSE])
      n.incorp.vals.grp <- tapply(n.incorp.vals, dat$`__group`, "[", 1)

      # If multiple incoporation dummy variables are 1 for any row, throw error
      if (any(rowSums(dat[, inc.ex, drop = FALSE]) > 1)) {
        stop('Multiple incorporation types specified in the same row--this cannot be done with ALFAM2mod().')
      }

      # Check if columns exist
      if(length(inc.ex) == 0){
        if (warn) {
          warning("No matching column for incorporation parameter(s): ", paste(inc.names, collapse = ", "), ". Skipping incorporation.")
        }
        time.incorp <- NULL
      }

    } else {
      if (warn) {
        warning("No incorporation parameters have been provided. Skipping incorporation.")
      }
      time.incorp <- NULL
    }

    # Add incorporation rows as needed
    dat$`__add.row` <- FALSE
    if(!is.null(time.incorp)) {

      # Loop through groups with incorporation (incorp.time != NA)
      for(i in names(incorp.time)[!is.na(incorp.time) & n.incorp.vals.grp > 0]) {

        sub.dat <- dat[dat$`__group` == i, ]

        # Extract cumulative time
        ct <- sub.dat[, time.name]

        # Find where incorporation occurs
        ct.ind <- which(ct > incorp.time[i])[1]

        # Add rows
        if(is.na(ct.ind)){
        
          if (warn) {
            warning('Incorporation takes place after the end of the last interval and will be ignored (group ', i, ').')
          }
        
        } else if(ct.ind == 1){

          if (warn) {
            message('Incorporation applied (for group ', i, ').')
          }

          # Use predictor values from first row
          ins.dat <- sub.dat[1, ]
          ins.dat[, time.name] <- incorp.time[i] 
          ins.dat$`__add.row` <- TRUE
          sub.dat[1, '__f4'] <- 0
          dat <- rbind(dat[dat$`__group` != i, ], sub.dat, ins.dat)          

        } else if(incorp.time[i] != ct[ct.ind - 1]){

          # Insert a row before incorp, interval ends at incorp.time
          ins.dat <- sub.dat[ct.ind, ]
          ins.dat[, time.name] <- incorp.time[i] 
          ins.dat$`__add.row` <- TRUE
          sub.dat[ct.ind, '__f4'] <- 0
          dat <- rbind(dat[dat$`__group` != i, ], sub.dat, ins.dat)          

        } else {

          sub.dat[ct.ind, '__f4'] <- 0
          dat <- rbind(dat[dat$`__group` != i, ], sub.dat)          

        }

        # Set incorp variables to FALSE for time <= incorp.time (incorp then applied at start of next interval)
        dat[dat$`__group` == i & dat[, time.name] <= incorp.time[i], inc.ex] <- FALSE

      }

      # Also set incorp to FALSE for all rows within groups with incorp.time = NA
      for(i in names(incorp.time)[is.na(incorp.time)]) {
        dat[dat$`__group` == i & is.na(incorp.time[i]), inc.ex] <- FALSE

      }

    }

  }

  # Sort 
  dat <- dat[order(dat$`__group`, dat[, time.name]), ]

  # Drop parameters for missing predictors
  p.orig <- pars
  ppnames <- gsub('\\.{1}[rf]{1}[0-9]$', '', names(pars))
  pars <- pars[predpres <- ppnames %in% names(dat) | ppnames == 'int']

  if(any(!predpres) & warn) {
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

  names(pars) <- gsub('\\.[rf][0-9]$', '', names(pars))

  if(!all(ww <- sort(c(which0, which1, which2, which3, which4)) == 1:length(pars))) {
    stop('Something wrong with parameter argument p. ', paste(ww, collapse = ', '))
  }

  # Make sure parameter names can be found in dat
  if(any(ncheck <- !(names(pars) %in% c('int', names(dat))))) stop ('Names in parameter vector pars not in dat (or not "int"): ', paste(names(pars)[ncheck], collapse = ', '))

  # Calculate primary parameters
  if(length(which0) > 0) dat[, "__f0"] <- calcPParms(pars[which0], dat, tr = 'logistic') else dat[, "__f0"] <- 0
  if(length(which1) > 0) dat[, "__r1"] <- calcPParms(pars[which1], dat)                  else dat[, "__r1"] <- 0
  if(length(which2) > 0) dat[, "__r2"] <- calcPParms(pars[which2], dat)                  else dat[, "__r2"] <- 0
  if(length(which3) > 0) dat[, "__r3"] <- calcPParms(pars[which3], dat)                  else dat[, "__r3"] <- 0
  # f4 only calculated where it is already 0 (not default of 1)
  if(length(which4) > 0) dat[dat[, "__f4"] == 0, "__f4"] <- calcPParms(pars[which4], dat[dat[, "__f4"] == 0, ], tr = 'logistic') ##else dat[, "__f4"] <- 1

  # Add drop row indicator
  dat$"__drop.row" <- dat$"__add.row" & !add.incorp.rows

  # Missing values
  if(check.NA && any(anyNA(dat[, c("__f0", "__r1", "__r2", "__r3", "__f4")]))) {
    cat('Error!\n')
    cat('Missing values in predictors:\n')
    print(apply(dat[, unique(names(pars[!grepl('^int', names(pars))]))], 2, function(x) sum(is.na(x))))
    stop('NA values in primary parameters. Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
  }

  # Pare down to essential columns
  dat <- dat[, c('__group', 'orig.order', time.name, app.name, group, '__add.row', '__f4', '__f0', '__r1', '__r2', '__r3', '__drop.row', pass.col)]
  # Split into list of data frames
  s.dat <- split(dat, dat$`__group`)

  e.list <- vector("list", length(s.dat))

  for(i in seq_along(s.dat)) {
    # get subset
    sub.dat <- s.dat[[i]]
    # Check for duplicate ct
    if(any(duplicated(sub.dat[!sub.dat$`__add.row`, time.name]))) {
      stop('Look for 998123b in function code. Duplicated ct values.')
    }

    # calculate emission
    drop.rows <- sub.dat$`__drop.row`
    ce <- calcEmis(
      ct = sub.dat[, time.name],
      # Calculate a0 and u0 (f4 transfers done in calcEmis())
      a0 = sub.dat[1, "__f0"]*sub.dat[1, app.name],
      u0 = (1 - sub.dat[1, "__f0"])*sub.dat[1, app.name],
      r1 = sub.dat[, "__r1"],
      r2 = sub.dat[, "__r2"],
      r3 = sub.dat[, "__r3"],
      f4 = sub.dat[, "__f4"], drop.rows = drop.rows)

    # add group
    e.list[[i]] <- data.frame(orig.order = sub.dat[!drop.rows, "orig.order"], 
                              sub.dat[!drop.rows, group, drop = FALSE],
                              ce, 
                              sub.dat[!drop.rows, pass.col, drop = FALSE],
                              row.names = NULL, check.names = FALSE)
  }

  # rbind e.list to data.frame
  e <- do.call("rbind", e.list)

  # Sort to match original order NTS how does this work with add.incorp.rows = TRUE?
  out <- e[order(e$orig.order), -1]
  row.names(out) <- seq.int(nrow(out))

  if (!add.incorp.rows & prep) {
    out <- cbind(dum, out)
  }

  return(out)

}

