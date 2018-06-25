# Function for running the model
# Use group = character name of column in dat to run by group
# time.incorp is name of column with incorporation time. Only first value (in each group) is used.
# group is name of group column, app.name is name of total pool (a0 + u0) column
ALFAM2mod <- function(
  dat, 
  pars = c(
           int0           = -0.91400,
           int1           = -1.16256,
           int2           = -1.02444,
           int3           = -2.92947,
           app.methodos0  = -0.98384,
           app.rate0      = -0.01602,
           man.dm0        =  0.40164,
           incorpdeep5    = -3.08108,
           incorpshallow5 = -0.91376,
           app.methodbc1  =  0.62870,
           man.dm1        = -0.07974,
           air.temp1      =  0.04909,
           wind.1m1       =  0.04876,
           air.temp3      =  0.01344,
           incorpdeep3    = -0.74621,
           app.methodos3  = -0.20088,
           rain.rate2     =  0.38434
           ), 
  app.name = 'TAN.app', 
  time.name = 'ct',
  time.incorp = NULL,                     # NULL with no incorporation, otherwise numeric or column name. If column name value should be NA for no incorporation (w groups)
  group = NULL, 
  center = TRUE,
  cmns = c(app.rate  = 40, 
           man.dm = 6, 
           man.tan = 1.2, 
           man.ph = 7.5, 
           air.temp = 13, 
           wind.1m = 2.7, 
           lwind = 0.43, 
           crop.z = 10),
  check.NA = TRUE, 
  pass.col = NULL,
  add.incorp.rows = FALSE,
  parallel = FALSE
  ) {

  #### NTS: not package-ready
  if(parallel) {
    library(foreach)
    library(parallel) # NTS: shouldn't need, since in R-core??? -> hac: but you still have to load it, unless we det depend on those libraries...
    library(doParallel)
  }

  #print(pars)

  # NTS: Work needed here. 
  # Add checks for all arguments
  checkArgClassValue(dat, expected.class = 'data.frame')
  checkArgClassValue(pars, expected.class = c('numeric', 'list'))

  checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))

  # If pars was given as list, change to vector
  if(is.list(pars)) {

    dnames <- as.numeric(gsub('[fr]', '', names(pars)))
    p.list <- pars
    pars <- NULL

    for(i in 1:length(p.list)) {
      spars <- p.list[[i]]
      names(spars) <- paste0(names(spars), dnames[i])
      pars <- c(pars, spars)
    }

  }

  # Check that all names for p end with a number
  if(any(!grepl('[0-9]$', names(pars)))) stop('One or more names in argument "pars" does not end with a number.')

  # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
  # -> possibly extend names as done below?

  # Rename pass-through column if pass-through requested
  if(!is.null(pass.col)) {
    dat[,paste0("pass_me.through_",pass.col)] <- dat[,pass.col]
  }

  # If there is no grouping variable, add one to simplify code below (only one set, for groups)
  if(is.null(group)) {
    dat$group <- 0
  } else {
    dat$group <- as.character(dat[,group])
  }

  # Center numeric predictors
  if(center) {
    # get columns that will be centered 
    c_cols <- names(cmns)[names(cmns) %in% names(dat)]
    # center
    if(length(c_cols)) dat[,c_cols] <- sweep(dat[,c_cols],2,cmns[c_cols])
  }

  # Original order (for sorting before return)
  dat$orig.order <- 1:nrow(dat)

  # Extend dat data frame with incorporation time if needed
  dat$ievent <- dat$added.row <- FALSE # NTS: problem if dat already has column with this name

  if(!is.null(time.incorp)) {

    # Add numeric time.incorp to data frame dat (column needed to handle groups)
    if(is.numeric(time.incorp)) {
      dat$time.incorp <- time.incorp
      time.incorp <- 'time.incorp' # NTS: really a name, change arg name to t.incorp.name?
    }

    # Add incorporation times -> hac: how about using a vector as input argument: e.g. c(group1=4, group2=2,...)
    # or even a list: e.g. list(group1=c("deep",4),group2=c("shallow",10))
    if(!is.null(time.incorp)) {
      for(i in sort(unique(dat$group))) {
        dd <- dat[dat$group == i, ]
        tt <- dd[1, time.incorp]

        if(!is.na(tt)) { 

          # If exact time is already present, no need to add row
          if(!tt %in% dd[, time.name]) {
            irow <- dd[1, ]
            irow$added.row <- TRUE
            irow[, time.name] <- tt
            dat <- rbind(dat, irow)
          } 

          # Identify time of incorporation event
          dat[dat$group == i & dat[, time.name] == tt, 'ievent'] <- TRUE

        }

      }
    }

  }

  # Sort (time must increase for calcEmis())
  dat <- dat[order(dat$group, dat[, time.name]), ]

  # Drop parameters for missing predictors
  p.orig <- pars
  ppnames <- gsub('[0-9]$', '', names(pars))
  pars <- pars[predpres <- ppnames %in% names(dat) | ppnames == 'int']
  if(any(!predpres)) {
    warning('Missing predictors. These secondary parameters have been dropped: ', paste(names(p.orig)[!predpres], collapse = ', '))
  }

  # Associate (secondary) parameters with primary parameters (r1, etc.)
  which0 <- grep('0$', names(pars)) # For f0
  which1 <- grep('1$', names(pars)) # For r1
  which2 <- grep('2$', names(pars)) # For r2
  which3 <- grep('3$', names(pars)) # For r3
  which5 <- grep('5$', names(pars)) # For a to u transfer at specific times, incorporation, will be applied once only!
  names(pars) <- gsub('[0-9]$', '', names(pars))
  if(!all(ww <- sort(c(which0, which1, which2, which3, which5)) == 1:length(pars))) {
    stop('Something wrong with p. ', paste(ww, collapse = ', '))
  }

  # Make sure parameter names can be found in dat
  if(any(ncheck <- !(names(pars) %in% c('int', names(dat))))) stop ('Names in parameter vector pars not in dat (or not "int"): ', paste(names(pars)[ncheck], collapse = ', '))

  # Calculate primary parameters (zero by default)
  zv <- rep(0, nrow(dat))
  if(length(which0) > 0) f0 <- calcPParms(pars[which0], dat, tr = 'logistic') else f0 <- zv
  if(length(which1) > 0) r1 <- calcPParms(pars[which1], dat)                  else r1 <- zv
  if(length(which2) > 0) r2 <- calcPParms(pars[which2], dat)                  else r2 <- zv
  if(length(which3) > 0) r3 <- calcPParms(pars[which3], dat)                  else r3 <- zv
  if(length(which5) > 0) f5 <- calcPParms(pars[which5], dat, tr = 'logistic') else f5 <- zv

  # f5 only applies when incorporation occurs (0 or 1 time per group), otherwise, 100% stays in f/a
  f5[!dat$ievent] <- 1

  if(check.NA) if(any(is.na(c(f0, r1, r2, r3, f5)))) {
    cat('Missing values in predictors:\n')
    print(apply(dat[, unique(names(pars[!grepl('^int', names(pars))]))], 2, function(x) sum(is.na(x))))
    stop('NA values in primary parameters. Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
  }

  
  # After calculating f5, set incorporation predictor variables to FALSE for times before incorporation occurred
  if(!is.null(time.incorp)) {
    for(i in sort(unique(dat$group))) {
      dd <- dat[dat$group == i, ]
      tt <- dd[1, time.incorp]

      if(!is.na(tt)) { 

    # NTS: problematic
        dat[dat$group == i & dat[, time.name] <= tt, grepl('incorp', names(dat)) & names(dat) %in% gsub('[0-9]$', '', names(pars))] <- FALSE

      } else {

        # NTS: does this really fix problem wehn there is no incorp?
        f5[dat$group == i] <- 1

      }

    }
  }

  # e is output data frame 
  e <- NULL

  # Not parallel
  if(!parallel) {
    for(i in sort(unique(dat$group))) {
      dd <- dat[dat$group == i, ]
      ff0 <- f0[dat$group == i]
      rr1 <- r1[dat$group == i]
      rr2 <- r2[dat$group == i]
      rr3 <- r3[dat$group == i]
      ff5 <- f5[dat$group == i]

      # Check for duplicate ct
      if(any(duplicated(dd[, time.name]))) {
        stop('Look for 998123b in pmod.R. Duplicated ct values.')
      }

      # Calculate a0 and u0 (f5 transfers done in calcEmis())
      u0 <- (1 - ff0[1])*dd[, app.name][1]
      a0 <- ff0[1]*dd[, app.name][1]
      ct <- dd[, time.name]
      drop.rows <- dd$added.row
      if(add.incorp.rows) drop.rows <- rep(FALSE, length(drop.rows))
      ce <- calcEmis(ct = ct, a0 = a0, u0 = u0, r1 = rr1, r2 = rr2, r3 = rr3, f5 = ff5, ievent = dd$ievent, drop.rows = drop.rows)
      e <- rbind(e, cbind(group = i, ce))
    }
  } else {
    e <- foreach(i = sort(unique(dat$group)), .packages = 'minpack.lm', .export = 'calcEmis', .combine = rbind, .init = NULL) %dopar% {
    dd <- dat[dat$group == i, ]
    ff0 <- f0[dat$group == i]
    rr1 <- r1[dat$group == i]
    rr2 <- r2[dat$group == i]
    rr3 <- r3[dat$group == i]
    ff5 <- f5[dat$group == i]

    # Check for duplicate ct
    if(any(duplicated(dd[, time.name]))) {
      stop('Look for 998123b in pmod.R. Duplicated ct values.')
    }

    # Calculate a0 and u0 (f5 transfers done in calcEmis())
    u0 <- (1 - ff0[1])*dd[, app.name][1]
    a0 <- ff0[1]*dd[, app.name][1]
    ct <- dd[, time.name]
    drop.rows <- dd$added.row
    if(add.incorp.rows) drop.rows <- rep(FALSE, length(drop.rows))
    ce <- calcEmis(ct = ct, a0 = a0, u0 = u0, r1 = rr1, r2 = rr2, r3 = rr3, f5 = ff5, ievent = dd$ievent, drop.rows = drop.rows)

      cbind(group = i, ce)
    }
 }
  
  if(!is.null(group)){
    names(e)[1] <- group
  }

  # Sort to match original order
  # NTS: check that this works
  drop.rows <- dat$added.row
  if(add.incorp.rows) drop.rows <- rep(FALSE, length(drop.rows))
  e <- e[order(dat$orig.order[!drop.rows]), ]

  # Add pass-through column if requested
  if(!is.null(pass.col)) {
    e <- data.frame(setNames(dat[, paste0("pass_me.through_",pass.col)],pass.col), e)
  }

  return(e)

}

