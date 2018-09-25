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
  incorp.names = c('^incorp', 'deep', 'shallow'),
  add.incorp.rows = FALSE, 
  parallel = FALSE, 
  n.cpus = 1
  ) {

  ###### NTS: not package-ready
  ##if(parallel) {
  ##  requireNamespace("parallel") 
  ##}

  #print(pars)

  # NTS: Work needed here. 
  # Add checks for all arguments
  checkArgClassValue(dat, expected.class = 'data.frame')
  checkArgClassValue(pars, expected.class = c('numeric', 'list'))
  checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))

  # If pars was given as list, change to vector
  if(is.list(pars)) {
    pars <- unlist(pars)
  }

  if(any(ch_nms <- grepl("[fr]{1}[0-5]{1}[.]", names(pars)))){
    names(pars)[ch_nms] <- gsub("^[fr]([0-5])[.](.*)", "\\2\\1", names(pars)[ch_nms])
  }

  # Check that all names for p end with a number
  if(any(!grepl('[0-9]$', names(pars)))) stop('One or more entries in argument "pars" cannot be assigned to parameters f0, r1, r2, r3, r4, r5.\n Make sure that the naming is correct. Either append the corresponding number (0 to 5) at the name endings (e.g. int0)\n or prepend the parameter separated by a dot (e.g. f0.int) or provide an appropriately named list as argument.')

  # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
  # -> possibly extend names as done below?

  # Rename pass-through column if pass-through requested
  if(!is.null(pass.col)) {
    dat[, paste0("pass_me.through_", pass.col)] <- dat[, pass.col]
  }

  # If there is no grouping variable, add one to simplify code below (only one set, for groups)
  if(is.null(group)) {
    dat$group <- 'a' 
  } else {
    dat$group <- as.character(dat[, group])
  }

  # Center numeric predictors
  if(center) {
    # get columns that will be centered 
    c_cols <- names(cmns)[names(cmns) %in% names(dat)]
    # center
    if(length(c_cols)) dat[, c_cols] <- sweep(dat[, c_cols], 2,cmns[c_cols])
  }

  # Original order (for sorting before return)
  dat$orig.order <- 1:nrow(dat)

  # Extend dat data frame with incorporation time if needed
  dat$added.row <- FALSE # NTS: problem if dat already has column with this name. Need to check names.


  # Sort out incorporation
  if(!is.null(time.incorp)) {

    # Get actual incorporation parameter names (if any) from parameters
    inc.names <- unique(gsub("[0-5]$", "", grep(incorp.names, names(pars), value = TRUE)))

    if(length(inc.names) > 0){

      # Do they exist?
      inc.ex <- inc.names[inc.names %in% names(dat)]

      # Unique groups
      u.group <- unique(dat$group)

      # Get times and types
      if(is.numeric(time.incorp)){
        # Repeat time.incorp values to match number of groups
        incorp.time <- rep(time.incorp, length(u.group))[seq_along(u.group)] # NTS: why is [] needed?
        names(incorp.time) <- u.group
      } else {
        # Get time.incorp column entries
        incorp.time <- tapply(dat[, time.incorp], dat$group, "[", 1)
      }

      # Check if columns exist
      if(length(inc.ex) == 0){
        warning("No matching column for incorporation parameter(s): ", paste(inc.names, collapse = ", "), ". Skipping incorporation.")
        time.incorp <- NULL
      } ##else {
        ### set incorporation column to FALSE before incorp.time
        ##for(i in seq_along(u.group)){
        ##  # for each group
        ##  dat[dat$group == u.group[i] & dat[, time.name] < incorp.time[i], inc.ex] <- FALSE
        ##}
      ##}
    } else {
      warning("No incorporation parameter estimates have been provided. Skipping incorporation.")
      time.incorp <- NULL
    }

    # Add incorporation rows as needed
    dat$added.row <- FALSE
    if(!is.null(time.incorp)) {

      # Loop through groups with incorporation (incorp.time != NA)
      for(i in u.group[!is.na(incorp.time)]) {

        sub.dat <- dat[dat$group == i, ]

        # Extract cumulative time time
        ct <- sub.dat[, time.name]

        # Find where incorporation occurs
        ct.ind <- which(ct > incorp.time[i])[1] - 1

        # Add rows        
        if(ct.ind == 0){

          # Use predictor values from first row
          ins.dat <- sub.dat[1, ]
          ins.dat[, time.name] <- incorp.time[i] 
          ins.dat$added.row <- TRUE
          dat <- rbind(dat, ins.dat)          

        } else if(ct.ind == length(ct)){

          warning("incorporation takes place after the end of the last interval and will be ignored")

        } else if(any(incorp.time[i] == ct)){ # NTS need to check that length == 1

          # Nothing?
          # dat[dat$group == i & dat[, time.name] == ?????? incorp.time[i], '__f5'] <- 1

        } else {

          # Insert a row before incorp, interval ends at incorp.time
          ins.dat <- sub.dat[ct.ind + 1, ]
          ins.dat[, time.name] <- incorp.time[i] 
          ins.dat$added.row <- TRUE
          dat <- rbind(dat, ins.dat)          

        }

        # Set incorp variables to FALSE for time <= incorp.time (incorp then applied at start of next interval)
        dat[dat$group == i & dat[, time.name] <= incorp.time[i], inc.ex] <- FALSE

        # Set f5 to 1 for intervals with incorporation (at start)

      }

    }

  }

  # Sort 
  dat <- dat[order(dat$group, dat[, time.name]), ]

  # Default f5 value (for no incorporation in group, or incorporation only later)
  dat[, '__f5'] <- 1

  # Allow f5 value only for intervals that have a *start* time (end time of previous interval) equal to incorp.time
  if(!is.null(time.incorp)) {

    # Loop through groups with incorporation (incorp.time != NA)
    for(i in u.group[!is.na(incorp.time)]) {
      ng <- nrow(dat[dat$group == i, ])
      dat[dat$group == i & c(0, dat[-ng, time.name]) == incorp.time[i], '__f5'] <- 0
      # Incorp FALSE unless previous interval ends at incorp.time
      dat[dat$group == i & c(0, dat[-ng, time.name]) < incorp.time[i], inc.ex] <- FALSE 
    }

  }

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
    stop('Something wrong with parameter argument p. ', paste(ww, collapse = ', '))
  }

  # Make sure parameter names can be found in dat
  if(any(ncheck <- !(names(pars) %in% c('int', names(dat))))) stop ('Names in parameter vector pars not in dat (or not "int"): ', paste(names(pars)[ncheck], collapse = ', '))

  # Calculate primary parameters
  if(length(which0) > 0) dat[, "__f0"] <- calcPParms(pars[which0], dat, tr = 'logistic') else dat[, "__f0"] <- 0
  if(length(which1) > 0) dat[, "__r1"] <- calcPParms(pars[which1], dat)                  else dat[, "__r1"] <- 0
  if(length(which2) > 0) dat[, "__r2"] <- calcPParms(pars[which2], dat)                  else dat[, "__r2"] <- 0
  if(length(which3) > 0) dat[, "__r3"] <- calcPParms(pars[which3], dat)                  else dat[, "__r3"] <- 0
  # f5 only calculated where it is already 0 (not default of 1)
  if(length(which5) > 0) dat[dat[, "__f5"] == 0, "__f5"] <- calcPParms(pars[which5], dat[dat[, "__f5"] == 0, ], tr = 'logistic') ##else dat[, "__f5"] <- 1

  # split dat into groups
  s.dat <- split(dat, dat$group)

  if(check.NA && sapply(s.dat, function(x) anyNA(x[, c("__f0", "__r1", "__r2", "__r3", "__f5")]))) {
    cat('Missing values in predictors:\n')
    print(apply(dat[, unique(names(pars[!grepl('^int', names(pars))]))], 2, function(x) sum(is.na(x))))
    stop('NA values in primary parameters. Look for missing values in predictor variables (in dat) and double-check parameters agaist dat column names')
  }

  # Parallel
  if(parallel) {

    # starting cluster and trigger stop on.exit
    cl <- parallel::makeCluster(n.cpus, type = "SOCK")
    on.exit(parallel::stopCluster(cl))

    # sorting input for efficiency
    s.nr <- sapply(s.dat, nrow)
    do.nr <- order(s.nr, decreasing = TRUE)
    e.list <- vector("list", length(s.dat))

    # do parallel
    # parallel::clusterExport(cl, c("calcEmis", "time.name", "app.name")) 
    e.list[do.nr] <- parallel::clusterApply(cl, s.dat[do.nr], function(sub.dat){
      data.frame(group = sub.dat[!sub.dat$added.row, "group"], calcEmis(
        ct = sub.dat[, time.name]
        # Calculate a0 and u0 (f5 transfers done in calcEmis())
        ,a0 = sub.dat[1, "__f0"]*sub.dat[1, app.name]
        ,u0 = (1 - sub.dat[1, "__f0"])*sub.dat[1, app.name]
        ,r1 = sub.dat[, "__r1"]
        ,r2 = sub.dat[, "__r2"]
        ,r3 = sub.dat[, "__r3"]
        ,f5 = sub.dat[, "__f5"]
        ,drop.rows = sub.dat$added.row & !add.incorp.rows)
      , row.names = NULL, check.names = FALSE)    
    })

    # stop cluster and empty on.exit
    parallel::stopCluster(cl)
    on.exit()

  } else {

    e.list <- vector("list", length(s.dat))

    for(i in seq_along(s.dat)) {
      # get subset
      sub.dat <- s.dat[[i]]
      # Check for duplicate ct
      if(any(duplicated(sub.dat[!sub.dat$added.row, time.name]))) {
        stop('Look for 998123b in pmod.R. Duplicated ct values.')
      }

      # calculate emission
      ce <- calcEmis(
        ct = sub.dat[, time.name]
        # Calculate a0 and u0 (f5 transfers done in calcEmis())
        ,a0 = sub.dat[1, "__f0"]*sub.dat[1, app.name]
        ,u0 = (1 - sub.dat[1, "__f0"])*sub.dat[1, app.name]
        ,r1 = sub.dat[, "__r1"]
        ,r2 = sub.dat[, "__r2"]
        ,r3 = sub.dat[, "__r3"]
        ,f5 = sub.dat[, "__f5"], drop.rows = sub.dat$added.row & !add.incorp.rows)

      # add group
      e.list[[i]] <- data.frame(orig.order = sub.dat[!(sub.dat$added.row & !add.incorp.rows), "orig.order"], 
                                group = sub.dat[!(sub.dat$added.row & !add.incorp.rows), "group"], ce, row.names = NULL, check.names = FALSE)
      ##e.list[[i]] <- data.frame(orig.order = sub.dat[, "orig.order"], group = sub.dat[, "group"], ce, row.names = NULL, check.names = FALSE)
    } 
  }

  # rbind e.list to data.frame
  e <- do.call("rbind", e.list)

  # rename 'group' column
  if(!is.null(group)){
    names(e)[2] <- group
  }

  # Sort to match original order
  e <- e[order(e$orig.order), -1]

  # Add pass-through column if requested
  if(!is.null(pass.col)) {
    e <- data.frame(setNames(dat[!dat$added.row, paste0("pass_me.through_", pass.col)], pass.col), e)
  }

  return(e)

}

