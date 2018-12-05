# Function for running the model
# Use group = character name of column in dat to run by group
# time.incorp is name of column with incorporation time. Only first value (in each group) is used.
# group is name of group column, app.name is name of total pool (a0 + u0) column

ALFAM2mod <- function(
  dat, 
  pars = c(int0           = -0.7364889,
           int1           = -1.1785848,
           int2           = -0.9543731,
           int3           = -2.9012937,
           app.methodos0  = -1.1717859,
           app.rate0      = -0.0134681,
           man.dm0        =  0.407466,
           incorpdeep4    = -3.6477259,
           incorpshallow4 = -0.4121023,
           app.methodbc1  =  0.6283396,
           man.dm1        = -0.075822,
           air.temp1      =  0.0492777,
           wind.2m1       =  0.0486651,
           man.ph1        =  0.5327231,
           air.temp3      =  0.0152419,
           incorpdeep3    = -0.3838862,
           app.methodos3  = -0.122883,
           man.ph3        =  0.2663616,
           rain.rate2     =  0.4327281,
           rain.cum3      = -0.0300936), 
  app.name = 'TAN.app', 
  time.name = 'ct', 
  time.incorp = NULL, # NULL with no incorporation, otherwise numeric or column name. If column name value should be NA for no incorporation (w groups)
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
  parallel = FALSE, 
  n.cpus = 1
  ) {


  # NTS: Work needed here. 
  # Add checks for all arguments
  checkArgClassValue(dat, expected.class = 'data.frame')
  checkArgClassValue(pars, expected.class = c('numeric', 'list'))
  checkArgClassValue(time.incorp, expected.class = c('character', 'numeric', 'integer', 'NULL'))

  # If pars was given as list, change to vector
  if(is.list(pars)) {
    pars <- unlist(pars)
  }

  # Continue with change, switch order for names that start with e.g. f0 or r3
  if(any(chg.nms <- grepl("^[fr]{1}[0-4]{1}[.]", names(pars)))){
    names(pars)[chg.nms] <- gsub("^[fr]([0-4])[.](.*)", "\\2\\1", names(pars)[chg.nms])
  }

  # Check that all names for pars end with a number
  if(any(!grepl('[0-9]$', names(pars)))) stop('One or more entries in argument "pars" cannot be assigned to parameters f0, r1, r2, r3, f4.\n Make sure that the naming is correct. Either append the corresponding number (0 to 4) at the name endings (e.g. int0)\n or prepend the parameter separated by a dot (e.g. f0.int) or provide an appropriately named list as argument.')

  # Check predictor names to make sure they don't match reserved names (group, incorporation, etc.)
  # -> possibly extend names as done below?
  # Yup! Will work on.

  # Rename pass-through column if pass-through requested
  # NTS: why?
  if(!is.null(pass.col)) {
    dat[, paste0("pass_me.through_", pass.col)] <- dat[, pass.col]
  }

  # If there is no grouping variable, add one to simplify code below (only one set, for groups)
  if(is.null(group)) {
    dat$`__group` <- 'a' 
  } else {
    dat$`__group` <- as.character(dat[, group])
  }

  # Center numeric predictors
  if(center) {
    # get columns that will be centered 
    c_cols <- names(cmns)[names(cmns) %in% names(dat)]

    # center
    if(length(c_cols)) dat[, c_cols] <- sweep(dat[, c_cols], 2, cmns[c_cols])
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
    inc.names <- unique(gsub("[0-4]$", "", unlist(mapply(function(x) grep(x, names(pars), value = TRUE), incorp.names))))

    if(length(inc.names) > 0){

      # Do they exist?
      inc.ex <- inc.names[inc.names %in% names(dat)]


      # Get times and types
      if(is.numeric(time.incorp)){
        # Unique groups
        u.group <- unique(dat$`__group`)
        # Repeat time.incorp values to match number of groups
        incorp.time <- rep(time.incorp, length(u.group))[seq_along(u.group)] # NTS: why is [] needed?
        names(incorp.time) <- u.group
      } else {
        # Get time.incorp column entries
        incorp.time <- tapply(dat[, time.incorp], dat$`__group`, "[", 1)
      }

      # Check if columns exist
      if(length(inc.ex) == 0){
        warning("No matching column for incorporation parameter(s): ", paste(inc.names, collapse = ", "), ". Skipping incorporation.")
        time.incorp <- NULL
      }

    } else {
      warning("No incorporation parameter estimates have been provided. Skipping incorporation.")
      time.incorp <- NULL
    }

    # Add incorporation rows as needed
    dat$`__add.row` <- FALSE
    if(!is.null(time.incorp)) {

      # Loop through groups with incorporation (incorp.time != NA)
      for(i in names(incorp.time)[!is.na(incorp.time)]) {

        sub.dat <- dat[dat$`__group` == i, ]

        # Extract cumulative time time
        ct <- sub.dat[, time.name]

        # Find where incorporation occurs
        ct.ind <- which(ct > incorp.time[i])[1]

        # Add rows
        if(is.na(ct.ind)){
        
          warning("incorporation takes place after the end of the last interval and will be ignored")
        
        } else if(ct.ind == 1){

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

    }

  }

  # Sort 
  dat <- dat[order(dat$`__group`, dat[, time.name]), ]

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
  which4 <- grep('4$', names(pars)) # For a to u transfer at specific times, incorporation, will be applied once only!

  names(pars) <- gsub('[0-9]$', '', names(pars))

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

  # split dat into groups
  s.dat <- split(dat, dat$`__group`)

  if(check.NA && sapply(s.dat, function(x) anyNA(x[, c("__f0", "__r1", "__r2", "__r3", "__f4")]))) {
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
      data.frame(group = sub.dat[!sub.dat$`__add.row`, "__group"], 
                 calcEmis(ct = sub.dat[, time.name],
                          # Calculate a0 and u0 (f4 transfers done in calcEmis())
                          a0 = sub.dat[1, "__f0"]*sub.dat[1, app.name],
                          u0 = (1 - sub.dat[1, "__f0"])*sub.dat[1, app.name],
                          r1 = sub.dat[, "__r1"],
                          r2 = sub.dat[, "__r2"],
                          r3 = sub.dat[, "__r3"],
                          f4 = sub.dat[, "__f4"],
                          drop.rows = sub.dat$`__add.row` & !add.incorp.rows),
                 row.names = NULL, check.names = FALSE)    
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
      if(any(duplicated(sub.dat[!sub.dat$`__add.row`, time.name]))) {
        stop('Look for 998123b in pmod.R. Duplicated ct values.')
      }

      # calculate emission
      ce <- calcEmis(
        ct = sub.dat[, time.name],
        # Calculate a0 and u0 (f4 transfers done in calcEmis())
        a0 = sub.dat[1, "__f0"]*sub.dat[1, app.name],
        u0 = (1 - sub.dat[1, "__f0"])*sub.dat[1, app.name],
        r1 = sub.dat[, "__r1"],
        r2 = sub.dat[, "__r2"],
        r3 = sub.dat[, "__r3"],
        f4 = sub.dat[, "__f4"], drop.rows = sub.dat$`__add.row` & !add.incorp.rows)

      # add group
      e.list[[i]] <- data.frame(orig.order = sub.dat[!(sub.dat$`__add.row` & !add.incorp.rows), "orig.order"], 
                                group = sub.dat[!(sub.dat$`__add.row` & !add.incorp.rows), "__group"], ce, row.names = NULL, check.names = FALSE)
    } 
  }

  # rbind e.list to data.frame
  e <- do.call("rbind", e.list)

  # rename 'group' column
  if(!is.null(group)){
    names(e)[2] <- group
  } else {
    e$group <- NULL
  }

  # Sort to match original order NTS how does this work with add.incorp.rows = TRUE?
  e <- e[order(e$orig.order), -1]

  # Add pass-through column if requested
  if(!is.null(pass.col)) {
    e <- data.frame(setNames(dat[!(dat$`__add.row` & !add.incorp.rows), paste0("pass_me.through_", pass.col)], pass.col), e)
  }

  return(e)

}

