
prepIncorp <- function(dat, pars, time.name, time.incorp, incorp.names, warn) {

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
      # Get time.incorp column entries (first row, so rows underneath could have time.incorp = NA etc., all ignored)
      incorp.time <- tapply(dat[, time.incorp], dat$`__group`, "[", 1)
    }

    # Get number of incorp columns by group for 1) checking that there is not more than 1 applied, 2) skipping incorp when there is none (even when t.incorp may have a value)
    n.incorp.vals <- rowSums(dat[, inc.ex, drop = FALSE])
    n.incorp.vals.grp <- tapply(n.incorp.vals, dat$`__group`, "[", 1)

    # If multiple incoporation dummy variables are 1 for any row, throw error
    if (any(rowSums(dat[, inc.ex, drop = FALSE]) > 1)) {
      stop('Multiple incorporation types specified in the same row.\nThis is not compatible with the ALFAM2 package and is likely a mistake.')
    }

    # Check if columns exist
    if(length(inc.ex) == 0){
      if (warn) {
        warning("No matching column for incorporation parameter(s): ", paste(inc.names, collapse = ", "), ". Skipping incorporation.")
      }
      return(list(dat = dat, time.incorp = NULL))
    }

  } else {
    if (warn) {
      warning("No incorporation parameters have been provided. Skipping incorporation.")
    }
    return(list(dat = dat, time.incorp = NULL))
  }

  # Incorporation groups
  incorp.grps <- names(incorp.time)[!is.na(incorp.time) & n.incorp.vals.grp > 0]
  # Drop groups that have incorp.time >= maximum time
  inc.dat <- dat[dat$`__group` %in% incorp.grps, ]
  maxtime <- tapply(inc.dat[, time.name], inc.dat$`__group`, max)
  incorp.grps <- names(maxtime)[maxtime > incorp.time[names(maxtime)]]
  if (warn && length(dn <- names(maxtime)[maxtime <= incorp.time[names(maxtime)]]) > 0) {
    message('Incorporation skipped where it occurred after all intervals.\nGroups: ', paste(dn, collapse = ','), '.')
  }

  # Set incorp to FALSE for groups without incorporation (based on missing etc. incorp time)
  dat[!dat$`__group` %in% incorp.grps, inc.ex] <- FALSE

  dat$`__add.row` <- FALSE

  # Loop through groups with incorporation (incorp.time != NA)
  if (warn) {
    message('Incorporation applied for groups', paste(incorp.grps, collapse = ', '), '.')
  }

  for(i in incorp.grps) {

    # Extract cumulative time
    ct <- dat[dat$`__group` == i, time.name]

    # Find where incorporation occurs
    # Hint to interpret __f4 = 0 placement below: incorporation occurs at the *start* of an interval (see rcpp_calcEmis.cpp)
    # and ct = *end* of an interval, with the start = the end of previous interval
    ct.ind <- which(ct > incorp.time[i])[1]

    # Add rows
    if (incorp.time[i] != ct[ct.ind - 1] || ct.ind == 1){

      # Use predictor values from ct.ind row, insert row before first interval
      ins.dat <- dat[dat$`__group` == i, ][ct.ind, ]
      ins.dat[, time.name] <- incorp.time[i] 
      ins.dat$`__add.row` <- TRUE
      dat[dat$`__group` == i, '__f4'][ct.ind] <- 0
      dat <- rbind(dat, ins.dat)          

    } else { # Exact time match (in *previous* interval)

      dat[dat$`__group` == i, '__f4'][ct.ind] <- 0

    }

    # Set incorp variables to FALSE for time <= incorp.time (incorp then applied at start of next interval)
    # NTS: Is this redundant because __f4 = 1 for these (or should)?
    dat[dat$`__group` == i & dat[, time.name] <= incorp.time[i], inc.ex] <- FALSE

  }

  return(list(dat = dat, time.incorp = time.incorp))

}

