# Prepare input data for ALFAM2mod()

prepDat <- function(dat, app.mthd.name = 'app.mthd', incorp.name = 'incorp', source.name = 'man.source',
                    app.mthd.levels = list(ts = 'ts', 
                                           bc = c('broadcast', 'bc', 'broadspread', 'bredspredning', 'bredspredt'),
                                           os = c('open slot injection', 'os', 'open-slot injection', 'shallow injection', 'nedfældning i græs'), 
                                           cs = c('closed slot injection', 'cs', 'closed-slot injection', 'deep injection', 'nedfældning på sort jord')),
                    incorp.levels = list(shallow = c('shallow', 'harrow'), deep = c('deep', 'plough', 'plow', 'nedbringning')),
                    source.levels = list(pig = c('pig', 'swine', 'svin', 'svinegylle')),
                    value = 'dummy'
                    ) {
  ncc <- ncol(dat)
  ndum <- 0

  # Application method
  if (app.mthd.name %in% names(dat)) {
    dat[, app.mthd.name] <- tolower(dat[, app.mthd.name])

    # Convert application method values to standards
    for (i in 1:length(app.mthd.levels)) {
      dat[dat[, app.mthd.name] %in% app.mthd.levels[[i]], app.mthd.name] <- names(app.mthd.levels)[i] 
    }

    # Application method dummy variables
    aml <- intersect(unique(dat[, app.mthd.name]), names(app.mthd.levels))
    for (i in aml) {
      dat[, paste(app.mthd.name, i, sep = '.')] <- 1 * (dat[, app.mthd.name] == i)
      ndum <- ndum + 1
    }
   }

  # Incorporation
  if (incorp.name %in% names(dat)) {
    dat[, incorp.name] <- tolower(dat[, incorp.name])

    # Convert incorporation values to standards
    for (i in 1:length(incorp.levels)) {
      dat[dat[, incorp.name] %in% incorp.levels[[i]], incorp.name] <- names(incorp.levels)[i] 
    }

    # Incorporation dummy variables
    il <- intersect(unique(dat[, incorp.name]), names(incorp.levels))
    for (i in il) {
      dat[, paste(incorp.name, i, sep = '.')] <- 1 * (dat[, incorp.name] == i)
      ndum <- ndum + 1
    }
   }

  # Source
  if (source.name %in% names(dat)) {
    dat[, source.name] <- tolower(dat[, source.name])

    # Convert source values to standards
    for (i in 1:length(source.levels)) {
      dat[dat[, source.name] %in% source.levels[[i]], source.name] <- names(source.levels)[i] 
    }

    # Source dummy variables
    sl <- intersect(unique(dat[, source.name]), names(source.levels))
    for (i in sl) {
      dat[, paste(source.name, i, sep = '.')] <- 1 * (dat[, source.name] == i)
      ndum <- ndum + 1
    }
  }

  dum <- dat[, 1:ndum + ncc]

  if (value == 'dummy') return(dum)

  return(dat)
}

#dat <- data.frame(ct = 168, app.mthd = c('open slot injection', 'cs', 'bsth'), t.incorp = 12, incorp = c('none', 'shallow', 'deep'), man.source = 'pig')
#dat
#prepDat(dat)
#prepDat(dat, value = 'dat')
