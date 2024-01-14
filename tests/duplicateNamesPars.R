rm(list = ls())

ff <- list.files('../R', full.names = T)

for(i in ff) source(i)

dat <- data.frame(ctime = 0:12*4, TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)
pars1 <- ALFAM2::alfam2pars02
out1 <- alfam2(dat = dat, pars = pars1, app.name = 'TAN.app', time.name = 'ctime')

# Adding an extra air.temp.r1 element to pars (where air.temp.r1 is already defined), 
# generates a different output even though the value was not changed. 
# An error message has not been implemented to prevent this.

pars2 <- c(pars1, air.temp.r1 = 0.07354268)
out2 <- alfam2(dat = dat, pars = pars2, app.name = 'TAN.app', time.name = 'ctime')

