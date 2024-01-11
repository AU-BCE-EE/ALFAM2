# alfam2() function tests based on tinytest package
# Will run during package checking
# To run manually first load tinytest
# library(tinytest)

# Time step has no effect on result ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 48, TAN.app = 100)
pred0 <- alfam2(dat0, app.name = "TAN.app", time.name = "ctime", warn = FALSE)

dat1 <- data.frame(ctime = 0:48, TAN.app = 100)
pred1 <- alfam2(dat1, app.name = "TAN.app", time.name = "ctime", warn = FALSE)

expect_equal(pred0$er[1], pred1$er[49])


# Time step has no effect on result even with incorporation ~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 48, TAN.app = 100)
dat0$incorpdeep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)

dat1 <- data.frame(ctime = 0:48, TAN.app = 100)
dat1$incorpdeep <- TRUE
dat1$t.incorp <- 4
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)

expect_equal(pred0$er[1], pred1$er[49])

# add.incorp.rows adds a row ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# when incorp is before first interval
dat0 <- data.frame(ctime = 48, TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 2)
expect_equal(pred0$ct, c(4, 48))

# add.incorp.rows adds a row ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# when incorp is between intervals
dat0 <- data.frame(ctime = c(2, 48), TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 3)
expect_equal(pred0$ct, c(2, 4, 48))

# add.incorp.rows does not add a row when incorp time matches interval end ~~~~~~~
dat0 <- data.frame(ctime = c(4, 48), TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 2)
expect_equal(pred0$ct, c(4, 48))

# External and internal (prep.incorp = FALSE) incorporation prep gives same result~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
detach('package:ALFAM2')
library(ALFAM2)
dat0 <- data.frame(ctime = 168, TAN.app = 100, incorp.deep = TRUE, t.incorp = 4)
dat0ip <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', value = 'incorp', warn = FALSE)
predex <- alfam2(dat0ip, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', prep.incorp = FALSE, check = FALSE, warn = FALSE)
predin <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)

# Is row dropping correct?
expect_equal(nrow(dat0), 1)
expect_equal(nrow(dat0ip), 2)

# Is output the same?
expect_equal(predex$er, predin$er)

# NAs in input variables should throw an error ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 48, TAN.app = 100, wind.2m = c(1, NA))
expect_error(alfam2(dat0, app.name = "TAN.app", time.name = "ctime", warn = FALSE))

# Predictor and other varibles can be added at end
dat0 <- data.frame(nothing = NA, ctime = 48, TAN.app = 100, wind.2m = 1)
dat1 <- data.frame(nothing = NA)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, ctime = 48, TAN.app = 100, wind.2m = 1)
expect_equal(pred0, pred1)

# Incorporation can be set with a column name or fixed value
dat0 <- data.frame(ctime = 48, TAN.app = 100, wind.2m = 1, incorp.shallow = TRUE, t.incorp = 4)
dat1 <- data.frame(ctime = 48, TAN.app = 100, wind.2m = 1, incorp.shallow = TRUE)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4, warn = FALSE)
expect_equal(pred0$er, pred1$er)

### Try to use data.table for dat
### No real expect_* to use here, so comparing to one with data frame
##dat0 <- data.table::data.table(ctime = 48, TAN.app = 100)
##pred0 <- alfam2(dat0, app.name = "TAN.app", time.name = "ctime", warn = FALSE)
##dat1 <- data.frame(ctime = 48, TAN.app = 100)
##pred1 <- alfam2(dat1, app.name = "TAN.app", time.name = "ctime", warn = FALSE)
##expect_equal(pred0, pred1)

# Get a warning if trying to use reserved names~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 48, TAN.app = 100)
# It's hard to use a reserved name!
dat0$`__r1` <- 0
expect_warning(alfam2(dat0, app.name = "TAN.app", time.name = "ctime"))

# Make sure add.pars changes output whether overriding/replacing or adding/extending~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 48, TAN.app = 100, man.dm = 5, air.temp = 10, wind.2m = 5, soil.type.clay = 1)
pred0 <- alfam2(dat0, app.name = "TAN.app", time.name = "ctime", warn = FALSE)
# Override
pred1 <- alfam2(dat0, add.pars = c(wind.2m.r1 = 1), app.name = "TAN.app", time.name = "ctime", warn = FALSE)
# Extend
pred2 <- alfam2(dat0, add.pars = c(soil.type.clay.f0 = 1), app.name = "TAN.app", time.name = "ctime", warn = FALSE)
expect_false(identical(pred0, pred1))
expect_false(identical(pred0, pred2))

# Tests are needed for groups and pass_cols
# Also perhaps for additional warnings or errors