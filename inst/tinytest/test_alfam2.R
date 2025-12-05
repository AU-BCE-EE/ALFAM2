# alfam2() function tests based on tinytest package
# Will run during package checking
# To run manually first load tinytest
# library(tinytest)

# Predictions are accurate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With simple parameter set
dat0 <- data.frame(ctime = 168, TAN.app = 100)
pars0 <- c(int.f0 = 0.5, int.r1 = -1, int.r2 = -2, int.r3 = -3)
pred0 <- alfam2(dat0, pars = pars0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
expect_equal(pred0$er, 0.63257, tolerance = 0.001)

# Predictions with current default parameter set are approximately as expected
dat1 <- data.frame(ctime = 168, TAN.app = 100, app.mthd = c('bc', 'bsth', 'ts', 'os', 'cs'))
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, group = 'app.mthd')
# Comparison seems to be relative
expect_equal(pred1$er, c(0.5, 0.3, 0.3, 0.1, 0.02), tolerance = 0.2)

# Increasing wind speed, temperature, dry matter, and pH all increase emission
# With default parameter set
dat2 <- data.frame(ctime = 168, TAN.app = 100, man.dm = c(3, 7))
pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, group = 'man.dm')
expect_true(diff(pred2$er) > 0)

dat2 <- data.frame(ctime = 168, TAN.app = 100, wind.2m = c(3, 7), wind.sqrt = sqrt(c(3, 7)))
pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, group = 'wind.2m')
expect_true(diff(pred2$er) > 0)

dat2 <- data.frame(ctime = 168, TAN.app = 100, air.temp = c(5, 20))
pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, group = 'air.temp')
expect_true(diff(pred2$er) > 0)

dat2 <- data.frame(ctime = 168, TAN.app = 100, man.ph = c(6, 8))
pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, group = 'man.ph')
expect_true(diff(pred2$er) > 0)

# Time step has no effect on result ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 168, TAN.app = 100)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)

dat1 <- data.frame(ctime = 0:168, TAN.app = 100)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)

expect_equal(pred0$er[1], pred1$er[169])

# Time step has no effect on result even with incorporation ~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 168, TAN.app = 100)
dat0$incorpdeep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)

dat1 <- data.frame(ctime = 0:168, TAN.app = 100)
dat1$incorpdeep <- TRUE
dat1$t.incorp <- 4
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)

expect_equal(pred0$er[1], pred1$er[169])

# add.incorp.rows adds a row ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# when incorp is before first interval
dat0 <- data.frame(ctime = 168, TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 2)
expect_equal(pred0$ct, c(4, 168))

# add.incorp.rows adds a row ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# when incorp is between intervals
dat0 <- data.frame(ctime = c(2, 168), TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 3)
expect_equal(pred0$ct, c(2, 4, 168))

# add.incorp.rows does not add a row when incorp time matches interval end ~~~~~~~
dat0 <- data.frame(ctime = c(4, 168), TAN.app = 100)
dat0$incorp.deep <- TRUE
dat0$t.incorp <- 4
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE, warn = FALSE)

expect_equal(length(pred0$er), 2)
expect_equal(pred0$ct, c(4, 168))

# External and internal (prep.incorp = FALSE) incorporation prep gives same result~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# Note that the input variables must be in the default parameter set, of course
dat0 <- data.frame(ctime = 168, TAN.app = 100, wind.2m = c(1, NA), air.temp = c(NA, 10))
expect_error(alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE))

# Predictor and other varibles can be added at end
dat0 <- data.frame(nothing = NA, ctime = 168, TAN.app = 100, wind.2m = 1)
dat1 <- data.frame(nothing = NA)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE, ctime = 168, TAN.app = 100, wind.2m = 1)
expect_equal(pred0, pred1)

# Incorporation can be set with a column name or fixed value
dat0 <- data.frame(ctime = 168, TAN.app = 100, wind.2m = 1, incorp.shallow = TRUE, t.incorp = 4)
dat1 <- data.frame(ctime = 168, TAN.app = 100, wind.2m = 1, incorp.shallow = TRUE)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', warn = FALSE)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4, warn = FALSE)
expect_equal(pred0$er, pred1$er)

### Try to use data.table for dat
### No real expect_* to use here, so comparing to one with data frame
##dat0 <- data.table::data.table(ctime = 168, TAN.app = 100)
##pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
##dat1 <- data.frame(ctime = 168, TAN.app = 100)
##pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
##expect_equal(pred0, pred1)

# Get a warning if trying to use reserved names~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 168, TAN.app = 100)
# It's hard to use a reserved name!
dat0$`__r1` <- 0
expect_warning(alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime'))

# Make sure add.pars changes output whether overriding/replacing or adding/extending~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 168, TAN.app = 100, man.dm = 5, air.temp = 10, wind.2m = 5, soil.type.clay = 1)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
# Override
pred1 <- alfam2(dat0, add.pars = c(wind.2m.r1 = 1), app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
# Extend
pred2 <- alfam2(dat0, add.pars = c(soil.type.clay.f0 = 1), app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
expect_false(identical(pred0, pred1))
expect_false(identical(pred0, pred2))

# Test that error is thrown when duplicate names exist in pars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 0:12*4, TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)
pars0 <- c(ALFAM2::alfam2pars02, air.temp.r1 = 0.07354268)
expect_error(alfam2(dat = dat0, pars = pars0, app.name = 'TAN.app', time.name = 'ctime'))

# Different par structures give same results~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat0 <- data.frame(ctime = 168, TAN.app = 100, man.dm = 5, air.temp = 10)
p1 <-    c(int.f0 = 0, man.dm.f0 = 0.1, int.r1 = -1, air.temp.r1 = 0.1)
p2 <- list(int.f0 = 0, man.dm.f0 = 0.1, int.r1 = -1, air.temp.r1 = 0.1)
p3 <-    c(f0.int = 0, f0.man.dm = 0.1, r1.int = -1, r1.air.temp = 0.1)
pred1 <- alfam2(dat0, pars = p1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
pred2 <- alfam2(dat0, pars = p2, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
pred3 <- alfam2(dat0, pars = p3, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
expect_equal(pred1, pred2)
expect_equal(pred1, pred3)

# Missing app.name returns relative emission~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# But with TAN application it does not
dat0 <- data.frame(ctime = c(0, 10, 168))
dat1 <- data.frame(ctime = c(0, 10, 168), TAN.app = 100)
pred0 <- alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', warn = FALSE)
expect_equal(pred0$e, pred0$er, tolerance = 0.00001)
expect_equal(pred1$e / 100, pred1$er, tolerance = 0.00001)

# Check that combination of no TAN.app column and conf.int no longer throws error (see #101)~~~~~~~~~~~~~~
# There is no `expect_...` here, just has a bare call, which will show up if it throws an error even though the test shows OK
dat0 <- data.frame(ctime = c(0, 10, 168))
alfam2(dat0, app.name = 'TAN.app', time.name = 'ctime', conf.int = 0.8, warn = FALSE)

# Tests are needed for groups and pass_cols
# Also perhaps for additional warnings or errors




