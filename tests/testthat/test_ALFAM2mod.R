context("ALFAM2mod() accuracy and stability")

test_that("Time step has no effect on result", {
  
  dat0 <- data.frame(ctime = 48, TAN.app = 100)
  pred0 <- ALFAM2mod(dat0, app.name = "TAN.app", time.name = "ctime")

  dat1 <- data.frame(ctime = 0:48, TAN.app = 100)
  pred1 <- ALFAM2mod(dat1, app.name = "TAN.app", time.name = "ctime")

  expect_equal(pred0$er[1], pred1$er[49])
})


test_that("Time step has no effect on result even with incorporation", {
  
  dat0 <- data.frame(ctime = 48, TAN.app = 100)
  dat0$incorpdeep <- TRUE
  dat0$t.incorp <- 4
  pred0 <- ALFAM2mod(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')

  dat1 <- data.frame(ctime = 0:48, TAN.app = 100)
  dat1$incorpdeep <- TRUE
  dat1$t.incorp <- 4
  pred1 <- ALFAM2mod(dat1, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')

  expect_equal(pred0$er[1], pred1$er[49])
})


test_that("add.incorp.rows adds a row", {
  
  dat0 <- data.frame(ctime = 48, TAN.app = 100)
  dat0$incorp.deep <- TRUE
  dat0$t.incorp <- 4
  pred0 <- ALFAM2mod(dat0, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp', add.incorp.rows = TRUE)

  expect_length(pred0$er, 2)
  expect_equal(pred0$ct, c(4, 48))
})

