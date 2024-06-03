# Creates parameter objects

pars01 <- read.csv('pars_set1.csv', row.names = 1, header = FALSE)
alfam2pars01 <- as.matrix(pars01)[, 1]
save(alfam2pars01, file = '../data/alfam2pars01.rda')

pars02 <- read.csv('pars_set2.csv', row.names = 1, header = FALSE)
alfam2pars02 <- as.matrix(pars02)[, 1]
save(alfam2pars02, file = '../data/alfam2pars02.rda')

pars03 <- read.csv('pars_set3.csv', row.names = 1, header = FALSE)
alfam2pars03 <- as.matrix(pars03)[, 1]
save(alfam2pars03, file = '../data/alfam2pars03.rda')

alfam2pars03var <- read.csv('pars_set3_var.csv', header = TRUE)
alfam2pars03var <- as.matrix(alfam2pars03var)
save(alfam2pars03var, file = '../data/alfam2pars03var.rda')
