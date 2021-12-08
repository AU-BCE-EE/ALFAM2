# Creates parameter objects

pars01 <- read.csv('pars_set1.csv', row.names = 1, header = FALSE)
alfam2pars01 <- ALFAM2pars01 <- as.matrix(pars01)[, 1]
save(alfam2pars01, ALFAM2pars01, file = '../data/alfam2pars01.rda')

pars02 <- read.csv('pars_set2.csv', row.names = 1, header = FALSE)
alfam2pars02 <- ALFAM2pars02 <- as.matrix(pars02)[, 1]
save(alfam2pars02, ALFAM2pars02, file = '../data/alfam2pars02.rda')

