# Creates parameter objects

pars01 <- read.csv('pars_set1.csv', row.names = 1, header = FALSE)
ALFAM2pars01 <- as.matrix(pars01)[, 1]
save(ALFAM2pars01, file = '../data/ALFAM2pars01.rda')

pars02 <- read.csv('pars_set2.csv', row.names = 1, header = FALSE)
ALFAM2pars02 <- as.matrix(pars02)[, 1]
save(ALFAM2pars02, file = '../data/ALFAM2pars02.rda')

