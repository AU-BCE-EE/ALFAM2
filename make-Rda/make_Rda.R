# Creates parameter objects

pars01 <- read.csv('pars_set1.csv', row.names = 1, header = FALSE)
alfam2pars01 <- ALFAM2pars01 <- as.matrix(pars01)[, 1]
save(alfam2pars01, ALFAM2pars01, file = '../data/alfam2pars01.rda')

pars02 <- read.csv('pars_set2.csv', row.names = 1, header = FALSE)
alfam2pars02 <- ALFAM2pars02 <- as.matrix(pars02)[, 1]
save(alfam2pars02, ALFAM2pars02, file = '../data/alfam2pars02.rda')

pars03_alpha <- read.csv('pars_set3.csv', row.names = 1, header = FALSE)
alfam2pars03_alpha <- ALFAM2pars03_alpha <- as.matrix(pars03_alpha)[, 1]
save(alfam2pars03_alpha, ALFAM2pars03_alpha, file = '../data/alfam2pars03_alpha.rda')

alfam2pars03var_alpha <- read.csv('pars_set3_var.csv', row.names = 1, header = TRUE)
alfam2pars03var_alpha <- ALFAM2pars03var_alpha <- as.matrix(alfam2pars03var_alpha)
save(alfam2pars03var_alpha, ALFAM2pars03var_alpha, file = '../data/alfam2pars03var_alpha.rda')
