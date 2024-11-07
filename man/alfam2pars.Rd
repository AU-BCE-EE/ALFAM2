\name{alfam2pars}
\alias{alfam2pars01}
\alias{alfam2pars02}
\alias{alfam2pars03}
\alias{alfam2pars03var}
\title{
Default Parameter Sets for the ALFAM2 Model
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
These default parameter sets can be used to generate average predictions for the \code{\link{alfam2}} function.
They are described in the papers cited below.
}

\usage{
alfam2pars01
alfam2pars02
alfam2pars03
}

\details{
  In general, the latest default parameter set (highest version number) should be used.
  Set 1 was presented in Hafner et al. (2019), 2 in Hafner et al. (2021, 2024a), and 3 in Hafner et al. (2024b).
  See vignette for more details.
}

\references{
  Hafner, S.D., Pacholski, A., Bittman, S., Carozzi, M., Chantigny, M., Genermont, S., Haeni, C., Hansen, M., Huijsmans, J., Kupper, T., Misselbrook, T., Neftel, A., Nyord, T., Sommer, S. 2019. A flexible semi-empirical model for estimating ammonia volatilization from field-applied slurry. \emph{Atmospheric Environment} \bold{199} 474-484. \doi{10.1016/j.atmosenv.2018.11.034}

  Hafner, S.D., Nyord, T., Sommer, S.G., Adamsen, A.P.S. 2021. Estimation of Danish emission factors for ammonia from field-applied liquid manure for 1980 to 2019. Danish Centre for Food and Agriculture, Aarhus University, Aarhus, Denmark. Report no. 2021-0251862. \url{https://pure.au.dk/portal/files/223538048/EFreport23092021.pdf}

  Hafner, S.D., Kamp, J.N., Pedersen, J., 2024a. Experimental and model-based comparison of wind tunnel and inverse dispersion model measurement of ammonia emission from field-applied animal slurry. Agricultural and Forest Meteorology 344, 109790. \doi{10.1016/j.agrformet.2023.109790}

  Hafner S, Pedersen J, Fuss R, Kamp J, Dalby F, Amon B, Pacholski A, Adamsen A, Sommer S., 2024b. Improved tools for estimation of ammonia emission from field-applied animal slurry: refinement of the ALFAM2 model and database. Atmospheric Environment. \doi{10.1016/j.atmosenv.2024.120910}

  The AlFAM2 project website. \url{https://projects.au.dk/alfam/}
%% ~put references to the literature/web site here ~
}
\author{
Sasha D. Hafner, Christoph Haeni, Roland Fuss
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# To view parameter values
pars03

# One possible way to facilitate comparison of different sets
nms <- unique(c(names(alfam2pars01), names(alfam2pars02), names(alfam2pars03)))
pars <- matrix(rep(NA, length(nms) * 3), 
	       ncol = 3,
	       dimnames = list(nms, c('alfam2pars01', 'alfam2pars02', 'alfam2pars03')))
pars[names(alfam2pars01), 1] <- alfam2pars01
pars[names(alfam2pars02), 2] <- alfam2pars02
pars[names(alfam2pars03), 3] <- alfam2pars03
pars

# See vignette for more details on parameters and predictor variables
# vignette("ALFAM2-start")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")
