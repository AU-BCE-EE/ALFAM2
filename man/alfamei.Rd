\name{alfam2}
\alias{alfam2}
\title{
Emissions Inventory Calculations of Ammonia Emission from Field-Applied Manure
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A wrapper for the \code{\link{alfam2}} function for emissions inventory applications.
}

\usage{
alfamei <- function(
  dat, 
  pars = ALFAM2::alfam2pars03, 
  time.name,
  app.tan.name = 'app.tan',
  time.incorp = NULL,
  eventkey = NULL,
  aggkey = NULL,
  uncert = NULL,
  pars.uncert = ALFAM2::alfam2pars03var,
  uncert.settings = NULL,
  nu = 100,
  cl = 0.8,
  seed = NULL,
  quiet = FALSE,
  ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    data frame containing predictor variable values.
    The data frame must include at least the elapsed (cumulative) time since manure was applied in hours, and the application rate of total ammoniaical nitrogen (TAN) in kg/h.
    (Other units could be used but should match parameter units and will affect the units of output.)
    Typically other predictor variables are included.
    See the details section below and the vignette for more information.
}
  \item{pars}{
    A named numeric vector (or a list of vectors) with model parameters (secondary parameters). Three parameter sets are provided with the package: \code{alfam2pars01}, \code{alfam2pars02} and \code{alfam2pars03}, with \code{alfam2pars03} recommended. See \code{\link{alfam2pars}} for details. Note that the function could be called with \code{pars = alfam2pars03} (omitting the \code{ALFAM2::} bit) but for clarity and safety, and to avoid a package check problem, the package name is included by default. See details for more information.
}
  \item{time.name}{
	  x.
}
  \item{app.tan.name}{
	  x
}
  \item{time.incorp}{
	  x
}
  \item{eventkey}{
	  x
}
  \item{aggkey}{
	  x
}
  \item{uncert}{
	  x
}
  \item{pars.uncert}{
	  x
}
  \item{uncert.settings}{
	  x
}
  \item{nu}{
	  Number of uncertainty iterations.
}  
  \item{cl}{
	  x
}
  \item{seed}{
}
  \item{quiet}{
}
  \item{...}{
    additional optional arguments to \code{\link{alfam2}}.
}
}

\details{
}

\value{
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
\seealso{
\code{\link{alfam2pars}}
}

\examples{
# Example 1
# Create predictor variable data
dat1 <- data.frame(ctime = 0:12*4, TAN.app = 100, man.dm = 8, air.temp = 15, 
		   app.mthd = 'trailing shoe')

# Run model, using default parameter values
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime')
pred1
plot(e ~ ctime, data = pred1, type = 'o')

# For fixed variables (same for all rows), they can be given as optional argument.
dat1b <- data.frame(ctime = 0:12*4)

# Run model, using default parameter values
pred1b <- alfam2(dat1b, app.name = 'TAN.app', time.name = 'ctime', 
                 TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd = 'trailing shoe')

all.equal(pred1, pred1b)


# Example 2
# Add incorporation (can occur at any time)
dat2 <- dat1
dat2$incorp <- 'deep'
dat2$t.incorp <- 3.5
dat2

pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')
# Note change in r3
pred2
lines(e ~ ctime, data = pred2, type = 'o', col = 'red')

# Example 3
# Time step doesn't matter
dat3 <- data.frame(ctime = c(0, 48), TAN.app = 100, man.dm = 8, air.temp = 15, 
		   app.mthd = 'trailing shoe')
pred3 <- alfam2(dat3, app.name = 'TAN.app', time.name = 'ctime')
lines(e ~ ctime, data = pred3, type = 'o', col = 'blue')

# Example 4
# Incorporation does not need to occur at end of interval
dat4 <- dat3
dat4$incorp <- 'deep'
dat4$t.incorp <- 4
pred4 <- alfam2(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')
lines(e ~ ctime, data = pred4, type = 'o', col = 'orange')

# Incorporation time can be numeric also (not very practical for groups)
alfam2(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4)

# To see incorporation time in output, use add.incorp.rows
alfam2(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4, 
       add.incorp.rows = TRUE)

# Example 5
# Function accepts multiple groups
# Also shown here: some aliases for the different application methods
dat5 <- data.frame(field.plot = 1:5, ctime = 48, TAN.app = 100, man.dm = 5, air.temp = 15, 
                   app.mthd = c('bc', 'th', 'ts', 'os', 'cs'), t.incorp = 4)
pred5 <- alfam2(dat5, app.name = 'TAN.app', time.name = 'ctime', group = 'field.plot', 
                   time.incorp = 't.incorp')
pred5

# See vignette for more examples and explanation. Run:
# vignette("ALFAM2-start")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{nonlinear}% __ONLY ONE__ keyword per line
