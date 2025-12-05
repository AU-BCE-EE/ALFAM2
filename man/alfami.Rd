\name{alfami}
\alias{alfami}
\title{
Emissions Inventory Calculations of Ammonia Emission from Field-Applied Manure
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A wrapper and higher-level analysis for the \code{\link{alfam2}} function for emissions inventory applications.
This function is under development and not currently exported!
Warning: The API will likely change in the coming months.
}

\usage{
alfami(
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
    The data frame must include at least the elapsed (cumulative) time since manure was applied in hours at which emission should be evaluated, and the quantity of total ammoniaical nitrogen (TAN) applied in t (metric tonnes).
    (Other units could be used but should match parameter units and will affect the units of output.)
    Typically other predictor variables are included.
    See the vignette for more information.
}
  \item{pars}{
    A named numeric vector (or a list of vectors) with model parameters (secondary parameters). Three parameter sets are provided with the package: \code{alfam2pars01}, \code{alfam2pars02} and \code{alfam2pars03}, with \code{alfam2pars03} recommended. See \code{\link{alfam2pars}} for details. Note that the function could be called with \code{pars = alfam2pars03} (omitting the \code{ALFAM2::} bit) but for clarity and safety, and to avoid a package check problem, the package name is included by default. See details for more information.
}
  \item{time.name}{
    name of column in \code{dat} that contains cumulative time since manure was applied (h) for evalution of emission.
}
  \item{app.tan.name}{
    name of column in \code{dat} with total ammoniaical nitrogen (TAN) application (t)
}
  \item{time.incorp}{
    passed to, so as in, \code{\link{alfam2}}: either name of column in \code{dat} that contains time at which incorporation occurred (h), or length-one numeric value giving time that incorporation occurred (h). Omit if there was no incorporation or if incorporation is not a predictor variable. Optional.
}
  \item{eventkey}{
    name of column containing a unique key application event key. Unlike \code{\link{alfam2}}, each row is otherwise considered a separate application event.
}
  \item{aggkey}{
    name of column or columns that should be used for aggregation of TAN application and emission. Character vector.
}
  \item{uncert}{
    use to include uncertainty based on model parameters (\code{"pars"}), input variables (\code{"vars"}), both (\code{c("pars", "vars")}), or none (\code{NULL}).
    Character vector.
}
  \item{pars.uncert}{
    set of parameter sets to use for model uncertainty.
    Unusual to specify; use default.
}
  \item{uncert.settings}{
    data frame with uncertainty settings for uncertainty in input variables (when \code{'vars'} is included in \code{pars.uncert}), with these columns: \code{pvar} (predictor variable name), \code{rel} (relative, absolute, or centered uncertainty), \code{dist.type} (normal or uniform distribution), \code{sd} (distribution standard deviation), \code{min} (uniform distribution minimum), \code{max} (uniform distribution maximum).
}
  \item{nu}{
     number of uncertainty iterations.
}  
  \item{cl}{
    confidence level.
}
  \item{seed}{
    integer seed for uncertainty iterations.
    Use for reproducing results.
    Length-one integer vector.
}
  \item{quiet}{
    set to \code{TRUE} to suppress some messages.
}
  \item{...}{
    additional optional arguments to \code{\link{alfam2}}.
}
}

\details{
This function simplifies emission calculations for inventory purposes and can estimate uncertainty from both model input variables and parameter values and aggregate TAN application and emission results.
Actual emission calculations are made using \code{alfam2()}.
The name of the function comes from 'ALFAM' and emissions inventory.
}

\value{
A list with six elements, each a data frame with predicted emission and calculated TAN application, but with various additional variables and aggregated in different ways.
See vignette.
}

\references{
  The AlFAM2 project website. \url{https://projects.au.dk/alfam/}
%% ~put references to the literature/web site here ~
}
\author{
Sasha D. Hafner
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{alfam2}}
\code{\link{alfam2pars03var}}
\code{\link{alfam2pars}}
}

\examples{
\dontrun{
dat <- data.frame(zone = c('north', 'north', 'south', 'east', 'west'),
		  sector = c(1, 2, 1, 1, 1),
		  man.source = c('cattle', 'pig', 'cattle', 'cattle', 'pig'),
		  man.dm = c(7, 4, 6.5, 6.5, 4),
		  man.ph = c(7.0, 7.2, 6.9, 7.5, 7.4),
		  app.mthd = c(rep('trailing hose', 4), 'broadcast'),
		  wind.sqrt = sqrt(c(2.2, 2.1, 4.0, 3.7, 4.0)),
		  air.temp = c(12, 8, 8, 15, 14),
		  time.h = 168,
		  TAN.app.tot = c(2000, 1500, 80, 4300, 25))

dat


ei <- alfami(dat, time.name = 'time.h', app.tan.name = 'TAN.app.tot', aggkey = 'zone')

ei$emistot
ei$emisagg
}
# See vignette for more examples and explanation. Run:
# vignette("ALFAM2-inventory")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{nonlinear}% __ONLY ONE__ keyword per line
