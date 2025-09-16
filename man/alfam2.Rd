\name{alfam2}
\alias{alfam2}
\title{
Predict Ammonia Emission from Field-Applied Manure
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
An implementation of the ALFAM2 model for predicting ammonia emission from field-applied manure. 
The model is described in Hafner et al. (2019). 
}

\usage{
alfam2(
  dat, 
  pars = ALFAM2::alfam2pars03, 
  add.pars = NULL,
  app.name = 'TAN.app', 
  time.name = 'ct', 
  time.incorp = NULL, 
  group = NULL, 
  center = c(app.rate  = 40, 
             man.dm    =  6.0, 
             man.tan   =  1.2, 
             man.ph    =  7.5, 
             air.temp  = 13, 
             wind.2m   =  2.7, 
             wind.sqrt =  sqrt(2.7), 
             crop.z    = 10), 
  pass.col = NULL, 
  incorp.names = c('incorp', 'deep', 'shallow'),
  prep.dum = TRUE,
  prep.incorp = TRUE,
  add.incorp.rows = FALSE, 
  check = TRUE,
  warn = TRUE,
  value = 'emis',
  conf.int = NULL,
  pars.ci = ALFAM2::alfam2pars03var,
  n.ci = NULL,
  var.ci = 'er',
  ...)
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
  \item{add.pars}{
    additional parameter values that will extend or overwrite the \code{pars}.
    Named numeric vector.
}
  \item{app.name}{
    name of column in \code{dat} that contains total ammonia nitrogen (TAN) application rate (usually kg/ha).
    If omitted, function will return relative emission (fraction of applied TAN) and a warning (if \code{warn = TRUE}).
}
  \item{time.name}{
    name of column in \code{dat} that contains cumulative time since manure was applied (h)
}
  \item{time.incorp}{
    either name of column in \code{dat} that contains time at which incorporation occurred (h), or length-one numeric value giving time that incorporation occurred (h). Omit if there was no incorporation or if incorporation is not a predictor variable. Optional.
}
  \item{group}{
    name of column in \code{dat} that contains a grouping variable for identifying individual plots or locations. Optional.
}
  \item{center}{
    numeric vector with means for centering.
    Generally should not be changed by users, because parameter values depend on particular centering values, and nonsense predictions can result from changes to even a single centering value.
    Set to \code{NULL} to turn off centering, but only do this if you know what you are doing.
    Internally, supplied values are used to either replace or extend centering values by variable, so it is possible to add a new centering value with e.g., \code{center = c(x1 = 10)}, which will not affect any of the default values.
    In contrast, \code{center = c(wind.2m = 5)} would change the value for the variable \code{wind.2m}.
    Default parameters are based on centered values.
}
  \item{pass.col}{
    character vector with name(s) of column(s) in \code{dat} that should be passed through to the returned data frame.
}
  \item{incorp.names}{
    character vector with name(s) of column(s) in \code{dat} that contain binary incorporation variables.
}
  \item{prep.dum}{
    if \code{TRUE} (default), function will automatically prepare dummy variables from input data. 
    If \code{FALSE}, any necessary dummy variables must already be present in \code{dat}. 
    See vignette.
    Length one logical vector.
}  
  \item{prep.incorp}{
    if \code{TRUE} (default), function will automatically prepare incorporation inputs from input data. 
    See vignette.
    Length one logical vector.
}
  \item{add.incorp.rows}{
    function will add additional rows that exactly match the incorporation time(s) (no more than one per level of \code{group}) if they are not already present. Should these be returned or left out (default)?
    Length one logical vector.
}
  \item{check}{
    should the function check inputs, including for \code{NA} values in calculation of primary parameters?
    Default of \code{TRUE} is recommended.
}
  \item{warn}{
    set to \code{FALSE} to suppress some warnings and messages.
    Doing so is useful for reducing console or report clutter, but do so with caution, because problems with the input data or call could be missed.
    Even with \code{warn = FALSE} the \code{alfam2} function may make some substitutions in inputs (see vignette).
}  
  \item{value}{
    type of output.
    Set to \code{"incorp"} to return results early with incorporation (or dummy variable) pre-processing and no emission calculations.
    Output can be used to run \code{alfam2} with \code{prep.incorp = FALSE} to speed up evaluation.
    Otherwise must be \code{"emis"} for "emission".
    See vignette.
    Length one character.
}  
  \item{conf.int}{
    confidence interval setting.
    Default (\code{NULL}) does not return a confidence interval.
    Use numeric values for confidence interval, e.g., \code{conf.int = 0.90} for 90\% confidence interval.
    This value will be used with the \code{quantile} function.
    Note that with default \code{pars.ci} like \code{alfam2pars03var} the returned confidence intervals are an estimate of uncertainty in the \emph{average} response for the particular values provided for input variables. 
    These parameters are based on variability in measured emission among research institutions, and were developed using a bootstrap approach.
    Set \code{conf.int} to \code{'all'} to have the function return all predictions instead of quantile estimates of the confidence interval.
    This can be useful for incorporation of uncertainty in input variables.
    See vignette.
  }
  \item{pars.ci}{
    matrix or data frame of parameter sets for confidence interval calculations.
    See \code{alfam2pars03var} for an example.
  }
  \item{n.ci}{
    number of parameter sets to use.
    Defaults to total number available.
  }
  \item{var.ci}{
    calculate confidence intervals for these variables.
    Calculation is done separately by variable and time interval.
  }
  \item{...}{
    additional optional arguments as length-one vectors that set values of fixed predictor variables.
    See examples.
}
}
\details{
  Names and units (matching units is essential) for numerical predictors are:
  \describe{
    \item{\code{app.rate.ni}}{manure application rate, but not for injection (\code{app.mthd = "os"} or \code{app.mthd = "cs"}) in t/ha}
    \item{\code{man.dm}}{slurry dry matter, percentage of fresh matter}
    \item{\code{man.ph}}{slurry pH, pH units}
    \item{\code{air.temp}}{air temperature, degress C}
    \item{\code{wind.2m}}{wind speed measured (or adjusted to) 2 m height, m/s}
    \item{\code{rain.rate}}{rainfall rate, mm/h}
  }
  See the vignette for more details.

  Categorical predictor variables can be entered as binary dummy variables or left as character or factors.
  The \code{alfam2} function automatically creates the dummy variables from from three categorical variables (this can be turned off with \code{prep.dum = FALSE}):

  \describe{
    \item{\code{app.mthd}}{application method, \code{bc} for broadcast, \code{ts} for trailing shoe, \code{os} for open slot injection, and \code{cs} for closed slot injection} (and see examples and vignette for aliases)
    \item{\code{man.source}}{type (source) of manure, \code{pig} for pig, otherwise assumed to cattle or other (reference)}
    \item{\code{incorp}}{incorporation, either \code{shallow} or \code{deep} (change in levels would require change in \code{incorp.names} as well as parameter values)}
  }

  For parameter set values, see the \code{\link{alfam2pars02}}, \code{\link{alfam2pars02}}, or \code{\link{alfam2pars03}} objects.

  Users are responsible for checking that input variable values are not beyond limits of measurement data used for parameter estimation.
  For parameter set 3, recommended limits are: DM 1-15\% DM, pH 5.5-9.0, air temperature 0-30 deg. C, wind speed 0-10 m/s, rainfall rate 0-2.5 mm/h, and duration 0-168 h.

}

\value{
  By default, a data frame with the same number of rows as \code{dat} (unless \code{add.incorp.rows = TRUE}).
  First column is \code{time.name} column from input data \code{dat}.
  All masses are intended to be as kg ammonia N, or TAN (for totoal ammoniacal nitrogen), but reflect the application rate variable used as input.
  Defaults for following columns are:
  \describe{
    \item{\code{dt}}{interval duration (time step)}
    \item{\code{f0}}{f0 parameter}
    \item{\code{r1}}{r1 parameter}
    \item{\code{r2}}{r2 parameter}
    \item{\code{r3}}{r3 parameter}
    \item{\code{r4}}{r4 parameter}
    \item{\code{r5}}{r5 parameter}
    \item{\code{f}}{fast pool size at \code{ct} (kg/ha)}
    \item{\code{s}}{slow pool size at \code{ct} (kg/ha)}
    \item{\code{j}}{average NH3 flux in interval (kg/ha-h)}
    \item{\code{jinst}}{instantaneous NH3 flux at given time (kg/ha-h)}
    \item{\code{ei}}{interval emission (kg/ha)}
    \item{\code{e}}{cumulative emission (from time = 0 to \code{ct}) (kg/ha)}
    \item{\code{er}}{relative cumulative emission (fraction of applied TAN)}
  }

  If \code{prep.dum} is used, additional dummy variable columns will also be returned.
  And if a grouping variable is used via \code{group}, this column will be included as well.
  Any columns listed in \code{pass.cols} will also be returned.

  If \code{value = 'incorp'} is used, the function will return intermediate data processed for incorporation but without emission predictions.
  See vignette.
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
