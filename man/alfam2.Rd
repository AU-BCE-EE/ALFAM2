\name{alfam2}
\alias{alfam2}
\alias{alfam2pars01}
\alias{alfam2pars02}
\alias{alfam2pars03_alpha}
\alias{alfam2pars03var_alpha}
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
  pars = ALFAM2::alfam2pars02, 
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
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    data frame containing predictor variable values.
    The data frame must include at least the elapsed (cumulative) time since manure was applied in hours, and the application rate of total ammoniaical nitrogen (TAN) in kg/h.
    (Other units could be used but should match parameter units and will affect the units of output.)
    Typically other predictor variables are included.
    See the Details section below and the vignette for more information.
}
  \item{pars}{
    A numeric vector (or a list of vectors) with model parameters (secondary parameters). Two parameter sets are provided with the package: \code{pars01} and \code{pars02}, with \code{pars02} recommended. The latest set is described in Hafner et al. (2021, 2024). The earlier set is described in Hafner et al. (2019). Note that the function could be called with \code{pars = alfam2pars02} (omitting the \code{ALFAM2::} bit) but for clarity and safety, and to avoid a package check problem, the package name is included by default. See details for more information.
}
  \item{add.pars}{
    additional parameter values that will extend or overwrite the \code{pars}
}
  \item{app.name}{
    name of column in \code{dat} that contains total ammonia nitrogen (TAN) application rate (usually kg/ha)
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
    Set to \code{NULL} or \code{NA} to turn off centering (but only do this if you know what you are doing).
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
    Doing so is useful for reducing console or report clutter, but use with caution, because problems with the input data or call could be missed.
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
  \item{...}{
    additional optional arguments as length-one vectors that set values of fixed predictor variables.
    See examples.
}
}
\details{
  Default parameter sets are provided for convenience. 

  For the latest set, Set 2, \code{alfam2pars02}, some parameters were removed, and others added, and all values changed at least slightly.
  Calculation of this set is described in a report on Danish emission factors (Hafner et al., 2021) and a paper (Hafner et al., 2024).

  \code{c(int.f0            = -0.605683,
           app.mthd.os.f0    = -1.74351,
           app.rate.ni.f0    = -0.0111490,
           man.dm.f0         = 0.399670,
           man.source.pig.f0 = -0.592028,
           app.mthd.cs.f0    = -7.63373,
           int.r1            = -0.939215,
           app.mthd.bc.r1    = 0.793524,
           man.dm.r1         = -0.139881,
           air.temp.r1       = 0.0735427,
           wind.2m.r1        = 0.150267,
           app.mthd.ts.r1    = -0.459071,
           ts.cereal.hght.r1 = -0.244712,
           man.ph.r1         = 0.665,
           int.r2            = -1.79919,
           rain.rate.r2      = 0.394022,
           int.r3            = -3.22841,
           app.mthd.bc.r3    = 0.561540,
           app.mthd.cs.r3    = -0.666474,
           man.ph.r3         = 0.238,
           incorp.shallow.f4 = -0.964967,
           incorp.shallow.r3 = -0.580527,
           incorp.deep.f4    = -3.69495,
           incorp.deep.r3    = -1.26570)}

  Names and units (matching units is essential) for numerical predictors are:
  \describe{
    \item{\code{man.dm}}{slurry dry matter, percentage of fresh matter}
    \item{\code{man.ph}}{slurry pH, pH units}
    \item{\code{air.temp}}{air temperature, degress C}
    \item{\code{wind.2m}}{wind speed measured (or adjusted to) 2 m height, m/s}
    \item{\code{rain.rate}}{rainfall rate, mm/h}
  }
  See the vignette for more details.

  Categorical predictor variables are all entered as binary dummy variables.
  The \code{alfam2} function automatically creates the dummy variables (this can be turned off with \code{prep.dum = FALSE}).

  \describe{
    \item{\code{app.mthd}}{application method, \code{bc} for broadcast, \code{ts} for trailing hose, \code{os} for open slot injection, and \code{cs} for closed slot injection}
  }

  Parameter set 1 (\code{alfam2pars01}) (Hafner et al., 2019) is retained in the package for reproducibility.
  It contains the following values:
  \code{c(int.f0            = -0.7364889,
           int.r1            = -1.1785848,
           int.r2            = -0.9543731,
           int.r3            = -2.9012937,
           app.mthd.os.f0    = -1.1717859,
           app.rate.f0       = -0.0134681,
           man.dm.f0         =  0.407466,
           incorp.deep.f4    = -3.6477259,
           incorp.shallow.f4 = -0.4121023,
           app.mthd.bc.r1    =  0.6283396,
           man.dm.r1         = -0.075822,
           air.temp.r1       =  0.0492777,
           wind.2m.r1        =  0.0486651,
           man.ph.r1         =  0.5327231,
           air.temp.r3       =  0.0152419,
           incorp.deep.r3    = -0.3838862,
           app.mthd.os.r3    = -0.122883,
           man.ph.r3         =  0.2663616,
           rain.rate.r2      =  0.4327281,
           rain.cum.r3       = -0.0300936)}
}

\value{
  A data frame with the same number of rows as \code{dat} (unless \code{add.incorp.rows = TRUE}).
  Default columns are:
  \describe{
    \item{\code{ct}}{cumulative time since manure was applied}
    \item{\code{dt}}{interval duration (time step)}
    \item{\code{f0}}{f0 parameter}
    \item{\code{r1}}{r1 parameter}
    \item{\code{r2}}{r2 parameter}
    \item{\code{r3}}{r3 parameter}
    \item{\code{r4}}{r4 parameter}
    \item{\code{r5}}{r4 parameter}
    \item{\code{f}}{fast pool size at \code{ct} (kg/ha)}
    \item{\code{s}}{slow pool size at \code{ct} (kg/ha)}
    \item{\code{j}}{average NH3 flux in interval (kg/ha-h)}
    \item{\code{ei}}{interval emission (kg/ha)}
    \item{\code{e}}{cumulative emission (from time = 0 to \code{ct}) (kg/ha)}
    \item{\code{er}}{relative cumulative emission (fraction of applied TAN)}
  }

  If \code{prep.dum} is used, additional dummy variable columns will also be returned.
}
\references{
  Hafner, S.D., Pacholski, A., Bittman, S., Carozzi, M., Chantigny, M., Genermont, S., Haeni, C., Hansen, M., Huijsmans, J., Kupper, T., Misselbrook, T., Neftel, A., Nyord, T., Sommer, S. 2019. A flexible semi-empirical model for estimating ammonia volatilization from field-applied slurry. \emph{Atmospheric Environment} \bold{199} 474-484. \doi{10.1016/j.atmosenv.2018.11.034}

  Hafner, S.D., Nyord, T., Sommer, S.G., Adamsen, A.P.S. 2021. Estimation of Danish emission factors for ammonia from field-applied liquid manure for 1980 to 2019. Danish Centre for Food and Agriculture, Aarhus University, Aarhus, Denmark. Report no. 2021-0251862. \url{https://pure.au.dk/portal/files/223538048/EFreport23092021.pdf}

  Hafner, S.D., Kamp, J.N., Pedersen, J., 2024. Experimental and model-based comparison of wind tunnel and inverse dispersion model measurement of ammonia emission from field-applied animal slurry. Agricultural and Forest Meteorology 344, 109790. \doi{10.1016/j.agrformet.2023.109790}

  The AlFAM2 project website. \url{https://projects.au.dk/alfam/}
%% ~put references to the literature/web site here ~
}
\note{
}
\author{
Sasha D. Hafner, Christoph Haeni, Roland Fuss
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example 1
# Create predictor variable data
dat1 <- data.frame(ctime = 0:12*4, TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)

# Run model, using default parameter values
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime')
pred1
plot(e ~ ctime, data = pred1, type = 'o')

# For fixed variables (constant over time), they can be given as optional argument.
dat1b <- data.frame(ctime = 0:12*4)

# Run model, using default parameter values
pred1b <- alfam2(dat1b, app.name = 'TAN.app', time.name = 'ctime', 
                 TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)

all.equal(pred1, pred1b)


# Example 2
# Add incorporation (can occur at any time)
dat2 <- dat1
dat2$incorp.deep <- TRUE
dat2$t.incorp <- 3.5

pred2 <- alfam2(dat2, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')
# Note change in r3
pred2
lines(e ~ ctime, data = pred2, type = 'o', col = 'red')

# Example 3
# Time step doesn't matter
dat3 <- data.frame(ctime = c(0, 48), TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)
pred3 <- alfam2(dat3, app.name = 'TAN.app', time.name = 'ctime')
lines(e ~ ctime, data = pred3, type = 'o', col = 'blue')

# Example 4
# Incorporation does not need to occur at end of interval
dat4 <- dat3
dat4$incorp.deep <- TRUE
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
dat5 <- data.frame(field.plot = 1:5, ctime = 48, TAN.app = 100, man.dm = 5, air.temp = 15, 
                   app.mthd.bc = TRUE, t.incorp = 4)
pred5 <- alfam2(dat5, app.name = 'TAN.app', time.name = 'ctime', group = 'field.plot', 
                   time.incorp = 't.incorp')
pred5

# See vignette for more examples. Run:
# vignette("ALFAM2-start")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{nonlinear}% __ONLY ONE__ keyword per line
