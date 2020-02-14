\name{ALFAM2mod}
\alias{ALFAM2mod}
\title{
Predict Ammonia Emission from Field-Applied Manure
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
An implementation of the ALFAM2 model for predicting ammonia emission from field-applied manure. 
The model is described in Hafner et al. (2018). 
}

\usage{
ALFAM2mod(dat, 
  pars = c(int0           = -0.7364889,
           int1           = -1.1785848,
           int2           = -0.9543731,
           int3           = -2.9012937,
           app.mthd.os0  = -1.1717859,
           app.rate0      = -0.0134681,
           man.dm0        =  0.407466,
           incorp.deep4    = -3.6477259,
           incorp.shallow4 = -0.4121023,
           app.mthd.bc1  =  0.6283396,
           man.dm1        = -0.075822,
           air.temp1      =  0.0492777,
           wind.2m1       =  0.0486651,
           man.ph1        =  0.5327231,
           air.temp3      =  0.0152419,
           incorp.deep3    = -0.3838862,
           app.mthd.os3  = -0.122883,
           man.ph3        =  0.2663616,
           rain.rate2     =  0.4327281,
           rain.cum3      = -0.0300936), 
    app.name = 'TAN.app', time.name = 'ct',
    time.incorp = NULL,group = NULL, center = TRUE,
    cmns = c(app.rate  = 40, 
             man.dm = 6, 
             man.tan = 1.2, 
             man.ph = 7.5, 
             air.temp = 13, 
             wind.1m = 2.7, 
             lwind = 0.43, 
             crop.z = 10),
    check.NA = TRUE, 
    pass.col = NULL, 
    incorp.names = c('incorp', 'deep', 'shallow'),
    add.incorp.rows = FALSE, 
    warn = TRUE,
    parallel = FALSE,
    n.cpus = 1
      )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    data frame containing values for predictor variables
}
  \item{pars}{
    A numeric vector (or a list of vectors) with model parameters (secondary parameters). Default value is described in Hafner et al. (in prep.). See examples.
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
    should numeric predictors be centered?
    Default parameters are based on centered values.
}
  \item{cmns}{
    numeric vector with means for centering.
}
  \item{check.NA}{
    should the function check for \code{NA} values in calculation of primary parameters?
    Usually makes sense to leave as \code{TRUE}.
}
  \item{pass.col}{
    character vector with name(s) of column(s) in \code{dat} that should be passed through to the returned data frame.
}
  \item{incorp.names}{
    character vector with name(s) of column(s) in \code{dat} that contain binary incorporation variables.
}
  \item{add.incorp.rows}{
    function will add additional rows that exactly match the incorporation time(s) (no more than one per level of \code{group}) if they are not already present. Should these be returned or left out (default)?
}
  \item{warn}{
    set to FALSE to suppress some warnings.
}  
  \item{parallel}{
    should parallel processing be used? Helpful for very large \code{dat}.
}
  \item{n.cpus}{
    numeric. Defines the number of cores if \code{parallel} is \code{TRUE}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A data frame with the same number of rows as \code{dat} (unless \code{add.incorp.rows = TRUE}).
  Deafult columns are:
  \describe{
    \item{\code{ct}}{cumulative time since manure was applied}
    \item{\code{dt}}{interval duration (time step)}
    \item{\code{f0}}{f0 parameter}
    \item{\code{r1}}{r1 parameter}
    \item{\code{r2}}{r2 parameter}
    \item{\code{r3}}{r3 parameter}
    \item{\code{f4}}{r4 parameter}
    \item{\code{f}}{fast pool size at \code{ct} (kg/ha)}
    \item{\code{s}}{slow pool size at \code{ct} (kg/ha)}
    \item{\code{j}}{average NH3 flux in interval (kg/ha-h)}
    \item{\code{ei}}{interval emission (kg/ha)}
    \item{\code{e}}{cumulative emission (from time = 0 to \code{ct}) (kg/ha)}
    \item{\code{er}}{relative cumulative emission (fraction of applied TAN)}
  }
}
\references{
  Hafner, S.D., Pacholski, A., Bittman, S., Carozzi, M., Chantigny, M., Genermont, S., Haeni, C., Hansen, M., Huijsmans, J., Kupper, T., Misselbrook, T., Neftel, A., Nyord, T., Sommer, S. 2019. A flexible semi-empirical model for estimating ammonia volatilization from field-applied slurry. \emph{Atmospheric Environment} \bold{199} 474-484.

  The AlFAM2 project website. \url{https://projects.au.dk/alfam/}
%% ~put references to the literature/web site here ~
}
\author{
Sasha D. Hafner
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
pred1 <- ALFAM2mod(dat1, app.name = 'TAN.app', time.name = 'ctime')
pred1
plot(e ~ ct, data = pred1, type = 'o')

# Example 2
# Add incorporation (can occur at any time)
dat2 <- dat1
dat2$incorp.deep <- TRUE
dat2$t.incorp <- 4

pred2 <- ALFAM2mod(dat2, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')
# See change in r3
pred2
lines(e ~ ct, data = pred2, type = 'o', col = 'red')

# Example 3
# Time step doesn't matter
dat3 <- data.frame(ctime = c(0, 48), TAN.app = 100, man.dm = 8, air.temp = 15, app.mthd.bc = TRUE)
pred3 <- ALFAM2mod(dat3, app.name = 'TAN.app', time.name = 'ctime')
lines(e ~ ct, data = pred3, type = 'o', col = 'blue')

# Example 4
# Incorporation does not need to occur at end of interval
dat4 <- dat3
dat4$incorp.deep <- TRUE
dat4$t.incorp <- 4
pred4 <- ALFAM2mod(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 't.incorp')
lines(e ~ ct, data = pred4, type = 'o', col = 'orange')

# Incorporation time can be numeric also (not very practical for groups)
ALFAM2mod(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4)

# To see incorporation time in output, use add.incorp.rows
ALFAM2mod(dat4, app.name = 'TAN.app', time.name = 'ctime', time.incorp = 4, add.incorp.rows = TRUE)

# Example 5
# Function accepts multiple groups
dat5 <- data.frame(field.plot = 1:5, ctime = 48, TAN.app = 100, man.dm = 5, air.temp = 15, app.mthd.bc = TRUE, t.incorp = 4)
pred5 <- ALFAM2mod(dat5, app.name = 'TAN.app', time.name = 'ctime', group = 'field.plot', time.incorp = 't.incorp')
pred5


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{nonlinear}% __ONLY ONE__ keyword per line
