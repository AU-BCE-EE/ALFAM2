\name{A2IntDat}
\alias{A2IntDat}
\alias{A2IntVars}
\alias{A2PlotDat}
\alias{A2PlotVars}
\docType{data}
\title{
  Ammonia Emission from Field-Applied Manure
}
\description{
  Ammonia emission and supporting data from the ALFAM2 database on ammonia emission from field-applied manure (described in Hafner et al. (2018)), including interval-level data (\code{A2IntDat}), plot-level data (\code{A2PlotDat}), and descriptions of variables (\code{A2IntVars} and \code{A2PlotVars}).
}

\usage{data("A2IntDat")}
\usage{data("A2PlotDat")}
\usage{data("A2IntVars")}
\usage{data("A2PlotVars")}

\format{
  Data frames with 30907 observations (measurement intervals) and 107 variables (\code{A2IntDat}) or 1899 observations (field plots) and 151 variables (\code{A2PlotDat}).
  A description of the contents can be found in \code{A2IntVars} or \code{A2PlotVars}.
  These two variable description data frames both contain 5 columns. 
  Each column contains a character vector.
  \describe{
    \item{\code{name}}{variable name}
    \item{\code{description}}{variable description}
    \item{\code{type}}{type of data}
    \item{\code{units}}{measurement units}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Interval-level data contain multiple observations per field plot, with an average ammonia emission rate for each interval.
Plot-level data were calculated from interval-level data.
For details, see Hafner et al. (2018).
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
https://www.alfam.dk
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Hafner, S.D., Pacholski, A., Bittman, S., Burchill, W., Bussink, W., Chantigny, M., Carozzi, M., Génermont, S., Haeni, C., Hansen, M.N., Huijsmans, J., Hunt, D., Kupper, T., Lanigan, G., Loubet, B., Misselbrook, T., Meisinger, J.J., Neftel, A., Nyord, T., Pedersen, S.V., Sintermann, J., Thompson, R.B., Vermeulen, B., Vestergaard, A.V., Voylokov, P., Williams, J.R., Sommer, S.G., 2018. The ALFAM2 database on ammonia emission from field-applied manure: Description and illustrative analysis. _Agricultural and Forest Meteorology_ https://doi.org/10.1016/j.agrformet.2017.11.027
}
\examples{
data(A2PlotDat)

# Field plot count by country and application method
table(A2PlotDat$country, A2PlotDat$app.method)

# Compare 48 h relative emission among application methods
plot(e.rel.48 ~ app.method, data = A2PlotDat)

# Or plot all interval data (not run because it is slow and requires ggplot2 package)
\dontrun{
data(A2IntDat)
library(ggplot2)

ggplot(A2IntDat) + geom_line(aes(ct, e.rel, colour = app.method, group = pmid)) +
  facet_wrap(~ country)

}

}
\keyword{datasets}
