\encoding{utf8} 
\name{fitpot}
\alias{fitpot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fits peaks-over-threshold model of a sample
}
\description{
Fits peaks-over-threshold model of a sample.
}
\usage{
fitpot(data, threshold=NA, nextremes=NA, evi=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a numeric vector.
}
  \item{threshold}{
a threshold value (either this or \code{nextremes} must
be given but not both).
}
  \item{nextremes}{
the number of upper extremes to be used (either
this or \code{threshold} must be given but not both).
}
  \item{evi}{
extreme value index. In particular, the shape parammeter
 of a generalized Pareto distribution.
 }
}

\value{
A \code{data.frame} with the following columns:
\itemize{
 \item{evi}{
 extreme value index. In particular, the shape parammeter of a generalized Pareto
distribution.
}
 \item{psi}{
 the scale parameter of a generalized Pareto distribution.
 }
 \item{threshold}{
 a threshold value where peaks-over-threshold is 
applied.
 }
 \item{prob}{
 proportion of size of data corresponding to the upper extremes modelled with generalized pareto distribution.
 }
}
}

\references{
del Castillo, J. and Padilla, M. (2016). Modeling extreme values by the residual coefficient of variation. SORT Statist. Oper. Res. Trans. \bold{40}(2), 303-320.

del Castillo, J. and Serra, I. (2015). Likelihood inference for Generalized Pareto Distribution. Computational Statistics and Data Analysis, \bold{83}, 116-128.

del Castillo, J., Daoudi, J. and Lockhart, R. (2014). Methods to Distinguish Between Polynomial and Exponential Tails. Scandinavian Journal of Statistics, \bold{41}, 382-393.
}

\author{
Joan del Castillo, David Moriña Soler and Isabel Serra
}

\seealso{
\code{\link{ercv-package}}, \code{\link{cievi}}, 
\code{\link{ccdfplot}}, \code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{evicv}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
data("nidd.thresh", package = "evir")
fitpot(nidd.thresh)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }