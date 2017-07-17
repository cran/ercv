\encoding{utf8} 
\name{tdata}
\alias{tdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transforms a heavy-tailed sampled to non-heavy tailed
}
\description{
Transformation of a sample with assumption of heavy-tail to a sample with non-heavy tail.
}
\usage{
tdata(data, threshold = NA, nextremes = NA, sigma=NA)
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
  \item{sigma}{
the scale parammeter divided by shape parameter in generalized Pareto distribution.
}
}

\value{
The transformed data as a numerical vector. 
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
\code{\link{ccdfplot}}, \code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{evicv}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
data("danish", package = "evir")
tdata(danish)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }