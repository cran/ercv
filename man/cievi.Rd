\encoding{utf8} 
\name{cievi}
\alias{cievi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Confidence interval for extreme value index
}
\description{
  Confidence interval for extreme value index estimation by \code{Tm} method.
}
\usage{
cievi(nextremes, evi=0, conf.level=0.90, m=10, nsim=100) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nextremes}{
the number of upper extremes to be used.
}
\item{evi}{
extreme value index. In particular, the shape parammeter
 of a generalized Pareto distribution.
}
\item{conf.level}{
confidence level of the interval.
}
\item{m}{
number of thresholds to do multiplicial test.
}
\item{nsim}{
number of simulation.
}
}

\value{
A numerical vector with two elements, containing the limits of the interval.
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
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
cievi(70, evi=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }
