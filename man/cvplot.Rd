\encoding{utf8} 
\name{cvplot}
\alias{cvplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Exploratory empirical residual coefficient of variation
}
\description{
  Exploratory empirical residual coefficient of variation for extreme value analysis.
}
\usage{
cvplot(data, threshold = NA, nextremes = NA, omit=4, evi=0, main="CVplot", 
       conf.level=0.90, ...)
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
  \item{omit}{
the minimum required number of upper extremes for computing residual statistics.
}
  \item{evi}{
extreme value index. In particular, the shape parammeter
 of a generalized Pareto distribution.
}
  \item{main}{
  an overall title for the plot.
  }
  \item{conf.level}{
  confidence level of the interval (defaults to 0.90).
  }
  \item{...}{
  Usual graphic parameters.
  }
}

\value{
Plot of the empirical residual CV and confidence intervals.
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
\code{\link{ccdfplot}}, \code{\link{cvevi}}, \code{\link{evicv}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
data("moby", package = "poweRlaw")
cvplot(moby, main="MobyDick")

data(d1)
cvplot(d1, threshold=median(d1), main="D1") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }
