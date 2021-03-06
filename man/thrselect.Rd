\encoding{utf8} 
\name{thrselect}
\alias{thrselect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold selection algorithm
}
\description{
Threshold selection algorithm.
}
\usage{
thrselect(data, threshold=NA, nextremes=NA, omit=16, evi=NA, m=10, nsim=100, 
          conf.level=0.90, oprint=TRUE)
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
  \item{m}{
number of thresholds to do multiplicial test.
  }
  \item{nsim}{
number of simulations.
  }
  \item{conf.level}{
confidence level of the interval.  
  }
  \item{oprint}{
logical.  If \code{TRUE} (default), the single solution is
printed. In any case, the full solution is the output of the function.  
  }
}

\value{
A list including two \code{data.frame} (solution and options). Each of the \code{data.frame} contains the following columns:
\itemize{
 \item{m}{
 number of thresholds for testing tail index.
 }
 \item{nextremes}{
 number of thresholds for testing tail index.
 }
 \item{threshold}{
 the threshold value
 }
 \item{rcv}{
 residual coefficient of variation for selected threshold.
 }
 \item{cvopt}{
 optimal coefficient of variation for the tail.
 }
 \item{evi}{
 the corresponding tail index for optimal coefficient of variation if \code{evi} parameter is \code{NA}.
 }
 \item{tms}{
 the statistic of the tail index test.
 }
 \item{pvalue}{
 p-value associated to \code{tms}.
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
\code{\link{ccdfplot}}, \code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{evicv}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}},
\code{\link{Tm}}
}

\examples{
data("nidd.thresh", package = "evir")
thrselect(nidd.thresh, nsim=500)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }