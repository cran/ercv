\encoding{utf8} 
\name{cvevi}
\alias{cvevi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Coefficient of variation for a given extreme value index
}
\description{
  The coefficient of variation for a given extreme value index in the generalized Pareto distribution.
}
\usage{
cvevi(evi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{evi}{
extreme value index. In particular, the shape parameter
 of a generalized Pareto distribution. It has to satisfy \code{evi} < 1/2.
}
}

\value{
A numerical value containing the coefficient of variation for the given extreme value index.
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
cvevi(-1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }
