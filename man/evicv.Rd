\encoding{utf8} 
\name{evicv}
\alias{evicv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extreme value index
}
\description{
  The extreme value index for a given coefficient of variation in the generalized Pareto distribution.
}
\usage{
evicv(cv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cv}{
coefficient of variation. It has to satisfy \code{cv} > 0. 
}
}

\value{
The extreme value index for a given coefficient of variation in the generalized Pareto distribution as a numerical value.
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
\code{\link{ccdfplot}}, \code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
evicv(2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }
