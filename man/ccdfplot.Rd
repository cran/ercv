\encoding{utf8} 
\name{ccdfplot}
\alias{ccdfplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot of complementary empirical distribution function and the complementary distribution function
}
\description{
  Plot of complementary empirical distribution function of a sample and the complementary distribution function from peaks-over-threshold model.
}
\usage{
  ccdfplot(data, pars=NA, log="y", from=NA, main="Complementary cdf", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a numeric vector. 
}
  \item{pars}{
a numeric vector with the set of parameters of
 peaks-over-threshold model.
}
  \item{log}{
a character string which contains \code{x} if the x axis is
to be logarithmic, \code{y} if the y axis is to be logarithmic and \code{xy}
or \code{yx} if both axes are to be logarithmic.
}
  \item{from}{
the origen of x-axis in the plot
}
  \item{main}{
an overall title for the plot.
}
  \item{...}{
usual graphic parameters.
  }
}

\value{
Plot of complementary empirical distribution function and the complementary distribution function.
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
\code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{evicv}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
data(d1)
ccdfplot(d1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }
