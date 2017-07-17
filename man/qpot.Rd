\encoding{utf8} 
\name{qpot}
\alias{qpot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile function
}
\description{
Quantile function from the peaks-over-threshold model.
}
\usage{
qpot(p, pars, lower.tail=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
vector of probabilities.
}
  \item{pars}{
a numeric vector with the set of parameters of
 peaks-over-threshold model. The names of the elements have to be \code{evi}, \code{psi}, \code{threshold}, \code{prob}.
}
 \item{lower.tail}{
 logical; if \code{TRUE} (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
 }
}

\value{
Quantile function as a numerical value.
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
\code{\link{ppot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}

\examples{
qpot(0.1, c(evi=0.1, psi=0.2, threshold=0.3, prob=0.4), lower.tail=FALSE)

x<-runif(10000)
x<-c(x^-1,x)
pars<-fitpot(x,1)
qpot(0.5/10,pars,lower.tail=FALSE) #the true value is 10
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ercv }