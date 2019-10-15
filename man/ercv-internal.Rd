\encoding{utf8} 
\name{ercv-internal}
\alias{ercv-internal}
\alias{frcv}
\alias{Tms.pvalue}
\alias{egpd}
\alias{print.fitpot}
\alias{summary.fitpot}
\alias{print.summary.fitpot}
\alias{confint.fitpot}

\title{Internal ercv functions}
\description{Internal ercv functions}
\usage{
frcv(data, Ps)
Tms.pvalue(n, tms, m, evi, hat=TRUE, omit=16, nsim=100)
egpd(x, evi=NA, heavy=NA)
\method{print}{fitpot}(x, ...)
\method{summary}{fitpot}(object, ...)
\method{print.summary}{fitpot}(x, ...)
\method{confint}{fitpot}(object, parm, level=0.95, ...)
}

\details{These functions are not to be called by the user}
\seealso{
\code{\link{ercv-package}}, \code{\link{ccdfplot}}, \code{\link{cievi}}, 
\code{\link{cvevi}}, \code{\link{cvplot}}, \code{\link{evicv}}, \code{\link{fitpot}},
\code{\link{ppot}}, \code{\link{qpot}}, \code{\link{tdata}}, \code{\link{thrselect}}, 
\code{\link{Tm}}
}
\keyword{internal}
