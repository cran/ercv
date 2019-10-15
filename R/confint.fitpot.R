confint.fitpot <- function(object, parm, level=0.95, ...)
{
  if(class(object)!="fitpot") stop("Object should be of class fitpot")
  cf <- object$coeff
  ses <- c(attr(object, "evi.sd"), attr(object, "psi.sd"))
  if (length(ses)==2)
  {
    pnames <- c("evi", "psi")
  }else{
    pnames <- c("psi")
  }
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, attr(object, "df.residual"))
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}