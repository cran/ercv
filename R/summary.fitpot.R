summary.fitpot <- function (object, ...) 
{
  call <- attr(object, "Call")
  coef.p <- object$coeff
  s.err <- c(attr(object, "evi.sd"), attr(object, "psi.sd"), NA, NA)
  dn <- c("Estimate", "Std. Error")
  coef.table <- cbind(coef.p, s.err)
  dimnames(coef.table) <- list(names(coef.p), dn)
  ans <- list(coefficients = coef.table, call=call)
  
  class(ans) <- "summary.fitpot"
  return(ans)
}