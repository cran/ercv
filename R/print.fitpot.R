print.fitpot <- function(x, ...)
{
  if(!inherits(x, "fitpot")) stop("Object should be of class fitpot")
  print(x$coeff)
}