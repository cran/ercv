print.fitpot <- function(x, ...)
{
  if(class(x)!="fitpot") stop("Object should be of class fitpot")
  print(x$coeff)
}