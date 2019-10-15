print.summary.fitpot <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  
  cat("\nCoefficients:\n")
  print(x$coefficients)
}