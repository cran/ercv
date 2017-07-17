evicv<-function(cv) 
{
  if (sum(cv<=0)!=0)  warning("The function is available for cv>0",call. = TRUE)
  else  (cv^2-1)/(2*cv^2)
}