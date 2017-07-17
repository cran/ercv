cvevi<-function(evi)
{
  if (sum(evi>=1/2)!=0) warning("The function is available for evi<1/2",call. = TRUE)
  else  1/sqrt(1-2*evi)
}