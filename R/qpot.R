qpot<-function(p,pars,lower.tail=TRUE)
{
  if (lower.tail==TRUE) p <- 1-p
  qGPD<-function(p,xi,psi){if(xi==0) y<--psi*log(1-p) else y<-psi*((1-p)^(-xi)-1)/xi;y}
  evi<-as.numeric(pars[1])
  psi<-as.numeric(pars[2])
  threshold<-as.numeric(pars[3])
  prob<-as.numeric(pars[4])
  if (p>prob) stop("q must be larger than threshold")
  threshold+qGPD(p=1-p/prob,xi=evi,psi=psi)
}