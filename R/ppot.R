ppot<-function(q,pars,lower.tail=TRUE)
{
  pGPD<-function(x,xi,psi){if(xi==0) y<-1-exp(-x/psi) else y<-1-(1+xi*x/psi)^(-1/xi);y}
  evi<-as.numeric(pars[1])
  psi<-as.numeric(pars[2])
  threshold<-as.numeric(pars[3])
  prob<-as.numeric(pars[4])
  if (q<=threshold) stop("q must be larger than threshold")
  ifelse(lower.tail==FALSE, prob*(1-pGPD(x=q-threshold,xi=evi,psi=psi)), 1-prob*(1-pGPD(x=q-threshold,xi=evi,psi=psi)))
}