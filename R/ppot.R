ppot<-function(q, pars, lower.tail=TRUE, log.p = FALSE)
{
  pGPD<-function(x, xi, psi, log.p = FALSE) # Upper tail
  {
    res <- ifelse(x<0 | (xi<0 & x>-xi/psi), 0, ifelse(x>=0 & xi==0, pexp(x,1/psi,lower.tail=FALSE,log.p=log.p), 
                                                      ifelse(x>=0 & log.p==FALSE, (1+xi*x/psi)^(-1/xi), (-1/xi)*log(1+xi*x/psi))))
    return(res)
  }
  evi<-as.numeric(pars[1])
  psi<-as.numeric(pars[2])
  threshold<-as.numeric(pars[3])
  if (any(q<=threshold)) warning("q must be larger than threshold")
  prob<-as.numeric(pars[4])
  sol0<-pGPD(x = q - threshold, xi = evi, psi = psi, log.p = log.p)
  if (log.p==FALSE) sol<-prob*sol0 
  else sol<-log(prob)+sol0
  if (lower.tail == TRUE)
  {
    if (log.p==FALSE)
    {
      sol <- 1-sol
    }else{
      sol <- log(1-exp(sol))
    }
  }
  return (sol)
}