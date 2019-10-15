qpot <- function (p, pars, lower.tail = TRUE, log.p = FALSE) 
{
  if (lower.tail == TRUE) 
  {
    if (log.p==FALSE) p <- 1 - p
    else p <- log(1-exp(p))
  }
  
  qGPD <- function(q, xi, psi, log.p=FALSE) #upper.tail
  {
    res <- vector()
    if (xi==0) res<-qexp(q,1/psi,lower.tail=FALSE,log.p=log.p)
    else 
    {
      if (log.p==FALSE) res<-(psi/xi)*(q^(-xi) - 1)
      else res<-(psi/xi)*(exp(-q*xi)-1)
    }
    for (i in 1:length(q))
    {
      if ((log.p==FALSE & (q[i]-0.5)^2>0.25) | (log.p==TRUE & q[i]>0)) res[i] <- NA
    }
    return(res)
  }
  
  evi <- as.numeric(pars[1])
  psi <- as.numeric(pars[2])
  threshold <- as.numeric(pars[3])
  prob <- as.numeric(pars[4])
  if (any(p > prob)) warning("q must be larger than threshold")
  if (log.p==FALSE) q<-p/prob 
  else q<-p-log(prob)
  return(threshold + qGPD(q = q, xi = evi, psi = psi, log.p = log.p))
}
