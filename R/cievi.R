cievi<-function(nextremes,evi=0,conf.level=0.90,m=10,nsim=100) 
{
  if (evi>=1/2) stop("The asymptotic confidence interval needs evi<1/2")
  evis<-numeric(nsim)
  for(sim in 1:nsim)
  {
    if (evi==0) dt<-rexp(nextremes) else dt<-(1/evi)*((1-runif(nextremes))^(-evi)-1)
    evis[sim]<-as.numeric(Tm(data=dt,m=m,nsim=0)["evi"])
  }
  quantile(evis,c((1-conf.level)/2,1-(1-conf.level)/2))
}
