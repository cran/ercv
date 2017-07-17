cvplot<-function(data, threshold = NA, nextremes = NA, omit=4, evi=0, main="CVplot", conf.level=0.90, ...)
{
  #####################################################
  #### controls
  #####################################################
  omit<-floor(omit)
  if (omit<2) warning("The parameter omit have to be bigger than 1",call.=TRUE)
  omit<-max(omit,2)
  data <- as.numeric(data)
  data<-data[!is.na(data)]
  data <- sort(data)
  if (is.na(nextremes) && is.na(threshold)) threshold<-min(data)
  if (!is.na(nextremes) && !is.na(threshold)) stop("Enter EITHER a threshold or the number of upper extremes")
  if (!is.na(nextremes)) threshold<-rev(data)[as.numeric(nextremes)]
  if (max(data)<threshold) stop("There are not data over threshold")
  #####################################################
  #### inizialization of vars
  #####################################################
  n0<-length(data)
  data<- data[data > threshold]- threshold
  data <- sort(data)
  n <- length(data)
  k0<-n0-n
  ks<-1:(n-omit)
  ifelse(n-omit>50,ksr<-1:(n-max(omit,20)),ksr<-ks)
  evi<-evi[!is.na(evi)]
  if (sum(evi[evi>=1/4])!=0) warning("The asymptotic confidence interval needs evi<1/4",call. = TRUE)
  evi<-evi[evi<1/4]
  nevi<-length(evi)
  conf.level<-sort(conf.level[conf.level>0.5&conf.level<1])
  nci<-length(conf.level)
  #####################################################
  #### the computation of residual coefficient of variation
  #####################################################
  x<-rev(data)
  t<-data.frame(i=1:n,x=x,cx=c(0,cumsum(x)[1:(n-1)]),cx2=c(0,cumsum(x^2)[1:(n-1)]))
  f<-function(p) (1/(p[1]-2))*(p[4]-(1/(p[1]-1))*p[3]^2)/((1/(p[1]-1))*p[3]-p[2])^2
  rcv<-rev(sqrt(apply(t,1,f))[(omit+1):n])
  #####################################################
  #### the computation of confidence intervals
  #####################################################
  u<-c()
  l<-c()
  if ((nevi*nci)!=0)
  {
    qs<-qnorm(1-(1-expand.grid(evi,conf.level)[,2])/2)
    for (i in 1:(nevi*nci))
    {
      j<-i-trunc((i-1)/nevi)*nevi
      cv<-1/sqrt(1-2*evi[j])
      sigma<-sqrt((1-evi[j])^2*(6*evi[j]^2-evi[j]+1)/((1-2*evi[j])^2*(1-3*evi[j])*(1-4*evi[j]))) #if evi=0, then sigma=1
      u<-cbind(u,cv+qs[i]*sigma/sqrt(n-ks))
      l<-cbind(l,cv-qs[i]*sigma/sqrt(n-ks))
    }
  }
  #####################################################
  #### plot of residual coefficient of variation
  #####################################################
  par(mar = c(5, 6, 5, 3))
  ifelse(nevi!=0,setpoint<-c(rcv[!is.na(rcv)],u[ksr,],l[ksr,]),setpoint<-rcv[!is.na(rcv)])
  plot(ks+k0-1,rcv,type="l",ylim=c(min(setpoint),max(setpoint)),
       xlab="Excluded sample size",ylab=paste("Coefficient of variation"),col="blue",...)
  title(main=main,outer=F,adj=1,line=4,cex.main=1)
  mtext("Threshold",side=3,line=2.5,cex=0.66)
  ti<-c(1+(k0-1),sort(axTicks(1))[c(-1,-(length(axTicks(1))))],min(max(axTicks(1)),n+(k0-1)))
  lti<-data[ti-(k0-1)]+threshold
  axis(3,at=ti,labels=paste(format(signif(lti,2))), tick = T,cex.axis=0.66)
  #####################################################
  #### plot of confidence intervals
  #####################################################
  if (nevi!=0)
  {
    for (i in 1:(nevi*nci))
    {
      cv<-1/sqrt(1-2*evi[i-trunc((i-1)/nevi)*nevi])
      abline(h=cv,col=rainbow(10)[2*(i-trunc((i-1)/nevi)*nevi)],lty=2,lwd=1)
      lines(ks+k0-1,u[,i],lty=1,col=rainbow(10)[2*(i-trunc((i-1)/nevi)*nevi)])
      lines(ks+k0-1,l[,i],lty=1,col=rainbow(10)[2*(i-trunc((i-1)/nevi)*nevi)])
    }
  }
  #####################################################
  #### plot control line
  #####################################################
  abline(h=sqrt(2),col="black",lty=3)
}