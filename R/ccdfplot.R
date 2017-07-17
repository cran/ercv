ccdfplot<-function(data, pars=NA, log="y", from=NA,main="Complementary cdf", ...)
{
  #####################################################
  #### controls and inizialization of vars
  #####################################################
  pf<-function(x,evi,psi){if(evi==0) y<-1-exp(-x/psi) else y<-1-(1+evi*x/psi)^(-1/evi);y}
  pars<-t(matrix(as.numeric(pars),nrow=4))
  data <- as.numeric(data)
  data<-data[!is.na(data)]
  data <- sort(data)
  n<-length(data)
  z<-ecdf(data)
  if (is.na(from)) from<-min(data)
  #####################################################
  #### output
  #####################################################
  plot(data[data>from][-n],1-z(data[data>from])[-n],cex=0.5,type="s",xlab="data",ylab="ccdf",log=log,col="blue",...)
  points(data[-n],1-z(data)[-n],cex=0.5)
  title(main=main,outer=F,adj=1,cex.main=1)
  if(sum(is.na(pars))==0)
  {
    nmod<-length(pars[,1])
    evi<-pars[,1]
    psi<-pars[,2]
    threshold<-pars[,3]
    prob<-pars[,4]
    for (i in 1:nmod)
    {
      mdata<-data[data>threshold[i]]-threshold[i]
      lines(mdata+threshold[i],(1-pf(x=mdata,evi=evi[i],psi=psi[i]))*prob[i],col=rainbow(10)[2*(i-trunc((i-1)/nmod)*nmod)],lwd=2)
      abline(v=threshold[i],lwd=2,col=rainbow(10)[2*(i-trunc((i-1)/nmod)*nmod)],lty=3)
    }
  }
}