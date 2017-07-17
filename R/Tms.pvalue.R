Tms.pvalue<-function(n,tms,m,evi,hat=TRUE,omit=16,nsim=100)
{
  p<-round(exp(log(omit/n)/m),digits=2)
  Ws<-p^(0:m)
  Ps<-1-Ws
  xi<-evi
  cx<-cvevi(evi)
  gettmb<-function(p,m,Ps,Ws,n,xi,hat)
  {
    if (xi==0) dt<-rexp(n) else dt<-(1/xi)*((1-runif(n))^(-xi)-1)
    dt<-dt-min(dt)
    Cs<-frcv(dt,Ps)
    if (length(dt[dt>=quantile(dt,Ps[m+1])])<2) stop("reduce m or increase omit")
    ifelse (hat==TRUE, cxb<-(1-p)*sum(Ws*Cs)/(1-p^(m+1)), cxb<-cx)
    tmb<-n*sum(Ws*(Cs-cxb)^2)
  }
  getnpv <- function(p,m,Ps,Ws,n,xi,hat,nsim)
  {
    tmbvector <- replicate(n = nsim, expr = gettmb(p=p,m=m,Ps=Ps,Ws=Ws,n=n,xi=xi,hat=hat))
    sum(tmbvector > (tms*(m+1)))
  }
  nrej<-getnpv(p,m,Ps,Ws,n,xi,hat,nsim)
  data.frame(nextremes=n,cvopt=cx,evi=xi,tms=tms,pvalue=nrej/nsim,row.names="")
}