egpd<-function(x,evi=NA,heavy=NA)
{
  int<-c(-100*max(x),100*max(x))
  if(!is.na(evi)) heavy<-(evi>0)
  if(!is.na(heavy)) int<-int*c(heavy,1-heavy)
  fk<-function(sigma) -mean(log(1-x/sigma))
  fp<-function(sigma) length(x)*(-log(fk(sigma)*sigma)+fk(sigma)-1)
  fk2<-function(sigma) -(evi-mean(log(1-x/sigma)))^2
  if (is.na(evi)) sigma<-optimize(fp,interval=int,maximum=T)$maximum
  else sigma<-optimize(fk2,interval=int,maximum=T)$maximum
  list(evi=-fk(sigma),psi=fk(sigma)*sigma)
}
