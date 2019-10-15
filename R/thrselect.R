thrselect<-function(data, threshold = NA, nextremes = NA, omit = 16, evi = NA, m = 10, nsim = 100, conf.level = 0.90, oprint=TRUE)
{
  #####################################################
  #### controls and inizialization of vars
  #####################################################
  omit<-floor(omit)
  if (omit<2) warning("The parameter omit has to be bigger than 1",call.=TRUE)
  omit<-max(omit,2)
  data <- as.numeric(data)
  data<-data[!is.na(data)]
  data <- sort(data)
  if (is.na(nextremes) && is.na(threshold)) threshold<-min(data)
  if (!is.na(nextremes) && !is.na(threshold)) stop("Enter EITHER a threshold or the number of upper extremes")
  if (!is.na(nextremes)) threshold<-rev(data)[as.numeric(nextremes)]
  data<- data[data >= threshold]- threshold
  n <- length(data)
  #####################################################
  #### inside
  #####################################################
  p<-round(exp(log(omit/n)/m),digits=2)
  Ws<-p^(0:m)
  Ps<-1-Ws
  if (length(data[data>=quantile(data,Ps[m+1])])<2) stop("Reduce m or increase omit")
  Ns<-round(n*Ws)
  Qs<-as.vector(quantile(data,Ps))
  Cs<-frcv(data,Ps)
  aux<-function(r) sum(Cs[r:(m+1)]*Ws[r:(m+1)])/sum(Ws[r:(m+1)])
  if (!is.na(evi)) CXs<-rep(1/sqrt(1-2*evi),m+1) else CXs<-apply(matrix(1:(m+1),ncol=1),1,FUN=aux)
  XIs<-evicv(CXs)
  aux<-function(r) n*sum(Ws[r:(m+1)]*(Cs[r:(m+1)]-CXs[r])^2)/(m+1-r)
  TMSs<-apply(matrix(1:(m+1),ncol=1),1,FUN=aux)
  aux<-function(r) Tms.pvalue(n=Ns[r],tms=TMSs[r],m=m-r+1,evi=XIs[r],hat=is.na(evi),omit,nsim)$pvalue
  PVs<-apply(matrix(1:(m),ncol=1),1,FUN=aux)
  PVs<-c(PVs,1)
  #####################################################
  #### output
  #####################################################
  options<-data.frame(m=seq(m,0,-1),nextremes=Ns,threshold=Qs+threshold,rcv=Cs,cvopt=CXs,evi=XIs,tms=TMSs,pvalue=PVs)[1:m,]
  solution<-options[options$pvalue>(1-conf.level),][1,]
  outlist<-list(solution=solution,options=options)
  if (oprint==T) print(solution)
  thrselect<-outlist
}