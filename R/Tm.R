Tm<-function(data, threshold = NA, nextremes = NA, omit = 16, evi = NA, m = 10, nsim = 100)
{
  #####################################################
  #### controls and inizialization of vars
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
  data<- data[data >= threshold]- threshold
  n <- length(data)
  #####################################################
  #### inside
  #####################################################
  if (!is.na(evi))
  {
    if (length(evi)>1) stop("Enter only one value or NA-value for extreme value index (evi) parameter")
    if (evi>=1/2) stop("The hypotesis testing needs evi<1/2",call. = TRUE)
  }
  p<-round(exp(log(omit/n)/m),digits=2)
  Ws<-p^(0:m)
  Ps<-1-Ws
  Cs<-frcv(data,Ps)
  if (is.na(evi)) cx<-(1-p)*sum(Ws*Cs)/(1-p^(m+1)) else cx=1/sqrt(1-2*evi)
  xi<-(cx^2-1)/(2*cx^2)
  tm<-n*sum(Ws*(Cs-cx)^2)
  #####################################################
  #### output
  #####################################################
  Tms.pvalue(n=n,tms=tm/(m+1),m=m,evi=xi,hat=is.na(evi),omit=omit,nsim=nsim)
}