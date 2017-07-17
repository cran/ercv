tdata<-function(data, threshold = NA, nextremes = NA,sigma=NA)
{
  #####################################################
  #### controls and inizialization of vars
  #####################################################
  data <- as.numeric(data)
  data<-data[!is.na(data)]
  data <- sort(data)
  if (is.na(nextremes) && is.na(threshold)) threshold<-min(data)
  if (!is.na(nextremes) && !is.na(threshold)) stop("Enter EITHER a threshold or the number of upper extremes")
  if (!is.na(nextremes)) threshold<-rev(data)[as.numeric(nextremes)]
  if (max(data)<threshold) stop("There are not data over threshold")
  data<- data[data >=threshold]- threshold
  #####################################################
  #### inside
  #####################################################
  if (is.na(sigma)) {aux<-egpd(data,heavy=T);sigma<-aux$psi/aux$evi}
  print(paste("sigma = ",sigma,sep=""))
  if (sigma>0) data<- (sigma*data)/(sigma+data)
  else data<--1/(data[data>0])
  #####################################################
  #### output
  #####################################################
  data<-data-min(data)
}
