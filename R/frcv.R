frcv<-function(data,Ps)
{
  Qs<-as.vector(quantile(data,Ps))
  Cs<-c();for(k in 1:length(Qs)) Cs<-c(Cs,sd(data[data>=Qs[k]]-Qs[k])/mean(data[data>=Qs[k]]-Qs[k]));Cs
}
